module Rake.PostgresSpec where

import Control.Exception (bracket, try)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL.Connection.Pool (runWithConnectionPool)
import Effectful.Time
import Rake.Storage.Effect
import Rake.Storage.InMemorySpec (SomeText (..))
import Rake.Storage.Postgres
import Rake.Types
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import UnliftIO (forConcurrently)
import UnliftIO.Pool (Pool, destroyAllResources, mkDefaultPoolConfig, newPool, setNumStripes)

spec :: Spec
spec = do
    describe "PostgreSQL identifiers" $ do
        it "accepts valid identifiers" $ do
            fmap pgIdentifierText (mkPgIdentifier "valid_identifier_123")
                `shouldBe` Right "valid_identifier_123"

        it "rejects empty, malformed, and uppercase identifiers" $ do
            mkPgIdentifier "" `shouldSatisfy` isLeft
            mkPgIdentifier "9invalid" `shouldSatisfy` isLeft
            mkPgIdentifier "invalid-name" `shouldSatisfy` isLeft
            mkPgIdentifier "Invalid" `shouldSatisfy` isLeft

        it "rejects prefixes whose derived table names exceed PostgreSQL limits" $
            case mkPgIdentifier (T.replicate 50 "a") of
                Left err ->
                    expectationFailure (toString err)
                Right prefix ->
                    conversationTablesFromPrefix prefix `shouldSatisfy` isLeft

    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    postgresAvailable <- runIO (isRight <$> try @SomeException (withConnection connectionString (\_ -> pure ())))

    if not postgresAvailable
        then
            describe "runRakeStoragePostgres" $
                it "requires a local PostgreSQL test database" $
                    pendingWith "PostgreSQL test database is unavailable."
        else
            describe "runRakeStoragePostgres" $ do
                conversationsTable <- runIO newConversationsTable
                pool <- runIO $ makePool connectionString 2 5

                let cleanup =
                        dropTablesIfExists connectionString conversationsTable

                runIO do
                    cleanup
                    runEff
                        . runWithConnectionPool pool
                        $ setupConversationTables conversationsTable

                afterAll_ (cleanup *> destroyAllResources pool) do
                    postgresStorageBehaviourSpec pool conversationsTable

postgresStorageBehaviourSpec :: Pool Connection -> ConversationTables -> Spec
postgresStorageBehaviourSpec pool conversationsTable =
    describe "RakeStorage" do
        let limit = modifyMaxSuccess (const 20)
        let runStack :: Eff '[RakeStorage, WithConnection, Error ChatStorageError, Time, Concurrent, IOE] a -> IO (Either ChatStorageError a)
            runStack =
                runEff
                    . runConcurrent
                    . runTime
                    . runErrorNoCallStack
                    . runWithConnectionPool pool
                    . runRakeStoragePostgres conversationsTable

        limit $ it "Fails to fetch conversation that does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <- run $ runStack $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        limit $ it "Creates an empty conversation" $ do
            monadicIO $ do
                Right conv <- run $ runStack $ do
                    convId <- createConversation
                    getConversation convId
                liftIO $ conv `shouldBe` []

        limit $ it "Lists new conversation after creating one" $ do
            property $ monadicIO $ do
                Right (listBefore, convId, listAfter) <-
                    run
                        $ runStack
                        $ do
                            listBefore <- listConversations
                            convId <- createConversation
                            (listBefore, convId,) <$> listConversations
                liftIO $
                    Set.difference (Set.fromList listAfter) (Set.fromList listBefore)
                        `shouldBe` [convId]

        limit $ it "AppendItem adds items to the end of the conversation" $ do
            property $ \(SomeText systemPrompt) (SomeText userPrompt1) (SomeText userPrompt2) -> monadicIO do
                Right (beforeAppend, afterAppend) <-
                    run
                        $ runStack
                        $ do
                            convId <- createConversation
                            conv <- getConversation convId
                            appendItems convId [system systemPrompt, user userPrompt1, user userPrompt2]
                            (conv,) <$> getConversation convId
                liftIO $ length beforeAppend + 3 `shouldBe` length afterAppend
                liftIO $
                    take 2 (reverseUserTexts afterAppend)
                        `shouldBe` [userPrompt2, userPrompt1]

        limit $ it "AppendItem errors if conversation does not exist" $ do
            property $ \(convId :: ConversationId) (SomeText prompt) -> monadicIO $ do
                result <-
                    run
                        $ runStack
                        $ appendItem convId (user prompt)
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        limit $ it "ModifyConversationAtomic appends based on the current history" $ do
            property $ \(SomeText firstPrompt) (SomeText secondPrompt) -> monadicIO $ do
                result <- run $ runStack $ do
                    convId <- createConversation
                    appendItem convId (user firstPrompt)
                    observedLength <-
                        modifyConversationAtomic convId \history ->
                            (length history, [user secondPrompt])
                    finalHistory <- getConversation convId
                    pure (observedLength, finalHistory)

                liftIO $
                    result `shouldBe`
                        Right
                            ( 1
                            , [user firstPrompt, user secondPrompt]
                            )

        limit $ it "ModifyConversationAtomic errors if conversation does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        $ runStack
                        $ modifyConversationAtomic convId (\history -> (length history, [user "ignored"]))
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        it "ModifyConversationAtomic serializes concurrent writers" $ do
            let workerCount = 4

            Right convId <- runStack createConversation
            threadResults <-
                forConcurrently ([1 .. workerCount] :: [Int]) $ \workerIndex ->
                    runStack $
                        modifyConversationAtomic convId \history ->
                            (length history, [user ("worker-" <> show workerIndex)])

            Right finalHistory <- runStack (getConversation convId)

            observedLengths <- forM threadResults $ \case
                Left err ->
                    expectationFailure ("Concurrent PostgreSQL storage worker failed: " <> show err)
                        >> pure (-1)
                Right observedLength ->
                    pure observedLength

            sort observedLengths `shouldBe` [0 .. workerCount - 1]
            length finalHistory `shouldBe` workerCount

        limit $ it "DeleteConversation removes the conversation" $ do
            property $ monadicIO $ do
                Right (convId, listAfterDelete, fetchResult) <- run
                    $ runStack do
                        convId <- createConversation
                        appendItem convId (user "hello")
                        deleteConversation convId
                        listAfterDelete <- listConversations
                        fetchResult <-
                            ( getConversation convId >>= \conversation -> pure (Right conversation)
                            )
                                `catchError` (\_ err -> pure (Left err :: Either ChatStorageError [HistoryItem]))
                        pure (convId, listAfterDelete, fetchResult)

                liftIO $ convId `shouldNotSatisfy` (`elem` listAfterDelete)
                liftIO $ fetchResult `shouldBe` Left (NoSuchConversation convId)
  where
    reverseUserTexts history =
        [ text
        | HLocal LocalMessage{role = GenericUser, parts = [PartText{text}]} <- reverse history
        ]

newConversationsTable :: IO ConversationTables
newConversationsTable = do
    now <- getCurrentTime
    uuid <- nextRandom
    let unixTime :: String = show (floor (utcTimeToPOSIXSeconds now) :: Integer)
        uuidSuffix =
            T.take 8 $
                T.replace "-" "_" $
                    toText (show (uuid :: UUID) :: String)
        prefixText = "conv_" <> toText unixTime <> "_" <> uuidSuffix
    prefix <- either (fail . toString) pure (mkPgIdentifier prefixText)
    either (fail . toString) pure (conversationTablesFromPrefix prefix)

makePool :: ByteString -> Int -> Int -> IO (Pool Connection)
makePool connStr stripes maxOpen = do
    config <- mkDefaultPoolConfig (connectPostgreSQL connStr) close 60 maxOpen
    newPool $ setNumStripes (Just stripes) config

withConnection :: ByteString -> (Connection -> IO a) -> IO a
withConnection connStr action = bracket (connectPostgreSQL connStr) close action

dropTablesIfExists :: ByteString -> ConversationTables -> IO ()
dropTablesIfExists connStr ConversationTables{conversationTable, itemsTable} =
    withConnection connStr $ \conn -> do
        void . execute_ conn . fromString . toString $
            "DROP TABLE IF EXISTS " <> quoteIdentifier itemsTable
        void . execute_ conn . fromString . toString $
            "DROP TABLE IF EXISTS " <> quoteIdentifier conversationTable

quoteIdentifier :: PgIdentifier -> Text
quoteIdentifier identifier =
    "\"" <> pgIdentifierText identifier <> "\""
