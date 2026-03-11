module LlmChat.PostgresSpec where

import Control.Lens (folded, reversed, taking, (^..))
import Control.Exception (bracket, try)
import Data.Generics.Product
import Data.Generics.Sum
import Data.Set qualified as Set
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL.Connection.Pool (runWithConnectionPool)
import Effectful.Time
import LlmChat.Storage.Effect
import LlmChat.Storage.InMemorySpec (SomeText (..))
import LlmChat.Storage.Postgres
import LlmChat.Types
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import UnliftIO.Pool (Pool, destroyAllResources, mkDefaultPoolConfig, newPool, setNumStripes)

spec :: Spec
spec = do
    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    postgresAvailable <- runIO (isRight <$> try @SomeException (withConnection connectionString (\_ -> pure ())))

    if not postgresAvailable
        then
            describe "runLlmChatStoragePostgres" $
                it "requires a local PostgreSQL test database" $
                    pendingWith "PostgreSQL test database is unavailable."
        else
            describe "runLlmChatStoragePostgres" $ do
                conversationsTable <- runIO newConversationsTable
                pool <- runIO $ makePool connectionString 1 5

                let cleanup = do
                        dropTablesIfExists connectionString conversationsTable

                runIO do
                    cleanup
                    runEff
                        . runWithConnectionPool pool
                        $ setupTable conversationsTable

                afterAll_ (cleanup *> destroyAllResources pool) do
                    postgresStorageBehaviourSpec pool conversationsTable

postgresStorageBehaviourSpec :: Pool Connection -> ConversationsTable -> Spec
postgresStorageBehaviourSpec pool conversationsTable =
    describe "LlmChatStorage" do
        let limit = modifyMaxSuccess (const 20)
        let runStack :: Eff '[LlmChatStorage, WithConnection, Error ChatStorageError, Time, Concurrent, IOE] a -> IO (Either ChatStorageError a)
            runStack =
                runEff
                    . runConcurrent
                    . runTime
                    . runErrorNoCallStack
                    . runWithConnectionPool pool
                    . runLlmChatStoragePostgres conversationsTable

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
                    afterAppend
                        ^.. reversed . taking 2 folded . _Ctor @"HLocal" . _Ctor @"LocalUser" . typed @Text
                        `shouldBe` [userPrompt2, userPrompt1]

        limit $ it "AppendItem errors if conversation does not exist" $ do
            property $ \(convId :: ConversationId) (SomeText prompt) -> monadicIO $ do
                result <-
                    run
                        $ runStack
                        $ appendItem convId (user prompt)
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

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

newConversationsTable :: IO ConversationsTable
newConversationsTable = do
    now <- getCurrentTime
    let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
    pure $ "conversations_" <> toText unixTime

makePool :: ByteString -> Int -> Int -> IO (Pool Connection)
makePool connStr stripes maxOpen = do
    config <- mkDefaultPoolConfig (connectPostgreSQL connStr) close 60 maxOpen
    newPool $ setNumStripes (Just stripes) config

withConnection :: ByteString -> (Connection -> IO a) -> IO a
withConnection connStr action = bracket (connectPostgreSQL connStr) close action

dropTablesIfExists :: ByteString -> ConversationsTable -> IO ()
dropTablesIfExists connStr tableName =
    withConnection connStr $ \conn -> do
        void . execute_ conn . fromString . toString $
            "DROP TABLE IF EXISTS " <> tableName <> "_items"
        void . execute_ conn . fromString . toString $
            "DROP TABLE IF EXISTS " <> tableName <> "_conversations"
