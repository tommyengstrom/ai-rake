module LlmChat.PostgresSpec where

import LlmChat.Storage.Effect
import LlmChat.Storage.InMemorySpec (SomeText (..))
import LlmChat.Storage.Postgres
import LlmChat.Types
import Control.Lens (folded, reversed, taking, (^..))
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
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import UnliftIO.Pool (Pool, destroyAllResources, mkDefaultPoolConfig, newPool, setNumStripes)

spec :: Spec
spec =
    describe "runLlmChatStoragePostgres" $ do
        conversationsTable <- runIO newConversationsTable

        let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
        pool <- runIO $ makePool connectionString 1 5

        let cleanup = do
                conn <- connectPostgreSQL connectionString
                dropTableIfExists conn conversationsTable
                close conn

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
                result <-
                    run
                        $ runStack
                        $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        limit $ it "Can retrieve conversation after creating it" $ do
            property $ \(SomeText systemPrompt) -> monadicIO $ do
                Right conv <- run $ runStack $ do
                    convId <- createConversation systemPrompt
                    getConversation convId
                liftIO do
                    length conv `shouldBe` 1
                    conv
                        ^.. folded . _Ctor @"SystemMsg" . typed @Text
                        `shouldBe` [systemPrompt]

        limit $ it "Lists new conversation after creating one" $ do
            property $ \(SomeText systemPrompt) -> monadicIO $ do
                Right (listBefore, convId, listAfter) <-
                    run
                        $ runStack
                        $ do
                            listBefore <- listConversations
                            convId <- createConversation systemPrompt
                            (listBefore, convId,)
                                <$> listConversations
                liftIO $
                    Set.difference (Set.fromList listAfter) (Set.fromList listBefore)
                        `shouldBe` [convId]

        limit $ it "AppendMessage adds messages to the end of the conversation" $ do
            property $ \(SomeText systemPrompt) (SomeText userPrompt1) (SomeText userPrompt2) -> monadicIO do
                Right (beforeAppend, afterAppend) <-
                    run
                        $ runStack
                        $ do
                            convId <- createConversation systemPrompt
                            conv <- getConversation convId
                            _ <- appendUserMessage convId userPrompt1
                            _ <- appendUserMessage convId userPrompt2
                            (conv,) <$> getConversation convId
                liftIO $ length beforeAppend + 2 `shouldBe` length afterAppend
                liftIO $
                    afterAppend
                        ^.. reversed . taking 2 folded . _Ctor @"UserMsg" . typed @Text
                        `shouldBe` [userPrompt2, userPrompt1]

        limit $ it "GetConversation errors if conversation does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        $ runStack
                        $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

newConversationsTable :: IO ConversationsTable
newConversationsTable = do
    now <- getCurrentTime
    let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
    pure $ "conversations_" <> toText unixTime

makePool :: ByteString -> Int -> Int -> IO (Pool Connection)
makePool connStr stripes maxOpen = do
    config <- mkDefaultPoolConfig (connectPostgreSQL connStr) close 60 maxOpen
    newPool $ setNumStripes (Just stripes) config

dropTableIfExists :: Connection -> ConversationsTable -> IO ()
dropTableIfExists conn tableName = do
    [Only exists] <-
        query
            conn
            "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = current_schema() AND table_name = ?)"
            (Only tableName)
    when exists do
        void $ execute_ conn $ fromString $ toString $ "DROP TABLE " <> tableName
