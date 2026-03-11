module LlmChat.PostgresPoolSpec where

import Control.Exception (bracket, try)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Effectful
import Effectful.Error.Static
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL.Connection.Pool (runWithConnectionPool)
import Effectful.Time
import LlmChat.Error (LlmChatError)
import LlmChat.PostgresSpec (postgresStorageBehaviourSpec)
import LlmChat.Storage.Effect
import LlmChat.Storage.Postgres
import LlmChat.Types
import Relude
import Test.Hspec
import UnliftIO (forConcurrently)
import UnliftIO.Pool (Pool, destroyAllResources, mkDefaultPoolConfig, newPool, setNumStripes)

spec :: Spec
spec = do
    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    postgresAvailable <- runIO (isRight <$> try @SomeException (withConnection connectionString (\_ -> pure ())))

    if not postgresAvailable
        then
            describe "PostgreSQL Connection Pooling" $
                it "requires a local PostgreSQL test database" $
                    pendingWith "PostgreSQL test database is unavailable."
        else
            describe "PostgreSQL Connection Pooling" $ do
                conversationsTable <- runIO newConversationsTable

                let cleanup = dropTablesIfExists connectionString conversationsTable

                describe "runLlmChatStoragePostgres with a pool" $ do
                    pool <- runIO $ makePool connectionString 2 5

                    runIO do
                        cleanup
                        runEff
                            . runWithConnectionPool pool
                            $ setupTable conversationsTable

                    afterAll_ (cleanup *> destroyAllResources pool) do
                        postgresStorageBehaviourSpec pool conversationsTable

                describe "Connection pool behavior" $ do
                    it "handles concurrent operations efficiently" $ do
                        pool <- makePool connectionString 2 5
                        cleanup
                        runEff
                            . runWithConnectionPool pool
                            $ setupTable conversationsTable

                        let runStack :: Eff '[LlmChatStorage, WithConnection, Error ChatStorageError, Error LlmChatError, Time, IOE] Int -> IO (Either LlmChatError (Either ChatStorageError Int))
                            runStack =
                                runEff
                                    . runTime
                                    . runErrorNoCallStack @LlmChatError
                                    . runErrorNoCallStack @ChatStorageError
                                    . runWithConnectionPool pool
                                    . runLlmChatStoragePostgres conversationsTable

                        results <- forConcurrently ([1 .. 20] :: [Int]) $ \i ->
                            runStack do
                                convId <- createConversation
                                appendItems
                                    convId
                                    [ system ("Test system prompt " <> show i)
                                    , user ("User message " <> show i)
                                    , assistantText ("Assistant response " <> show i)
                                    ]
                                msgs <- getConversation convId
                                pure (length msgs)

                        forM_ results $ \case
                            Left err ->
                                expectationFailure ("Concurrent operation failed: " <> show err)
                            Right (Left err) ->
                                expectationFailure ("Concurrent operation failed: " <> show err)
                            Right (Right msgCount) ->
                                msgCount `shouldBe` 3

                        destroyAllResources pool
                        cleanup

newConversationsTable :: IO ConversationsTable
newConversationsTable = do
    now <- getCurrentTime
    let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
    pure $ "conversations_pool_" <> toText unixTime

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
