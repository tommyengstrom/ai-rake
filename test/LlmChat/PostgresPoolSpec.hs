module LlmChat.PostgresPoolSpec where

import Control.Exception (try)
import Effectful
import Effectful.Error.Static
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL.Connection.Pool (runWithConnectionPool)
import Effectful.Time
import LlmChat.Error (LlmChatError)
import LlmChat.PostgresSpec
    ( dropTablesIfExists
    , makePool
    , newConversationsTable
    , postgresStorageBehaviourSpec
    , withConnection
    )
import LlmChat.Storage.Effect
import LlmChat.Storage.Postgres
import LlmChat.Types
import Relude
import Test.Hspec
import UnliftIO (forConcurrently)
import UnliftIO.Pool (destroyAllResources)

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
                            $ setupConversationTables conversationsTable

                    afterAll_ (cleanup *> destroyAllResources pool) do
                        postgresStorageBehaviourSpec pool conversationsTable

                describe "Connection pool behavior" $ do
                    it "handles concurrent operations efficiently" $ do
                        pool <- makePool connectionString 2 5
                        cleanup
                        runEff
                            . runWithConnectionPool pool
                            $ setupConversationTables conversationsTable

                        let runStack :: Eff '[LlmChatStorage, WithConnection, Error ChatStorageError, Error LlmChatError, Time, IOE] Int -> IO (Either LlmChatError (Either ChatStorageError Int))
                            runStack =
                                runEff
                                    . runTime
                                    . runErrorNoCallStack @LlmChatError
                                    . runErrorNoCallStack @ChatStorageError
                                    . runWithConnectionPool pool
                                    . runLlmChatStoragePostgres conversationsTable

                        results <- forConcurrently ([1 .. 4] :: [Int]) $ \i ->
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
