module LlmChat.PostgresLoggerSpec where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, try)
import Data.Aeson (Value, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (execute_)
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL.Connection.Pool (runWithConnectionPool)
import LlmChat.PostgresLogger
    ( JsonField (..)
    , LogEntry (..)
    , PgIdentifier
    , getResponseLogs
    , getResponseLogsByTimeRange
    , insertResponseLog
    , mkPgIdentifier
    , pgIdentifierText
    , setupResponseLogTable
    )
import LlmChat.PostgresSpec (makePool, withConnection)
import LlmChat.Types (ConversationId (..))
import Relude
import Test.Hspec
import UnliftIO.Pool (destroyAllResources)

testResponse :: Value
testResponse =
    object
        [ "id" Aeson..= ("test-id" :: Text)
        , "object" Aeson..= ("response" :: Text)
        , "model" Aeson..= ("gpt-4.1-mini" :: Text)
        , "output" Aeson..= ([] :: [Value])
        ]

spec :: Spec
spec = do
    let connectionString = "host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test"
    postgresAvailable <- runIO (isRight <$> try @SomeException (withConnection connectionString (\_ -> pure ())))

    if not postgresAvailable
        then
            describe "PostgresLogger" $
                it "requires a local PostgreSQL test database" $
                    pendingWith "PostgreSQL test database is unavailable."
        else
            describe "PostgresLogger" $ do
                pool <- runIO $ makePool connectionString 1 5

                let runDb :: Eff '[WithConnection, IOE] a -> IO a
                    runDb =
                        runEff
                            . runWithConnectionPool pool

                let withLogTable action = do
                        tableName <- uniqueTableName
                        let cleanup = dropTableIfExists connectionString tableName
                        cleanup
                        runDb (setupResponseLogTable tableName)
                        action tableName `finally` cleanup

                afterAll_ (destroyAllResources pool) do
                    it "logs JSON responses to PostgreSQL as JSONB" $
                        withLogTable $ \tableName -> do
                            testConvId <- ConversationId <$> nextRandom
                            runDb $ insertResponseLog tableName testConvId testResponse

                            logs <- runDb (getResponseLogs tableName :: Eff '[WithConnection, IOE] [LogEntry Value])
                            length logs `shouldBe` 1

                            case logs of
                                [LogEntry{conversationId = actualConversationId, response = JsonField actualResponse, createdAt = logTime}] -> do
                                    toJSON actualResponse `shouldBe` testResponse
                                    actualConversationId `shouldBe` testConvId
                                    now <- getCurrentTime
                                    diffUTCTime now logTime `shouldSatisfy` (< 10)
                                _ ->
                                    expectationFailure "Expected exactly one log entry"

                    it "returns log entries in reverse chronological order" $
                        withLogTable $ \tableName -> do
                            firstConversationId <- ConversationId <$> nextRandom
                            secondConversationId <- ConversationId <$> nextRandom

                            runDb $ insertResponseLog tableName firstConversationId testResponse
                            threadDelay 50000
                            runDb $ insertResponseLog tableName secondConversationId testResponse

                            logs <- runDb (getResponseLogs tableName :: Eff '[WithConnection, IOE] [LogEntry Value])
                            let conversationIds =
                                    [ actualConversationId
                                    | LogEntry{conversationId = actualConversationId} <- logs
                                    ]

                            conversationIds `shouldBe` [secondConversationId, firstConversationId]

                    it "filters log entries by time range" $
                        withLogTable $ \tableName -> do
                            firstConversationId <- ConversationId <$> nextRandom
                            secondConversationId <- ConversationId <$> nextRandom

                            runDb $ insertResponseLog tableName firstConversationId testResponse
                            threadDelay 50000
                            startTime <- getCurrentTime
                            threadDelay 50000
                            runDb $ insertResponseLog tableName secondConversationId testResponse
                            endTime <- getCurrentTime

                            logs <-
                                runDb
                                    ( getResponseLogsByTimeRange tableName startTime endTime
                                        :: Eff '[WithConnection, IOE] [LogEntry Value]
                                    )
                            let conversationIds =
                                    [ actualConversationId
                                    | LogEntry{conversationId = actualConversationId} <- logs
                                    ]

                            conversationIds `shouldBe` [secondConversationId]

uniqueTableName :: IO PgIdentifier
uniqueTableName = do
    now <- getCurrentTime
    uuid <- nextRandom
    let randomId = T.replace "-" "_" (toText (show (uuid :: UUID) :: String))
        unixTime :: String = show (floor (utcTimeToPOSIXSeconds now) :: Integer)
        tableName = "response_logs_" <> toText unixTime <> "_" <> randomId
    either (fail . toString) pure (mkPgIdentifier tableName)

dropTableIfExists :: ByteString -> PgIdentifier -> IO ()
dropTableIfExists connStr tableName =
    withConnection connStr $ \conn ->
        void . execute_ conn . fromString . toString $
            "DROP TABLE IF EXISTS " <> quoteIdentifier tableName

quoteIdentifier :: PgIdentifier -> Text
quoteIdentifier identifier =
    "\"" <> pgIdentifierText identifier <> "\""
