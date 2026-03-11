module LlmChat.PostgresLoggerSpec where

import Control.Lens
import Control.Exception (bracket, try)
import Data.Aeson (Value, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.Generics.Labels ()
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import LlmChat.PostgresLogger
    ( JsonField (..)
    , createTableQuery
    , getAllLogs
    , postgresResponseLogger
    )
import LlmChat.Types (ConversationId (..))
import Relude
import Test.Hspec

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
                let getConnection = connectPostgreSQL connectionString

                describe "logs JSON responses to PostgreSQL as JSONB" $ do
                    tableName <- runIO uniqueTableName

                    let setupTable = do
                            conn <- getConnection
                            _ <- execute_ conn $ createTableQuery tableName
                            close conn

                    let cleanup = do
                            conn <- getConnection
                            _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> tableName
                            close conn

                    runIO do
                        cleanup
                        setupTable
                    afterAll_ cleanup $ do
                        it "works correctly" $ do
                            testConvId <- ConversationId <$> nextRandom
                            postgresResponseLogger tableName getConnection testConvId testResponse

                            logs <- getAllLogs @Value tableName getConnection
                            length logs `shouldBe` 1

                            case viaNonEmpty head logs of
                                Nothing ->
                                    expectationFailure "Expected at least one log entry"
                                Just logEntry -> do
                                    case logEntry ^. #response of
                                        JsonField actualResponse ->
                                            toJSON actualResponse `shouldBe` testResponse
                                    (logEntry ^. #conversationId) `shouldBe` testConvId
                                    now <- getCurrentTime
                                    let logTime = logEntry ^. #createdAt
                                    diffUTCTime now logTime `shouldSatisfy` (< 10)

                describe "can log multiple entries" $ do
                    tableName <- runIO uniqueTableName

                    let setupTable = do
                            conn <- getConnection
                            _ <- execute_ conn $ createTableQuery tableName
                            close conn

                    let cleanup = do
                            conn <- getConnection
                            _ <- execute_ conn $ fromString $ toString $ "DROP TABLE IF EXISTS " <> tableName
                            close conn

                    runIO do
                        cleanup
                        setupTable
                    afterAll_ cleanup $ do
                        it "works correctly" $ do
                            testConvId1 <- ConversationId <$> nextRandom
                            testConvId2 <- ConversationId <$> nextRandom
                            postgresResponseLogger tableName getConnection testConvId1 testResponse
                            postgresResponseLogger tableName getConnection testConvId2 testResponse

                            logs <- getAllLogs @Value tableName getConnection
                            length logs `shouldBe` 2

uniqueTableName :: IO Text
uniqueTableName = do
    now <- getCurrentTime
    uuid <- nextRandom
    let randomId = toText (show (uuid :: UUID) :: String)
    let unixTime :: String = show $ (floor $ utcTimeToPOSIXSeconds now :: Integer)
    pure $ "response_logs_" <> toText unixTime <> "_" <> T.replace "-" "_" randomId

withConnection :: ByteString -> (Connection -> IO a) -> IO a
withConnection connStr action = bracket (connectPostgreSQL connStr) close action
