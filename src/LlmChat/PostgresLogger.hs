{-# LANGUAGE UndecidableInstances #-}

module LlmChat.PostgresLogger
    ( PgIdentifier
    , mkPgIdentifier
    , pgIdentifierText
    , JsonField (..)
    , LogEntry (..)
    , setupResponseLogTable
    , insertResponseLog
    , getResponseLogs
    , getResponseLogsByTimeRange
    ) where

import Data.Aeson (FromJSON, Result (Error, Success), ToJSON, Value, fromJSON, toJSON)
import Data.Time
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import LlmChat.Postgres.Internal
import LlmChat.Types (ConversationId)
import Relude

newtype JsonField a = JsonField a
    deriving stock (Show, Eq, Generic)

instance ToJSON a => ToField (JsonField a) where
    toField (JsonField obj) = toField (toJSON obj)

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
    fromField field maybeData = do
        (value :: Value) <- fromField field maybeData
        case fromJSON value of
            Success obj ->
                pure (JsonField obj)
            Error err ->
                returnError ConversionFailed field ("Could not decode from JSON: " <> err)

data LogEntry a = LogEntry
    { logId :: Int64
    , conversationId :: ConversationId
    , response :: JsonField a
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

instance (FromJSON a, Typeable a) => FromRow (LogEntry a) where
    fromRow = LogEntry <$> PG.field <*> PG.field <*> PG.field <*> PG.field

setupResponseLogTable
    :: ( IOE :> es
       , WithConnection :> es
       )
    => PgIdentifier
    -> Eff es ()
setupResponseLogTable tableName =
    withPinnedConnection $
        PG.withTransaction $
            void $ PG.execute_ (createTableQuery tableName)

insertResponseLog
    :: ( IOE :> es
       , WithConnection :> es
       , ToJSON a
       )
    => PgIdentifier
    -> ConversationId
    -> a
    -> Eff es ()
insertResponseLog tableName conversationId responseValue = do
    createdAt <- liftIO getCurrentTime
    withPinnedConnection $
        PG.withTransaction $
            void $
                PG.execute
                    (insertQuery tableName)
                    (conversationId, JsonField responseValue, createdAt)

getResponseLogs
    :: ( IOE :> es
       , WithConnection :> es
       , FromJSON a
       , Typeable a
       )
    => PgIdentifier
    -> Eff es [LogEntry a]
getResponseLogs tableName =
    withPinnedConnection $
        PG.withTransaction $
            PG.query_ (selectAllQuery tableName)

getResponseLogsByTimeRange
    :: ( IOE :> es
       , WithConnection :> es
       , FromJSON a
       , Typeable a
       )
    => PgIdentifier
    -> UTCTime
    -> UTCTime
    -> Eff es [LogEntry a]
getResponseLogsByTimeRange tableName startTime endTime =
    withPinnedConnection $
        PG.withTransaction $
            PG.query
                (selectByTimeRangeQuery tableName)
                (startTime, endTime)

withPinnedConnection
    :: forall es a
     . ( WithConnection :> es
       )
    => Eff (WithConnection ': es) a
    -> Eff es a
withPinnedConnection action =
    PG.withConnection $ \connection ->
        PG.runWithConnection connection action

createTableQuery :: PgIdentifier -> Query
createTableQuery tableName =
    toQueryText $
        "CREATE TABLE IF NOT EXISTS "
            <> renderIdentifier tableName
            <> " ("
            <> "id BIGSERIAL PRIMARY KEY, "
            <> "conversation_id UUID NOT NULL, "
            <> "response JSONB NOT NULL, "
            <> "created_at TIMESTAMP WITH TIME ZONE NOT NULL"
            <> ")"

insertQuery :: PgIdentifier -> Query
insertQuery tableName =
    toQueryText $
        "INSERT INTO "
            <> renderIdentifier tableName
            <> " (conversation_id, response, created_at) VALUES (?, ?, ?)"

selectAllQuery :: PgIdentifier -> Query
selectAllQuery tableName =
    toQueryText $
        "SELECT id, conversation_id, response, created_at FROM "
            <> renderIdentifier tableName
            <> " ORDER BY created_at DESC, id DESC"

selectByTimeRangeQuery :: PgIdentifier -> Query
selectByTimeRangeQuery tableName =
    toQueryText $
        "SELECT id, conversation_id, response, created_at FROM "
            <> renderIdentifier tableName
            <> " WHERE created_at >= ? AND created_at <= ? ORDER BY created_at DESC, id DESC"

renderIdentifier :: PgIdentifier -> Text
renderIdentifier = quotePgIdentifier

toQueryText :: Text -> Query
toQueryText = fromString . toString
