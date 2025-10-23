{-# OPTIONS_GHC -Wno-orphans #-}

module LlmChat.Storage.Postgres where

import LlmChat.Storage.Effect
import LlmChat.Types
import Data.Aeson (Value, decode, encode, toJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Relude

-- | Convenience alias for the table storing conversations.
type ConversationsTable = Text

instance ToField ConversationId where
    toField (ConversationId uuid) = toField uuid

instance FromField ConversationId where
    fromField f mdata = ConversationId <$> fromField f mdata

instance ToField ChatMsg where
    toField msg = toField (toJSON msg)

instance FromField ChatMsg where
    fromField f mdata = do
        (value :: Value) <- fromField f mdata
        case decode (encode value) of
            Just msg -> pure msg
            Nothing -> returnError ConversionFailed f "Could not decode ChatMsg from JSON"

data MessageRow = MessageRow
    { messageId :: UUID
    , conversationId :: ConversationId
    , message :: ChatMsg
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

instance FromRow MessageRow where
    fromRow = MessageRow <$> PG.field <*> PG.field <*> PG.field <*> PG.field

runLlmChatStoragePostgres
    :: forall es a
     . ( IOE :> es
       , Error ChatStorageError :> es
       , WithConnection :> es
       )
    => ConversationsTable
    -> Eff (LlmChatStorage ': es) a
    -> Eff es a
runLlmChatStoragePostgres tableName = interpret $ \_ -> \case
    CreateConversation systemPrompt -> do
        conversationId <- ConversationId <$> liftIO nextRandom
        void $ insertMessage tableName conversationId (SystemMsg systemPrompt)
        pure conversationId
    DeleteConversation conversationId -> do
        void $ PG.execute (deleteConversationQuery tableName) (Only conversationId)
    GetStoredConversation conversationId -> do
        rows <- PG.query (selectMessagesQuery tableName) (Only conversationId)
        case (rows :: [MessageRow]) of
            [] -> throwError $ NoSuchConversation conversationId
            _ -> pure $ map messageRowToStored rows
      where
        messageRowToStored MessageRow{message = rowMessage, messageId = rowMessageId, createdAt = rowCreatedAt} =
            StoredMsg
                { msg = rowMessage
                , msgId = rowMessageId
                , createdAt = rowCreatedAt
                }
    AppendMessage conversationId msgIn -> do
        insertMessage tableName conversationId msgIn >>= \case
            [s] -> pure s
            [] -> throwError $ InsertFailure "insert operation returned no rows"
            _ss -> throwError $ InsertFailure "insert operation returned multiple rows"

    ListConversations -> do
        rows <- PG.query_ (listConversationsQuery tableName)
        pure $ map (\(Only cid) -> cid) rows

insertMessage
    :: ( WithConnection :> es
       , IOE :> es
       )
    => ConversationsTable
    -> ConversationId
    -> ChatMsg
    -> Eff es [StoredMsg]
insertMessage tableName conversationId msg = do
    r <-
        PG.returning
            (insertMessageQuery tableName)
            [(conversationId, msg)]
    pure $ (\(a,b,c) -> StoredMsg a b c) <$> r


setupTable
    :: ( IOE :> es
       , WithConnection :> es
       )
    => ConversationsTable
    -> Eff es ()
setupTable tableName = PG.withTransaction do
    void $ PG.execute_ (createTableQuery tableName)
    void $ PG.execute_ (createIndexQuery tableName)

createTableQuery :: ConversationsTable -> Query
createTableQuery tableName =
    fromString
        $ toString
        $ "CREATE TABLE IF NOT EXISTS "
        <> tableName
            <> " ("
            <> "message_id UUID PRIMARY KEY DEFAULT gen_random_uuid(), "
            <> "conversation_id UUID NOT NULL, "
            <> "message JSONB NOT NULL, "
            <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
            <> ")"

insertMessageQuery :: ConversationsTable -> Query
insertMessageQuery tableName =
    fromString
        $ toString
        $ "INSERT INTO "
        <> tableName
            <> " (conversation_id, message) VALUES (?, ?) "
        <> "RETURNING message, message_id, created_at"

deleteConversationQuery :: ConversationsTable -> Query
deleteConversationQuery tableName =
    fromString
        $ toString
        $ "DELETE FROM "
        <> tableName
            <> " WHERE conversation_id = ?"

selectMessagesQuery :: ConversationsTable -> Query
selectMessagesQuery tableName =
    fromString
        $ toString
        $ "SELECT message_id, conversation_id, message, created_at FROM "
        <> tableName
            <> " WHERE conversation_id = ? "
            <> " ORDER BY created_at ASC"

listConversationsQuery :: ConversationsTable -> Query
listConversationsQuery tableName =
    fromString
        $ toString
        $ "SELECT DISTINCT conversation_id FROM "
        <> tableName

createIndexQuery :: ConversationsTable -> Query
createIndexQuery tableName =
    fromString
        $ toString
        $ "CREATE INDEX IF NOT EXISTS idx_"
        <> tableName
            <> "_conversation_id ON "
            <> tableName
            <> " (conversation_id)"
