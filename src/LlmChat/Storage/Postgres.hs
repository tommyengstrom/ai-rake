{-# OPTIONS_GHC -Wno-orphans #-}

module LlmChat.Storage.Postgres where

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
import LlmChat.Storage.Effect
import LlmChat.Types
import Relude

type ConversationsTable = Text

instance ToField ConversationId where
    toField (ConversationId uuid) = toField uuid

instance FromField ConversationId where
    fromField f mdata = ConversationId <$> fromField f mdata

instance ToField HistoryItem where
    toField historyItem = toField (toJSON historyItem)

instance FromField HistoryItem where
    fromField f mdata = do
        (value :: Value) <- fromField f mdata
        case decode (encode value) of
            Just historyItem -> pure historyItem
            Nothing -> returnError ConversionFailed f "Could not decode HistoryItem from JSON"

data ItemRow = ItemRow
    { storedItemId :: UUID
    , conversationId :: ConversationId
    , historyItem :: HistoryItem
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

instance FromRow ItemRow where
    fromRow = ItemRow <$> PG.field <*> PG.field <*> PG.field <*> PG.field

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
    CreateConversation -> do
        conversationId <- ConversationId <$> liftIO nextRandom
        void $
            PG.execute
                (insertConversationQuery tableName)
                (Only conversationId)
        pure conversationId
    DeleteConversation conversationId ->
        void $
            PG.execute
                (deleteConversationQuery tableName)
                (Only conversationId)
    GetStoredConversation conversationId -> do
        exists <- conversationExists tableName conversationId
        unless exists $
            throwError $ NoSuchConversation conversationId

        rows <- PG.query (selectItemsQuery tableName) (Only conversationId)
        pure (map itemRowToStoredItem (rows :: [ItemRow]))
      where
        itemRowToStoredItem ItemRow{storedItemId = itemId, historyItem = item, createdAt} =
            StoredItem{itemId, item, createdAt}
    AppendItem conversationId historyItem -> do
        inserted <- insertItemIfConversationExists tableName conversationId historyItem
        unless inserted $
            throwError $ NoSuchConversation conversationId
    ListConversations -> do
        rows <- PG.query_ (listConversationsQuery tableName)
        pure $ map (\(Only cid) -> cid) rows

conversationExists
    :: ( WithConnection :> es
       , IOE :> es
       )
    => ConversationsTable
    -> ConversationId
    -> Eff es Bool
conversationExists tableName conversationId = do
    rows <-
        PG.query
            (selectConversationExistsQuery tableName)
            (Only conversationId)
    pure $ not (null (rows :: [Only ConversationId]))

insertItemIfConversationExists
    :: ( WithConnection :> es
       , IOE :> es
       )
    => ConversationsTable
    -> ConversationId
    -> HistoryItem
    -> Eff es Bool
insertItemIfConversationExists tableName conversationId item = do
    itemId <- liftIO nextRandom
    inserted <-
        PG.execute
            (insertItemIfExistsQuery tableName)
            (itemId, conversationId, item, conversationId)
    pure (inserted > 0)

setupTable
    :: ( IOE :> es
       , WithConnection :> es
       )
    => ConversationsTable
    -> Eff es ()
setupTable tableName = PG.withTransaction do
    void $ PG.execute_ (createConversationsTableQuery tableName)
    void $ PG.execute_ (createItemsTableQuery tableName)
    void $ PG.execute_ (createItemsIndexQuery tableName)

conversationTableName :: ConversationsTable -> Text
conversationTableName tableName = tableName <> "_conversations"

itemsTableName :: ConversationsTable -> Text
itemsTableName tableName = tableName <> "_items"

createConversationsTableQuery :: ConversationsTable -> Query
createConversationsTableQuery tableName =
    fromString
        $ toString
        $ "CREATE TABLE IF NOT EXISTS "
        <> conversationTableName tableName
        <> " ("
        <> "conversation_id UUID PRIMARY KEY, "
        <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
        <> ")"

createItemsTableQuery :: ConversationsTable -> Query
createItemsTableQuery tableName =
    fromString
        $ toString
        $ "CREATE TABLE IF NOT EXISTS "
        <> itemsTableName tableName
        <> " ("
        <> "id SERIAL PRIMARY KEY, "
        <> "item_id UUID NOT NULL UNIQUE, "
        <> "conversation_id UUID NOT NULL REFERENCES "
        <> conversationTableName tableName
        <> " (conversation_id) ON DELETE CASCADE, "
        <> "item JSONB NOT NULL, "
        <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
        <> ")"

insertConversationQuery :: ConversationsTable -> Query
insertConversationQuery tableName =
    fromString
        $ toString
        $ "INSERT INTO "
        <> conversationTableName tableName
        <> " (conversation_id) VALUES (?)"

insertItemIfExistsQuery :: ConversationsTable -> Query
insertItemIfExistsQuery tableName =
    fromString
        $ toString
        $ "INSERT INTO "
        <> itemsTableName tableName
        <> " (item_id, conversation_id, item) "
        <> "SELECT ?, ?, ? WHERE EXISTS ("
        <> "SELECT 1 FROM "
        <> conversationTableName tableName
        <> " WHERE conversation_id = ?"
        <> ")"

deleteConversationQuery :: ConversationsTable -> Query
deleteConversationQuery tableName =
    fromString
        $ toString
        $ "DELETE FROM "
        <> conversationTableName tableName
        <> " WHERE conversation_id = ?"

selectConversationExistsQuery :: ConversationsTable -> Query
selectConversationExistsQuery tableName =
    fromString
        $ toString
        $ "SELECT conversation_id FROM "
        <> conversationTableName tableName
        <> " WHERE conversation_id = ?"

selectItemsQuery :: ConversationsTable -> Query
selectItemsQuery tableName =
    fromString
        $ toString
        $ "SELECT item_id, conversation_id, item, created_at FROM "
        <> itemsTableName tableName
        <> " WHERE conversation_id = ? ORDER BY created_at ASC, id ASC"

listConversationsQuery :: ConversationsTable -> Query
listConversationsQuery tableName =
    fromString
        $ toString
        $ "SELECT conversation_id FROM "
        <> conversationTableName tableName
        <> " ORDER BY created_at ASC"

createItemsIndexQuery :: ConversationsTable -> Query
createItemsIndexQuery tableName =
    fromString
        $ toString
        $ "CREATE INDEX IF NOT EXISTS idx_"
        <> itemsTableName tableName
        <> "_conversation_id ON "
        <> itemsTableName tableName
        <> " (conversation_id)"
