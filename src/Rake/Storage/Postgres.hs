module Rake.Storage.Postgres
    ( PgIdentifier
    , mkPgIdentifier
    , pgIdentifierText
    , ConversationTables (..)
    , conversationTablesFromPrefix
    , runRakeStoragePostgres
    , setupConversationTables
    ) where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Rake.Postgres.Internal
import Rake.Storage.Effect
import Rake.Types
import Relude

data ItemRow = ItemRow
    { storedItemId :: UUID
    , conversationId :: ConversationId
    , historyItem :: HistoryItem
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

instance FromRow ItemRow where
    fromRow = ItemRow <$> PG.field <*> PG.field <*> PG.field <*> PG.field

runRakeStoragePostgres
    :: forall es a
     . ( IOE :> es
       , Error ChatStorageError :> es
       , WithConnection :> es
       )
    => ConversationTables
    -> Eff (RakeStorage ': es) a
    -> Eff es a
runRakeStoragePostgres tables = interpret $ \_ -> \case
    CreateConversation -> do
        conversationId <- ConversationId <$> liftIO nextRandom
        void $
            PG.execute
                (insertConversationQuery tables)
                (Only conversationId)
        pure conversationId
    DeleteConversation conversationId ->
        void $
            PG.execute
                (deleteConversationQuery tables)
                (Only conversationId)
    GetStoredConversation conversationId -> do
        rows <- PG.query (selectItemsQuery tables) (Only conversationId)
        if null (rows :: [ItemRow])
            then do
                exists <- conversationExists tables conversationId
                unless exists $
                    throwError (NoSuchConversation conversationId)
                pure []
            else pure (map itemRowToStoredItem rows)
    AppendConversationItems conversationId historyItems ->
        unless (null historyItems) $
            withPinnedConnection $
                PG.withTransaction do
                    lockConversation tables conversationId
                    insertRows <- mkInsertRows conversationId historyItems
                    void $
                        PG.executeMany
                            (insertItemsQuery tables)
                            insertRows
    ListConversations -> do
        rows <- PG.query_ (listConversationsQuery tables)
        pure (map (\(Only conversationId) -> conversationId) rows)
    ModifyConversationAtomic conversationId modifyConversation ->
        withPinnedConnection $
            PG.withTransaction do
                lockConversation tables conversationId
                rows <- PG.query (selectItemsQuery tables) (Only conversationId)
                let currentHistory =
                        [ item
                        | ItemRow{historyItem = item} <- (rows :: [ItemRow])
                        ]
                    (result, newItems) = modifyConversation currentHistory
                unless (null newItems) do
                    insertRows <- mkInsertRows conversationId newItems
                    void $
                        PG.executeMany
                            (insertItemsQuery tables)
                            insertRows
                pure result
  where
    itemRowToStoredItem ItemRow{storedItemId = itemId, historyItem = item, createdAt} =
        StoredItem{itemId, item, createdAt}

conversationExists
    :: ( WithConnection :> es
       , IOE :> es
       )
    => ConversationTables
    -> ConversationId
    -> Eff es Bool
conversationExists tables conversationId = do
    rows <-
        PG.query
            (selectConversationExistsQuery tables)
            (Only conversationId)
    pure (not (null (rows :: [Only ConversationId])))

lockConversation
    :: ( WithConnection :> es
       , IOE :> es
       , Error ChatStorageError :> es
       )
    => ConversationTables
    -> ConversationId
    -> Eff es ()
lockConversation tables conversationId = do
    rows <-
        PG.query
            (selectConversationForUpdateQuery tables)
            (Only conversationId)
    when (null (rows :: [Only ConversationId])) $
        throwError (NoSuchConversation conversationId)

mkInsertRows
    :: IOE :> es
    => ConversationId
    -> [HistoryItem]
    -> Eff es [(UUID, ConversationId, HistoryItem)]
mkInsertRows conversationId historyItems =
    forM historyItems $ \historyItem -> do
        itemId <- liftIO nextRandom
        pure (itemId, conversationId, historyItem)

setupConversationTables
    :: ( IOE :> es
       , WithConnection :> es
       )
    => ConversationTables
    -> Eff es ()
setupConversationTables tables =
    withPinnedConnection $
        PG.withTransaction do
            void $ PG.execute_ (createConversationsTableQuery tables)
            void $ PG.execute_ (createItemsTableQuery tables)
            void $ PG.execute_ (createItemsIndexQuery tables)

withPinnedConnection
    :: forall es a
     . ( WithConnection :> es
       )
    => Eff (WithConnection ': es) a
    -> Eff es a
withPinnedConnection action =
    PG.withConnection $ \connection ->
        PG.runWithConnection connection action

createConversationsTableQuery :: ConversationTables -> Query
createConversationsTableQuery tables =
    toQueryText $
        "CREATE TABLE IF NOT EXISTS "
            <> renderIdentifier (conversationTableName tables)
            <> " ("
            <> "conversation_id UUID PRIMARY KEY, "
            <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
            <> ")"

createItemsTableQuery :: ConversationTables -> Query
createItemsTableQuery tables =
    toQueryText $
        "CREATE TABLE IF NOT EXISTS "
            <> renderIdentifier (itemsTableName tables)
            <> " ("
            <> "id BIGSERIAL PRIMARY KEY, "
            <> "item_id UUID NOT NULL UNIQUE, "
            <> "conversation_id UUID NOT NULL REFERENCES "
            <> renderIdentifier (conversationTableName tables)
            <> " (conversation_id) ON DELETE CASCADE, "
            <> "item JSONB NOT NULL, "
            <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
            <> ")"

insertConversationQuery :: ConversationTables -> Query
insertConversationQuery tables =
    toQueryText $
        "INSERT INTO "
            <> renderIdentifier (conversationTableName tables)
            <> " (conversation_id) VALUES (?)"

insertItemsQuery :: ConversationTables -> Query
insertItemsQuery tables =
    toQueryText $
        "INSERT INTO "
            <> renderIdentifier (itemsTableName tables)
            <> " (item_id, conversation_id, item) VALUES (?, ?, ?)"

deleteConversationQuery :: ConversationTables -> Query
deleteConversationQuery tables =
    toQueryText $
        "DELETE FROM "
            <> renderIdentifier (conversationTableName tables)
            <> " WHERE conversation_id = ?"

selectConversationExistsQuery :: ConversationTables -> Query
selectConversationExistsQuery tables =
    toQueryText $
        "SELECT conversation_id FROM "
            <> renderIdentifier (conversationTableName tables)
            <> " WHERE conversation_id = ?"

selectConversationForUpdateQuery :: ConversationTables -> Query
selectConversationForUpdateQuery tables =
    toQueryText $
        "SELECT conversation_id FROM "
            <> renderIdentifier (conversationTableName tables)
            <> " WHERE conversation_id = ? FOR UPDATE"

selectItemsQuery :: ConversationTables -> Query
selectItemsQuery tables =
    toQueryText $
        "SELECT item_id, conversation_id, item, created_at FROM "
            <> renderIdentifier (itemsTableName tables)
            <> " WHERE conversation_id = ? ORDER BY created_at ASC, id ASC"

listConversationsQuery :: ConversationTables -> Query
listConversationsQuery tables =
    toQueryText $
        "SELECT conversation_id FROM "
            <> renderIdentifier (conversationTableName tables)
            <> " ORDER BY created_at ASC"

createItemsIndexQuery :: ConversationTables -> Query
createItemsIndexQuery tables =
    toQueryText $
        "CREATE INDEX IF NOT EXISTS "
            <> renderIdentifier (itemsConversationIndexName tables)
            <> " ON "
            <> renderIdentifier (itemsTableName tables)
            <> " (conversation_id)"

conversationTableName :: ConversationTables -> PgIdentifier
conversationTableName ConversationTables{conversationTable} = conversationTable

itemsTableName :: ConversationTables -> PgIdentifier
itemsTableName ConversationTables{itemsTable} = itemsTable

itemsConversationIndexName :: ConversationTables -> PgIdentifier
itemsConversationIndexName ConversationTables{itemsConversationIndex} = itemsConversationIndex

renderIdentifier :: PgIdentifier -> Text
renderIdentifier = quotePgIdentifier

toQueryText :: Text -> Query
toQueryText = fromString . toString
