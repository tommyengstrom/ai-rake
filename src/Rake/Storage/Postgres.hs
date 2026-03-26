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
import Data.Set qualified as Set
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
    { storedItemId :: HistoryItemId
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
                    existingItemIds <- Set.fromList . map fromOnly <$> PG.query (selectConversationItemIdsQuery tables) (Only conversationId)
                    insertRows <- mkInsertRows conversationId existingItemIds historyItems
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
                        [ storedItemHistoryItem StoredItem{itemId = storedItemId, item = historyItem, createdAt}
                        | ItemRow{storedItemId, historyItem, createdAt} <- (rows :: [ItemRow])
                        ]
                    existingItemIds =
                        Set.fromList
                            [ storedItemId
                            | ItemRow{storedItemId} <- (rows :: [ItemRow])
                            ]
                    (result, newItems) = modifyConversation currentHistory
                unless (null newItems) do
                    insertRows <- mkInsertRows conversationId existingItemIds newItems
                    void $
                        PG.executeMany
                            (insertItemsQuery tables)
                            insertRows
                pure result
  where
    itemRowToStoredItem ItemRow{storedItemId = itemId, historyItem = item, createdAt} =
        StoredItem{itemId, item = setHistoryItemId (Just itemId) item, createdAt}

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
    :: ( IOE :> es
       , Error ChatStorageError :> es
       )
    => ConversationId
    -> Set.Set HistoryItemId
    -> [HistoryItem]
    -> Eff es [(HistoryItemId, ConversationId, HistoryItem)]
mkInsertRows conversationId existingItemIds historyItems = do
    normalizedItems <- ensureHistoryItemIds historyItems
    validateConversationItemIds conversationId existingItemIds normalizedItems
    forM normalizedItems $ \normalizedItem -> do
        let itemId =
                fromMaybe
                    (error "ensureHistoryItemId returned a HistoryItem without an id")
                    (historyItemId normalizedItem)
        pure (itemId, conversationId, normalizedItem)

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
            void $ PG.execute_ (dropLegacyGlobalItemIdConstraintQuery tables)
            void $ PG.execute_ (createItemsConversationItemUniqueIndexQuery tables)
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
            <> "item_id UUID NOT NULL, "
            <> "conversation_id UUID NOT NULL REFERENCES "
            <> renderIdentifier (conversationTableName tables)
            <> " (conversation_id) ON DELETE CASCADE, "
            <> "item JSONB NOT NULL, "
            <> "created_at timestamp with time zone NOT NULL DEFAULT now()"
            <> ")"

dropLegacyGlobalItemIdConstraintQuery :: ConversationTables -> Query
dropLegacyGlobalItemIdConstraintQuery tables =
    toQueryText $
        "ALTER TABLE "
            <> renderIdentifier (itemsTableName tables)
            <> " DROP CONSTRAINT IF EXISTS "
            <> renderIdentifier (legacyGlobalItemIdConstraintName tables)

createItemsConversationItemUniqueIndexQuery :: ConversationTables -> Query
createItemsConversationItemUniqueIndexQuery tables =
    toQueryText $
        "CREATE UNIQUE INDEX IF NOT EXISTS "
            <> renderIdentifier (itemsConversationItemUniqueIndexName tables)
            <> " ON "
            <> renderIdentifier (itemsTableName tables)
            <> " (conversation_id, item_id)"

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

selectConversationItemIdsQuery :: ConversationTables -> Query
selectConversationItemIdsQuery tables =
    toQueryText $
        "SELECT item_id FROM "
            <> renderIdentifier (itemsTableName tables)
            <> " WHERE conversation_id = ?"

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

itemsConversationItemUniqueIndexName :: ConversationTables -> PgIdentifier
itemsConversationItemUniqueIndexName tables =
    fromRight
        (error "ConversationTables produced an invalid composite item-id index name")
        (mkPgIdentifier ("uidx_" <> pgIdentifierText (itemsTableName tables) <> "_conv_item"))

legacyGlobalItemIdConstraintName :: ConversationTables -> PgIdentifier
legacyGlobalItemIdConstraintName tables =
    fromRight
        (error "ConversationTables produced an invalid legacy item-id constraint name")
        (mkPgIdentifier (pgIdentifierText (itemsTableName tables) <> "_item_id_key"))

renderIdentifier :: PgIdentifier -> Text
renderIdentifier = quotePgIdentifier

toQueryText :: Text -> Query
toQueryText = fromString . toString

validateConversationItemIds
    :: Error ChatStorageError :> es
    => ConversationId
    -> Set.Set HistoryItemId
    -> [HistoryItem]
    -> Eff es ()
validateConversationItemIds conversationId existingItemIds normalizedItems =
    case duplicateHistoryItemId existingItemIds normalizedItems of
        Just duplicateItemId ->
            throwError (DuplicateHistoryItemId conversationId duplicateItemId)
        Nothing ->
            pure ()

duplicateHistoryItemId :: Set.Set HistoryItemId -> [HistoryItem] -> Maybe HistoryItemId
duplicateHistoryItemId existingItemIds normalizedItems =
    go existingItemIds
        [ itemId
        | historyItem <- normalizedItems
        , Just itemId <- [historyItemId historyItem]
        ]
  where
    go _ [] =
        Nothing
    go seenIds (itemId : remainingItemIds)
        | Set.member itemId seenIds =
            Just itemId
        | otherwise =
            go (Set.insert itemId seenIds) remainingItemIds
