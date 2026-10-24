{-# OPTIONS_GHC -Wno-orphans #-}

module Rake.Postgres.Internal
    ( PgIdentifier
    , mkPgIdentifier
    , pgIdentifierText
    , quotePgIdentifier
    , ConversationTables (..)
    , conversationTablesFromPrefix
    ) where

import Data.Aeson (Result (Error, Success), Value, fromJSON, toJSON)
import Data.Char (isAsciiLower, isDigit)
import Data.Text qualified as T
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Rake.Types
import Relude

newtype PgIdentifier = PgIdentifier
    { unPgIdentifier :: Text
    }
    deriving stock (Show, Eq, Ord, Generic)

data ConversationTables = ConversationTables
    { conversationTable :: PgIdentifier
    , itemsTable :: PgIdentifier
    , itemsConversationIndex :: PgIdentifier
    }
    deriving stock (Show, Eq, Ord, Generic)

mkPgIdentifier :: Text -> Either Text PgIdentifier
mkPgIdentifier rawIdentifier
    | T.null rawIdentifier =
        Left validationMessage
    | T.length rawIdentifier > maxIdentifierLength =
        Left validationMessage
    | not (startsWithValidChar rawIdentifier) =
        Left validationMessage
    | not (T.all isValidIdentifierChar rawIdentifier) =
        Left validationMessage
    | otherwise =
        Right (PgIdentifier rawIdentifier)
  where
    validationMessage =
        "PostgreSQL identifiers must be 1-63 characters, start with a lowercase letter or underscore, and contain only lowercase ASCII letters, digits, or underscores."

pgIdentifierText :: PgIdentifier -> Text
pgIdentifierText (PgIdentifier identifier) = identifier

quotePgIdentifier :: PgIdentifier -> Text
quotePgIdentifier identifier =
    "\"" <> pgIdentifierText identifier <> "\""

conversationTablesFromPrefix :: PgIdentifier -> Either Text ConversationTables
conversationTablesFromPrefix prefix = do
    conversationTable <- deriveIdentifier "_conversations"
    itemsTable <- deriveIdentifier "_items"
    itemsConversationIndex <-
        mkPgIdentifier ("idx_" <> pgIdentifierText itemsTable <> "_conversation_id")
    pure ConversationTables{conversationTable, itemsTable, itemsConversationIndex}
  where
    deriveIdentifier suffix =
        mkPgIdentifier (pgIdentifierText prefix <> suffix)

maxIdentifierLength :: Int
maxIdentifierLength = 63

startsWithValidChar :: Text -> Bool
startsWithValidChar identifier = case T.uncons identifier of
    Just (firstChar, _) ->
        firstChar == '_' || isAsciiLower firstChar
    Nothing ->
        False

isValidIdentifierChar :: Char -> Bool
isValidIdentifierChar char =
    char == '_'
        || isAsciiLower char
        || isDigit char

instance ToField ConversationId where
    toField (ConversationId uuid) = toField uuid

instance FromField ConversationId where
    fromField field maybeData = ConversationId <$> fromField field maybeData

instance ToField HistoryItemId where
    toField (HistoryItemId uuid) = toField uuid

instance FromField HistoryItemId where
    fromField field maybeData = HistoryItemId <$> fromField field maybeData

instance ToField HistoryItem where
    toField historyItem = toField (toJSON historyItem)

instance FromField HistoryItem where
    fromField field maybeData = do
        (value :: Value) <- fromField field maybeData
        case fromJSON value of
            Success historyItem ->
                pure historyItem
            Error err ->
                returnError ConversionFailed field ("Could not decode HistoryItem from JSON: " <> err)
