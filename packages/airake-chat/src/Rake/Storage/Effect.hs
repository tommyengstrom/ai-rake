module Rake.Storage.Effect
    ( RakeStorage (..)
    , StoredItem (..)
    , ChatStorageError (..)
    , storedItemHistoryItem
    , createConversation
    , deleteConversation
    , getStoredConversation
    , listConversations
    , modifyConversationAtomic
    , getConversation
    , appendItem
    , appendItems
    , appendUserMessage
    ) where

import Data.Time
import Effectful
import Effectful.TH
import Rake.Types
import Relude

data RakeStorage :: Effect where
    CreateConversation :: RakeStorage m ConversationId
    DeleteConversation :: ConversationId -> RakeStorage m ()
    GetStoredConversation :: ConversationId -> RakeStorage m [StoredItem]
    AppendConversationItems :: ConversationId -> [HistoryItem] -> RakeStorage m ()
    ListConversations :: RakeStorage m [ConversationId]
    ModifyConversationAtomic
        :: ConversationId
        -> ([HistoryItem] -> (a, [HistoryItem]))
        -> RakeStorage m a

data StoredItem = StoredItem
    { item :: HistoryItem
    , itemId :: HistoryItemId
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

data ChatStorageError
    = NoSuchConversation ConversationId
    | DuplicateHistoryItemId ConversationId HistoryItemId
    deriving stock (Show, Eq)

type instance DispatchOf RakeStorage = 'Dynamic

makeEffect ''RakeStorage

getConversation :: RakeStorage :> es => ConversationId -> Eff es [HistoryItem]
getConversation convId =
    fmap storedItemHistoryItem <$> getStoredConversation convId

appendItem :: RakeStorage :> es => ConversationId -> HistoryItem -> Eff es ()
appendItem convId historyItem =
    appendConversationItems convId [historyItem]

appendItems :: RakeStorage :> es => ConversationId -> [HistoryItem] -> Eff es ()
appendItems convId historyItems =
    unless (null historyItems) $
        appendConversationItems convId historyItems

appendUserMessage
    :: RakeStorage :> es
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage convId content =
    appendItem convId (user content)

storedItemHistoryItem :: StoredItem -> HistoryItem
storedItemHistoryItem StoredItem{item, itemId} =
    setHistoryItemId (Just itemId) item
