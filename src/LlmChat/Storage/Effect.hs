module LlmChat.Storage.Effect
    ( LlmChatStorage (..)
    , StoredItem (..)
    , ChatStorageError (..)
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
import Data.UUID (UUID)
import Effectful
import Effectful.TH
import LlmChat.Types
import Relude

data LlmChatStorage :: Effect where
    CreateConversation :: LlmChatStorage m ConversationId
    DeleteConversation :: ConversationId -> LlmChatStorage m ()
    GetStoredConversation :: ConversationId -> LlmChatStorage m [StoredItem]
    AppendConversationItems :: ConversationId -> [HistoryItem] -> LlmChatStorage m ()
    ListConversations :: LlmChatStorage m [ConversationId]
    ModifyConversationAtomic
        :: ConversationId
        -> ([HistoryItem] -> (a, [HistoryItem]))
        -> LlmChatStorage m a

data StoredItem = StoredItem
    { item :: HistoryItem
    , itemId :: UUID
    , createdAt :: UTCTime
    }
    deriving stock (Show, Eq, Generic)

data ChatStorageError
    = NoSuchConversation ConversationId
    deriving stock (Show, Eq)

type instance DispatchOf LlmChatStorage = 'Dynamic

makeEffect ''LlmChatStorage

getConversation :: LlmChatStorage :> es => ConversationId -> Eff es [HistoryItem]
getConversation convId =
    fmap (\StoredItem{item} -> item) <$> getStoredConversation convId

appendItem :: LlmChatStorage :> es => ConversationId -> HistoryItem -> Eff es ()
appendItem convId historyItem =
    appendConversationItems convId [historyItem]

appendItems :: LlmChatStorage :> es => ConversationId -> [HistoryItem] -> Eff es ()
appendItems convId historyItems =
    unless (null historyItems) $
        appendConversationItems convId historyItems

appendUserMessage
    :: LlmChatStorage :> es
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage convId content =
    appendItem convId (user content)
