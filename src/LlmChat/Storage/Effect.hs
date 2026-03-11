module LlmChat.Storage.Effect where

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
    AppendItem :: ConversationId -> HistoryItem -> LlmChatStorage m ()
    ListConversations :: LlmChatStorage m [ConversationId]

data StoredItem = StoredItem
    { item :: HistoryItem
    , itemId :: UUID
    , createdAt :: UTCTime
    }
    deriving stock (Show, Generic)

type instance DispatchOf LlmChatStorage = 'Dynamic

makeEffect ''LlmChatStorage

getConversation :: LlmChatStorage :> es => ConversationId -> Eff es [HistoryItem]
getConversation convId =
    fmap (\StoredItem{item} -> item) <$> getStoredConversation convId

appendItems :: LlmChatStorage :> es => ConversationId -> [HistoryItem] -> Eff es ()
appendItems convId = traverse_ (appendItem convId)

appendUserMessage
    :: LlmChatStorage :> es
    => ConversationId
    -> Text
    -> Eff es ()
appendUserMessage convId content =
    appendItem convId (user content)

data ChatStorageError
    = NoSuchConversation ConversationId
    deriving stock (Show, Eq)
