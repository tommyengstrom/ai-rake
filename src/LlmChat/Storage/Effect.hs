module LlmChat.Storage.Effect where

import Data.Text (Text)
import Effectful
import Effectful.TH
import LlmChat.Types
import Prelude
import Data.Time
import Control.Lens
import Data.Generics.Labels ()
import Data.UUID (UUID)
import GHC.Generics (Generic)

data LlmChatStorage :: Effect where
    CreateConversation :: SystemPrompt -> LlmChatStorage m ConversationId
    DeleteConversation :: ConversationId -> LlmChatStorage m ()
    GetStoredConversation :: ConversationId -> LlmChatStorage m [StoredMsg]
    AppendMessage :: ConversationId -> ChatMsg -> LlmChatStorage m StoredMsg
    ListConversations :: LlmChatStorage m [ConversationId]

data StoredMsg = StoredMsg
  { msg :: ChatMsg
  , msgId :: UUID
  , createdAt :: UTCTime
  } deriving stock (Show, Generic)

type instance DispatchOf LlmChatStorage = 'Dynamic

makeEffect ''LlmChatStorage

getConversation :: LlmChatStorage :> es
   => ConversationId
   -> Eff es [ChatMsg]
getConversation convId = fmap (^. #msg) <$> getStoredConversation convId


appendUserMessage
    :: LlmChatStorage :> es
    => ConversationId
    -> Text
    -> Eff es StoredMsg
appendUserMessage convId content = do
    let userMsgIn = UserMsg{content}
    appendMessage convId userMsgIn

data ChatStorageError
    = NoSuchConversation ConversationId
    | InsertFailure String
    deriving stock (Show, Eq)
