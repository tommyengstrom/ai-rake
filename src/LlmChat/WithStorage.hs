module LlmChat.WithStorage where

import Control.Lens (folded, toListOf)
import Control.Monad.Morph
import Effectful
import Effectful.Error.Static
import LlmChat.Effect as X
import LlmChat.Storage.Effect as X
import LlmChat.Types as X
import Relude
import Servant.API (SourceIO)
import Servant.Types.SourceT
import LlmChat (respondWithToolsStepT')

withStorage
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => ([ChatMsg] -> Eff es [ChatMsg])
    -> ConversationId
    -> Eff es [ChatMsg]
withStorage = withStorageBy id

withStorageStructured
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => ([ChatMsg] -> Eff es ([ChatMsg], a))
    -> ConversationId
    -> Eff es ([ChatMsg], a)
withStorageStructured = withStorageBy fst

withStorageBy
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => (a -> [ChatMsg])
    -> ([ChatMsg] -> Eff es a)
    -> ConversationId
    -> Eff es a
withStorageBy extract action convId = do
    conversation <- getConversation convId
    result <- action conversation
    traverse_ (appendMessage convId) (extract result)
    pure result


respondWithToolsSourceIOStorage
    :: forall es
     . ( Error LlmChatError :> es
       , LlmChat :> es
       , LlmChatStorage :> es
       , IOE :> es
       )
    => ResponseFormat
    -> [ToolDef es]
    -> ConversationId
    -> Eff es (SourceIO (Either ChatStorageError StoredMsg))
respondWithToolsSourceIOStorage responseFormat tools convId = do
    conversation <- toListOf (folded . #msg) <$> getStoredConversation convId
    withEffToIO SeqForkUnlift $ \runInIO ->
        pure
            . hoist runInIO
            . fromStepT
            $ respondWithToolsStepT'
                (\msg -> runErrorNoCallStack $ appendMessage convId msg)
                responseFormat
                tools
                conversation
