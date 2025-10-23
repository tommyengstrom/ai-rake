{-# LANGUAGE RecordWildCards #-}

module LlmChat.Storage.InMemory where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Time
import LlmChat.Storage.Effect
import LlmChat.Types
import Prelude

runLlmChatStorageInMemory
    :: forall es a
     . ( Error ChatStorageError :> es
       , Concurrent :> es
       , IOE :> es
       , Time :> es
       )
    => Eff (LlmChatStorage ': es) a
    -> Eff es a
runLlmChatStorageInMemory eff = do
    tvar <- newTVarIO (mempty :: Map ConversationId [StoredMsg])
    interpretWith eff \_ -> \case
        CreateConversation systemPrompt -> do
            conversationId <- ConversationId <$> liftIO nextRandom
            storedMsg <- mkStoredMsg $ SystemMsg systemPrompt
            atomically $
                modifyTVar' tvar (Map.insert conversationId [storedMsg])
            pure conversationId
        DeleteConversation conversationId -> do
            atomically $ modifyTVar' tvar (Map.delete conversationId)
        GetStoredConversation conversationId -> do
            conversations <- readTVarIO tvar
            let conv = Map.lookup conversationId conversations
            case conv of
                Nothing -> throwError $ NoSuchConversation conversationId
                Just c -> pure c
        AppendMessage conversationId msg -> do
            storedMsg <- mkStoredMsg msg
            atomically $ modifyTVar' tvar $ Map.adjust (<> [storedMsg]) conversationId
            pure storedMsg
        ListConversations -> do
            conversations <- readTVarIO tvar
            pure $ Map.keys conversations
  where
    mkStoredMsg msg = do
        createdAt <- currentTime
        msgId <- liftIO nextRandom
        pure StoredMsg{..}
