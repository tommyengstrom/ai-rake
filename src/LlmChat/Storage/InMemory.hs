{-# LANGUAGE RecordWildCards #-}

module LlmChat.Storage.InMemory where

import Data.Map.Strict qualified as Map
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM qualified as STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Time
import LlmChat.Storage.Effect
import LlmChat.Types
import Relude

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
    tvar <- STM.newTVarIO (mempty :: Map.Map ConversationId [StoredItem])
    interpretWith eff \_ -> \case
        CreateConversation -> do
            conversationId <- ConversationId <$> liftIO nextRandom
            STM.atomically $
                STM.modifyTVar' tvar (Map.insert conversationId [])
            pure conversationId
        DeleteConversation conversationId ->
            STM.atomically $
                STM.modifyTVar' tvar (Map.delete conversationId)
        GetStoredConversation conversationId -> do
            conversations <- STM.readTVarIO tvar
            case Map.lookup conversationId conversations of
                Nothing -> throwError $ NoSuchConversation conversationId
                Just conversation -> pure conversation
        AppendItem conversationId historyItem -> do
            storedItem <- mkStoredItem historyItem
            inserted <- STM.atomically do
                conversations <- STM.readTVar tvar
                case Map.lookup conversationId conversations of
                    Nothing -> pure False
                    Just conversation -> do
                        STM.writeTVar tvar $
                            Map.insert conversationId (conversation <> [storedItem]) conversations
                        pure True

            unless inserted $
                throwError $ NoSuchConversation conversationId
        ListConversations ->
            Map.keys <$> STM.readTVarIO tvar
  where
    mkStoredItem item = do
        createdAt <- currentTime
        itemId <- liftIO nextRandom
        pure StoredItem{..}
