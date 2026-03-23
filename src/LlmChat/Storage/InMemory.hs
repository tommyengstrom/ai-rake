{-# LANGUAGE RecordWildCards #-}

module LlmChat.Storage.InMemory
    ( runLlmChatStorageInMemory
    ) where

import Data.Map.Strict qualified as Map
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.MVar qualified as MVar
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
    conversationsVar <- MVar.newMVar (mempty :: Map.Map ConversationId [StoredItem])
    interpretWith eff \_ -> \case
        CreateConversation ->
            MVar.modifyMVar conversationsVar $ \conversations -> do
                conversationId <- freshConversationId conversations
                pure (Map.insert conversationId [] conversations, conversationId)
        DeleteConversation conversationId ->
            MVar.modifyMVar_ conversationsVar $
                pure . Map.delete conversationId
        GetStoredConversation conversationId ->
            MVar.withMVar conversationsVar $ \conversations ->
                case Map.lookup conversationId conversations of
                    Nothing ->
                        throwError (NoSuchConversation conversationId)
                    Just conversation ->
                        pure conversation
        AppendConversationItems conversationId historyItems ->
            MVar.modifyMVar_ conversationsVar $ \conversations -> do
                conversation <- lookupConversation conversations conversationId
                storedItems <- traverse mkStoredItem historyItems
                pure $
                    Map.insert conversationId (conversation <> storedItems) conversations
        ListConversations ->
            MVar.withMVar conversationsVar (pure . Map.keys)
        ModifyConversationAtomic conversationId modifyConversation ->
            MVar.modifyMVar conversationsVar $ \conversations -> do
                conversation <- lookupConversation conversations conversationId
                let currentHistory = map (\StoredItem{item} -> item) conversation
                    (result, newItems) = modifyConversation currentHistory
                storedItems <- traverse mkStoredItem newItems
                pure
                    ( Map.insert conversationId (conversation <> storedItems) conversations
                    , result
                    )
  where
    freshConversationId conversations = do
        conversationId <- ConversationId <$> liftIO nextRandom
        if Map.member conversationId conversations
            then freshConversationId conversations
            else pure conversationId

    lookupConversation conversations conversationId =
        case Map.lookup conversationId conversations of
            Nothing ->
                throwError (NoSuchConversation conversationId)
            Just conversation ->
                pure conversation

    mkStoredItem item = do
        createdAt <- currentTime
        itemId <- liftIO nextRandom
        pure StoredItem{..}
