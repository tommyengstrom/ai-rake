{-# LANGUAGE RecordWildCards #-}

module Rake.Storage.InMemory
    ( runRakeStorageInMemory
    ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Time
import Rake.Storage.Effect
import Rake.Types
import Relude

runRakeStorageInMemory
    :: forall es a
     . ( Error ChatStorageError :> es
       , Concurrent :> es
       , IOE :> es
       , Time :> es
       )
    => Eff (RakeStorage ': es) a
    -> Eff es a
runRakeStorageInMemory eff = do
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
                storedItems <- mkStoredItems conversationId conversation historyItems
                pure $
                    Map.insert conversationId (conversation <> storedItems) conversations
        ListConversations ->
            MVar.withMVar conversationsVar (pure . Map.keys)
        ModifyConversationAtomic conversationId modifyConversation ->
            MVar.modifyMVar conversationsVar $ \conversations -> do
                conversation <- lookupConversation conversations conversationId
                let currentHistory = map storedItemHistoryItem conversation
                    (result, newItems) = modifyConversation currentHistory
                storedItems <- mkStoredItems conversationId conversation newItems
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

    mkStoredItems conversationId conversation historyItems = do
        normalizedItems <- ensureHistoryItemIds historyItems
        validateConversationItemIds conversationId conversation normalizedItems
        traverse mkStoredItem normalizedItems

    mkStoredItem normalizedItem = do
        createdAt <- currentTime
        let item = normalizedItem
            itemId =
                fromMaybe
                    (error "ensureHistoryItemId returned a HistoryItem without an id")
                    (historyItemId normalizedItem)
        pure StoredItem{item, itemId, createdAt}

    validateConversationItemIds conversationId conversation normalizedItems =
        case duplicateHistoryItemId existingItemIds newItemIds of
            Just duplicateItemId ->
                throwError (DuplicateHistoryItemId conversationId duplicateItemId)
            Nothing ->
                pure ()
      where
        existingItemIds =
            [ itemId
            | StoredItem{itemId} <- conversation
            ]
        newItemIds =
            [ itemId
            | historyItem <- normalizedItems
            , Just itemId <- [historyItemId historyItem]
            ]

    duplicateHistoryItemId existingItemIds =
        go (Set.fromList existingItemIds)
      where
        go _ [] =
            Nothing
        go seenIds (itemId : remainingItemIds)
            | Set.member itemId seenIds =
                Just itemId
            | otherwise =
                go (Set.insert itemId seenIds) remainingItemIds
