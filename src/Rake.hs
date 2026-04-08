module Rake
    ( module X
    , chatOutcome
    , streamChatOutcome
    , chatOutcomeAppendedItems
    , withResumableChat
    , withResumableStreamingChat
    , conversationReplayState
    , effectiveSystemSnapshot
    , renderableReplayHistory
    , validResetCheckpoints
    , latestValidCheckpoint
    , resetToLatestValidCheckpoint
    , itemTexts
    , filterUnavailableMediaHistory
    , lastAssistantTexts
    , lastAssistantTextsStrict
    , decodeLastAssistant
    , decodeLastAssistantStrict
    , withStorage
    , withStorageBy
    ) where

import Data.Aeson (FromJSON, Value (..), eitherDecodeStrictText)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Rake.Effect as X
import Rake.Media as X
import Rake.MediaStorage.Effect as X
import Rake.Storage.Effect as X
import Rake.TTS as X
import Rake.Tool as X
import Rake.Types as X
import Relude

type ProviderRoundRequest es =
    [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es ProviderRound

data PendingToolCallState = PendingToolCallState
    { pendingToolCall :: ToolCall
    , pendingToolCallItemIndex :: Int
    }

data ToolMatchingState = ToolMatchingState
    { unresolvedToolCallsById :: Map.Map ToolCallId PendingToolCallState
    , unresolvedToolCallOrderRev :: [ToolCallId]
    , unresolvedToolCallCountsByItemIndex :: Map.Map Int Int
    , resolvedToolCallItemIndexes :: Set.Set Int
    , replayableToolResultIndexes :: Set.Set Int
    , strayToolResultIndexes :: Set.Set Int
    , duplicateToolCallBlock :: Maybe ReplayBlockReason
    }

chatOutcome
    :: ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       , Rake :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es ChatOutcome
chatOutcome ChatConfig{tools, responseFormat, sampling = samplingOptions, onItem, maxToolRounds} conversation = do
    normalizedConversation <- ensureHistoryItemIds conversation
    handleToolLoop
        getLlmResponse
        tools
        responseFormat
        samplingOptions
        onItem
        maxToolRounds
        conversation
        normalizedConversation
        []
        0

streamChatOutcome
    :: ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       , Rake :> es
       )
    => StreamCallbacks es
    -> ChatConfig es
    -> [HistoryItem]
    -> Eff es ChatOutcome
streamChatOutcome StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} ChatConfig{tools, responseFormat, sampling = samplingOptions, onItem, maxToolRounds} conversation = do
    normalizedConversation <- ensureHistoryItemIds conversation
    handleToolLoop
        (getLlmResponseStream onAssistantTextDelta onAssistantRefusalDelta)
        tools
        responseFormat
        samplingOptions
        onItem
        maxToolRounds
        conversation
        normalizedConversation
        []
        0

chatOutcomeAppendedItems :: ChatOutcome -> [HistoryItem]
chatOutcomeAppendedItems = \case
    ChatFinished{appendedItems} ->
        appendedItems
    ChatPaused{appendedItems} ->
        appendedItems
    ChatFailed{appendedItems} ->
        appendedItems

itemTexts :: HistoryItem -> [Text]
itemTexts HistoryItem{genericItem = historyGenericItem} =
    genericItemTexts historyGenericItem

filterUnavailableMediaHistory
    :: RakeMediaStorage :> es
    => ProviderApiFamily
    -> [HistoryItem]
    -> Eff es [HistoryItem]
filterUnavailableMediaHistory providerFamily =
    fmap catMaybes . traverse (filterUnavailableMediaHistoryItem providerFamily)

filterUnavailableMediaHistoryItem
    :: RakeMediaStorage :> es
    => ProviderApiFamily
    -> HistoryItem
    -> Eff es (Maybe HistoryItem)
filterUnavailableMediaHistoryItem
    providerFamily
    historyItem@HistoryItem
        { genericItem = GenericMessage{role, parts}
        } = do
        filteredParts <- catMaybes <$> traverse (filterAvailableMessagePart providerFamily) parts
        pure
            $ case filteredParts of
                [] ->
                    Nothing
                _
                    | filteredParts == parts ->
                        Just historyItem
                    | otherwise ->
                        Just
                            historyItem
                                { genericItem = GenericMessage{role, parts = filteredParts}
                                , providerItem = Nothing
                                }
filterUnavailableMediaHistoryItem _ historyItem =
    pure (Just historyItem)

filterAvailableMessagePart
    :: RakeMediaStorage :> es
    => ProviderApiFamily
    -> MessagePart
    -> Eff es (Maybe MessagePart)
filterAvailableMessagePart providerFamily part = case part of
    PartText{} ->
        pure (Just part)
    PartRefusal{} ->
        pure (Just part)
    PartImage{blobId} ->
        keepIfMediaAvailable providerFamily blobId part
    PartAudio{blobId} ->
        keepIfMediaAvailable providerFamily blobId part
    PartFile{blobId} ->
        keepIfMediaAvailable providerFamily blobId part

keepIfMediaAvailable
    :: RakeMediaStorage :> es
    => ProviderApiFamily
    -> MediaBlobId
    -> MessagePart
    -> Eff es (Maybe MessagePart)
keepIfMediaAvailable providerFamily blobId part = do
    maybeMediaRef <- lookupMediaReference providerFamily blobId
    pure
        $ case maybeMediaRef of
            Just{} ->
                Just part
            Nothing ->
                Nothing

conversationReplayState :: [HistoryItem] -> ReplayState
conversationReplayState history =
    ReplayState
        { activeHistory = activeItems
        , replayHistory = reverse replayableHistoryRev
        , resumableToolCalls = reverse resumableToolCallsRev
        , pendingArtifacts = reverse pendingArtifactsRev
        , blocked =
            duplicateHistoryItemIdBlock <|> resetBlock <|> failureBlock <|> duplicateToolCallBlock
        }
  where
    duplicateHistoryItemIdBlock =
        ReplayBlocked
            . duplicateHistoryItemIdReason
            <$> firstDuplicateHistoryItemId history

    (activeItems, resetBlock) = foldl' applyControlItem ([], Nothing) history

    failureBlock =
        ReplayBlocked
            <$> viaNonEmpty head [reason | Just reason <- map historyItemFailureSignal activeItems]

    ToolMatchingState
        { unresolvedToolCallsById = matchingUnresolvedToolCallsById
        , unresolvedToolCallOrderRev = matchingUnresolvedToolCallOrderRev
        , unresolvedToolCallCountsByItemIndex = matchingUnresolvedToolCallCountsByItemIndex
        , resolvedToolCallItemIndexes = matchingResolvedToolCallItemIndexes
        , replayableToolResultIndexes = matchingReplayableToolResultIndexes
        , strayToolResultIndexes = matchingStrayToolResultIndexes
        , duplicateToolCallBlock
        } =
            foldl'
                matchToolHistoryItem
                emptyToolMatchingState
                (zip [0 ..] activeItems)

    unresolvedToolCallItemIndexes =
        Set.fromList (Map.keys matchingUnresolvedToolCallCountsByItemIndex)

    resumableToolCallsRev =
        [ pendingToolCall
        | toolCallId <- matchingUnresolvedToolCallOrderRev
        , Just PendingToolCallState{pendingToolCall} <-
            [Map.lookup toolCallId matchingUnresolvedToolCallsById]
        ]

    (replayableHistoryRev, pendingArtifactsRev) =
        foldl'
            classifyReplayItem
            ([], [])
            (zip [0 ..] activeItems)

    applyControlItem (currentActiveHistory, currentBlocked) historyItem =
        case genericItem historyItem of
            GenericResetTo{checkpoint} ->
                case applyResetCheckpoint checkpoint currentActiveHistory of
                    Left blockedReason ->
                        (currentActiveHistory, Just blockedReason)
                    Right resetHistory ->
                        (resetHistory, Nothing)
            _ ->
                (currentActiveHistory <> [historyItem], currentBlocked)

    matchToolHistoryItem matchingState (itemIndex, historyItem) =
        let stateAfterToolCalls =
                foldl'
                    (registerPendingToolCall itemIndex historyItem)
                    matchingState
                    (historyItemToolCalls historyItem)
         in case genericItem historyItem of
                GenericToolResult{toolResult = ToolResult{toolCallId}} ->
                    resolveToolResult itemIndex toolCallId stateAfterToolCalls
                _ ->
                    stateAfterToolCalls

    registerPendingToolCall
        itemIndex
        _historyItem
        matchingState@ToolMatchingState
            { unresolvedToolCallsById = stateUnresolvedToolCallsById
            , unresolvedToolCallOrderRev = stateUnresolvedToolCallOrderRev
            , unresolvedToolCallCountsByItemIndex = stateUnresolvedToolCallCountsByItemIndex
            , duplicateToolCallBlock = stateDuplicateToolCallBlock
            }
        pendingToolCall@ToolCall{toolCallId} =
            case Map.lookup toolCallId stateUnresolvedToolCallsById of
                Just{} ->
                    matchingState
                        { duplicateToolCallBlock =
                            stateDuplicateToolCallBlock
                                <|> Just (ReplayBlocked (duplicateUnresolvedToolCallReason toolCallId))
                        }
                Nothing ->
                    matchingState
                        { unresolvedToolCallsById =
                            Map.insert
                                toolCallId
                                PendingToolCallState
                                    { pendingToolCall
                                    , pendingToolCallItemIndex = itemIndex
                                    }
                                stateUnresolvedToolCallsById
                        , unresolvedToolCallOrderRev =
                            toolCallId : filter (/= toolCallId) stateUnresolvedToolCallOrderRev
                        , unresolvedToolCallCountsByItemIndex =
                            Map.insertWith (+) itemIndex 1 stateUnresolvedToolCallCountsByItemIndex
                        }

    resolveToolResult
        itemIndex
        toolCallId
        matchingState@ToolMatchingState
            { unresolvedToolCallsById = stateUnresolvedToolCallsById
            , unresolvedToolCallCountsByItemIndex = stateUnresolvedToolCallCountsByItemIndex
            , resolvedToolCallItemIndexes = stateResolvedToolCallItemIndexes
            , replayableToolResultIndexes = stateReplayableToolResultIndexes
            , strayToolResultIndexes = stateStrayToolResultIndexes
            } =
            case Map.lookup toolCallId stateUnresolvedToolCallsById of
                Nothing ->
                    matchingState
                        { strayToolResultIndexes =
                            Set.insert itemIndex stateStrayToolResultIndexes
                        }
                Just PendingToolCallState{pendingToolCallItemIndex} ->
                    let unresolvedToolCallsById' =
                            Map.delete toolCallId stateUnresolvedToolCallsById
                        unresolvedToolCallCountsByItemIndex' =
                            decrementItemCount pendingToolCallItemIndex stateUnresolvedToolCallCountsByItemIndex
                        toolCallItemFullyResolved =
                            Map.notMember pendingToolCallItemIndex unresolvedToolCallCountsByItemIndex'
                        resolvedToolCallItemIndexes' =
                            if toolCallItemFullyResolved
                                then Set.insert pendingToolCallItemIndex stateResolvedToolCallItemIndexes
                                else stateResolvedToolCallItemIndexes
                     in matchingState
                            { unresolvedToolCallsById = unresolvedToolCallsById'
                            , unresolvedToolCallCountsByItemIndex = unresolvedToolCallCountsByItemIndex'
                            , resolvedToolCallItemIndexes = resolvedToolCallItemIndexes'
                            , replayableToolResultIndexes =
                                Set.insert itemIndex stateReplayableToolResultIndexes
                            }

    classifyReplayItem (replayItems, pendingItems) (itemIndex, historyItem) =
        case genericItem historyItem of
            GenericResetTo{} ->
                (replayItems, pendingItems)
            GenericReplayBarrier{} ->
                (replayItems, pendingItems)
            GenericToolResult{}
                | Set.member itemIndex matchingReplayableToolResultIndexes ->
                    (historyItem : replayItems, pendingItems)
                | Set.member itemIndex matchingStrayToolResultIndexes ->
                    (replayItems, pendingItems)
                | historyItemLifecycle historyItem == ItemPending ->
                    (replayItems, historyItem : pendingItems)
                | otherwise ->
                    (replayItems, pendingItems)
            GenericNonPortable{}
                | historyItemLifecycle historyItem == ItemCompleted ->
                    (historyItem : replayItems, pendingItems)
                | otherwise ->
                    (replayItems, historyItem : pendingItems)
            _ ->
                if historyItemHasFailure historyItem
                    then
                        (replayItems, historyItem : pendingItems)
                    else
                        if Set.member itemIndex unresolvedToolCallItemIndexes
                            then
                                (replayItems, historyItem : pendingItems)
                            else
                                if hasReplayableToolCalls itemIndex historyItem
                                    then
                                        (historyItem : replayItems, pendingItems)
                                    else
                                        if not (null (historyItemToolCalls historyItem))
                                            then
                                                (replayItems, historyItem : pendingItems)
                                            else
                                                if historyItemLifecycle historyItem == ItemCompleted
                                                    then
                                                        (historyItem : replayItems, pendingItems)
                                                    else
                                                        (replayItems, historyItem : pendingItems)

    hasReplayableToolCalls itemIndex historyItem =
        not (null (historyItemToolCalls historyItem))
            && Set.member itemIndex matchingResolvedToolCallItemIndexes

    applyResetCheckpoint checkpoint currentActiveHistory =
        case checkpoint of
            ResetToStart ->
                Right []
            ResetToItem targetItemId ->
                case break ((== Just targetItemId) . historyItemId) currentActiveHistory of
                    (_beforeReset, []) ->
                        Left (ReplayInvalidReset checkpoint)
                    (beforeReset, targetItem : _) ->
                        Right (beforeReset <> [targetItem])

lastAssistantTexts :: [HistoryItem] -> [Text]
lastAssistantTexts history =
    fromMaybe []
        $ viaNonEmpty
            head
            [ messagePartsText parts
            | HistoryItem
                { itemLifecycle = ItemCompleted
                , genericItem = GenericMessage{role = GenericAssistant, parts}
                } <-
                List.reverse (flattenReplayHistory history)
            ]

lastAssistantTextsStrict :: [HistoryItem] -> [Text]
lastAssistantTextsStrict history =
    case replayBlockReason history of
        Just{} ->
            []
        Nothing ->
            concatMap itemTexts (assistantMessageTail history)

decodeLastAssistant
    :: forall a
     . FromJSON a
    => [HistoryItem]
    -> Either RakeError a
decodeLastAssistant history =
    case viaNonEmpty head assistantMessages of
        Nothing ->
            Left (LlmExpectationError "Assistant returned no message")
        Just GenericMessage{parts} ->
            decodeAssistantMessageParts "Assistant last message" parts
        Just _ ->
            Left (LlmExpectationError "Assistant last message is not a message item")
  where
    assistantMessages =
        [ assistantItem
        | HistoryItem
            { itemLifecycle = ItemCompleted
            , genericItem = assistantItem@GenericMessage{role = GenericAssistant}
            } <-
            List.reverse (flattenReplayHistory history)
        ]

decodeLastAssistantStrict
    :: forall a
     . FromJSON a
    => [HistoryItem]
    -> Either RakeError a
decodeLastAssistantStrict history =
    case replayBlockedConversation history history of
        Just blockedError ->
            Left blockedError
        Nothing ->
            case assistantMessageTail history of
                [] ->
                    Left (LlmExpectationError "Assistant returned no message in latest turn")
                [HistoryItem{genericItem = GenericMessage{parts}}] ->
                    decodeAssistantMessageParts "Assistant latest turn" parts
                _ ->
                    Left (LlmExpectationError "Assistant latest turn is not a single message")

executeToolCalls :: [ToolDef es] -> [ToolCall] -> Eff es [HistoryItem]
executeToolCalls tools toolCalls =
    forM toolCalls $ \ToolCall{toolCallId, toolName = requestedToolName, toolArgs} -> do
        toolResponse <- case find (matchesTool requestedToolName) tools of
            Nothing ->
                pure . ToolResponseText $ "Tool not found: " <> requestedToolName
            Just ToolDef{executeFunction} -> do
                let args = Object (KM.fromMap (Map.mapKeys fromText toolArgs))
                result <- executeFunction args
                pure
                    $ either
                        (ToolResponseText . ("Tool error: " <>) . toText)
                        identity
                        result
        pure (toolResult toolCallId toolResponse)
  where
    matchesTool requestedToolName ToolDef{name} = name == requestedToolName

handleToolLoop
    :: ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       , Rake :> es
       )
    => ProviderRoundRequest es
    -> [ToolDef es]
    -> ResponseFormat
    -> SamplingOptions
    -> (HistoryItem -> Eff es ())
    -> Int
    -> [HistoryItem]
    -> [HistoryItem]
    -> [HistoryItem]
    -> Int
    -> Eff es ChatOutcome
handleToolLoop requestProviderRound tools responseFormat samplingOptions onItem maxRounds checkpointHistory conversation accumulated completedRounds = do
    let fullHistory = conversation <> accumulated
    throwIfReplayBlocked fullHistory checkpointHistory
    let ReplayState{resumableToolCalls = pendingToolCalls} = conversationReplayState fullHistory
    resumedToolResults <- ensureHistoryItemIds =<< executeToolCalls tools pendingToolCalls
    traverse_ onItem resumedToolResults
    let accumulatedAfterResume = accumulated <> resumedToolResults
        fullHistoryAfterResume = conversation <> accumulatedAfterResume
    throwIfReplayBlocked fullHistoryAfterResume checkpointHistory

    ProviderRound
        { roundItems = roundHistoryItemsRaw
        , mediaReferences = roundMediaReferences
        , action = roundAction
        } <-
        requestProviderRound
            (toToolDeclaration <$> tools)
            responseFormat
            samplingOptions
            ( let ReplayState{replayHistory} = conversationReplayState fullHistoryAfterResume
               in replayHistory
            )
    saveMediaReferences roundMediaReferences
    roundHistoryItems <- ensureHistoryItemIds roundHistoryItemsRaw
    traverse_ onItem roundHistoryItems

    let accumulatedHistory = accumulatedAfterResume <> roundHistoryItems
        fullHistoryWithRound = conversation <> accumulatedHistory
        ReplayState
            { resumableToolCalls = replayDerivedRoundToolCalls
            , blocked = postRoundBlocked
            } = conversationReplayState fullHistoryWithRound

    case roundAction of
        ProviderRoundDone ->
            maybe
                (pure ChatFinished{appendedItems = accumulatedHistory})
                (emitFailedOutcome accumulatedHistory . FailureContract . replayBlockReasonText)
                postRoundBlocked
        ProviderRoundPaused pauseReason ->
            maybe
                ( pure
                    ChatPaused
                        { appendedItems = accumulatedHistory
                        , pauseReason
                        }
                )
                (emitFailedOutcome accumulatedHistory . FailureContract . replayBlockReasonText)
                postRoundBlocked
        ProviderRoundFailed failureReason ->
            emitFailedOutcome accumulatedHistory failureReason
        ProviderRoundNeedsLocalTools toolCalls ->
            case postRoundBlocked of
                Just blockedReason ->
                    emitFailedOutcome
                        accumulatedHistory
                        (FailureContract (replayBlockReasonText blockedReason))
                Nothing
                    | replayDerivedRoundToolCalls /= toolCalls ->
                        emitFailedOutcome
                            accumulatedHistory
                            (FailureContract toolCallProjectionMismatchReason)
                    | completedRounds >= maxRounds ->
                        pure
                            ChatPaused
                                { appendedItems = accumulatedHistory
                                , pauseReason = PauseToolLoopLimit maxRounds
                                }
                    | otherwise -> do
                        toolCallResults <-
                            ensureHistoryItemIds =<< executeToolCalls tools replayDerivedRoundToolCalls
                        traverse_ onItem toolCallResults
                        handleToolLoop
                            requestProviderRound
                            tools
                            responseFormat
                            samplingOptions
                            onItem
                            maxRounds
                            checkpointHistory
                            conversation
                            (accumulatedHistory <> toolCallResults)
                            (completedRounds + 1)
  where
    toToolDeclaration ToolDef{name, description, parameterSchema} =
        ToolDeclaration{name, description, parameterSchema}

    emitFailedOutcome historyItems failureReason = do
        replayBarrier <- ensureHistoryItemId (failureBarrier failureReason)
        onItem replayBarrier
        pure (ChatFailed{appendedItems = historyItems <> [replayBarrier], failureReason})

    toolCallProjectionMismatchReason =
        "Provider round toolCalls did not match the unresolved tool calls implied by appended round history"

flattenReplayHistory :: [HistoryItem] -> [HistoryItem]
flattenReplayHistory history =
    let ReplayState{replayHistory} = conversationReplayState history
     in replayHistory

effectiveSystemSnapshot :: [HistoryItem] -> Maybe HistoryItem
effectiveSystemSnapshot =
    viaNonEmpty last
        . filter isSystemSnapshotItem
        . flattenReplayHistory

renderableReplayHistory :: [HistoryItem] -> (Maybe HistoryItem, [HistoryItem])
renderableReplayHistory history =
    let replayHistory = flattenReplayHistory history
     in ( viaNonEmpty last (filter isSystemSnapshotItem replayHistory)
        , filter (not . isSystemSnapshotItem) replayHistory
        )

validResetCheckpoints :: [HistoryItem] -> [ResetCheckpoint]
validResetCheckpoints history =
    catMaybes
        $ [ guard (isStableReplayPrefix 0) $> ResetToStart
          ]
        <> [ ResetToItem <$> historyItemId lastItem
           | prefixLength <- [1 .. length activeItems]
           , isStableReplayPrefix prefixLength
           , let lastItem = activeItems List.!! (prefixLength - 1)
           ]
  where
    activeItems =
        let ReplayState{activeHistory = activeHistoryItems} = conversationReplayState history
         in activeHistoryItems

    isStableReplayPrefix prefixLength =
        case conversationReplayState (take prefixLength activeItems) of
            ReplayState{blocked = Nothing, pendingArtifacts, resumableToolCalls} ->
                null pendingArtifacts && null resumableToolCalls
            ReplayState{blocked = Just{}} ->
                False

latestValidCheckpoint :: [HistoryItem] -> Maybe ResetCheckpoint
latestValidCheckpoint =
    viaNonEmpty last . validResetCheckpoints

resetToLatestValidCheckpoint :: [HistoryItem] -> Maybe HistoryItem
resetToLatestValidCheckpoint history =
    case latestValidCheckpoint history of
        Nothing ->
            Nothing
        Just latestCheckpoint
            | currentReplayIsStable ->
                Nothing
            | otherwise ->
                Just (resetToCheckpoint latestCheckpoint)
  where
    currentReplayIsStable =
        case conversationReplayState history of
            ReplayState{blocked = Nothing, pendingArtifacts, resumableToolCalls} ->
                null pendingArtifacts && null resumableToolCalls
            ReplayState{blocked = Just{}} ->
                False

    resetToCheckpoint = \case
        ResetToStart ->
            resetToStart
        ResetToItem itemId ->
            resetTo itemId

assistantMessageTail :: [HistoryItem] -> [HistoryItem]
assistantMessageTail =
    reverse
        . takeWhile isAssistantMessage
        . dropWhile isCompletedNonPortable
        . List.reverse
        . flattenReplayHistory
  where
    isAssistantMessage = \case
        HistoryItem
            { itemLifecycle = ItemCompleted
            , genericItem = GenericMessage{role = GenericAssistant}
            } ->
            True
        _ ->
            False

    isCompletedNonPortable = \case
        HistoryItem{itemLifecycle = ItemCompleted, genericItem = GenericNonPortable{}} ->
            True
        _ ->
            False

genericItemTexts :: GenericItem -> [Text]
genericItemTexts = \case
    GenericMessage{parts} ->
        messagePartsText parts
    GenericToolCall{} ->
        []
    GenericToolResult{toolResult = ToolResult{toolResponse}} ->
        case toolResponse of
            ToolResponseText{text} ->
                [text]
            ToolResponseJson{} ->
                []
    GenericResetTo{} ->
        []
    GenericReplayBarrier{} ->
        []
    GenericNonPortable{} ->
        []

messagePartsText :: [MessagePart] -> [Text]
messagePartsText =
    mapMaybe partText
  where
    partText = \case
        PartText{text} ->
            Just text
        PartRefusal{text} ->
            Just text
        PartImage{} ->
            Nothing
        PartAudio{} ->
            Nothing
        PartFile{} ->
            Nothing

isSystemSnapshotItem :: HistoryItem -> Bool
isSystemSnapshotItem HistoryItem{genericItem = GenericMessage{role = GenericSystem}} =
    True
isSystemSnapshotItem _ =
    False

decodeAssistantMessageParts
    :: forall a
     . FromJSON a
    => String
    -> [MessagePart]
    -> Either RakeError a
decodeAssistantMessageParts label parts =
    case messagePartsText parts of
        [] ->
            Left (LlmExpectationError (label <> " contains no text content"))
        texts ->
            first LlmExpectationError (eitherDecodeStrictText (mconcat texts))

throwIfReplayBlocked
    :: Error RakeError :> es
    => [HistoryItem]
    -> [HistoryItem]
    -> Eff es ()
throwIfReplayBlocked history checkpointHistory =
    maybe (pure ()) throwError (replayBlockedConversation history checkpointHistory)

replayBlockedConversation :: [HistoryItem] -> [HistoryItem] -> Maybe RakeError
replayBlockedConversation history checkpointHistory =
    ConversationBlocked
        <$> replayBlockReason history
        <*> pure (blockedHistoryResetHint checkpointHistory)

blockedHistoryResetHint :: [HistoryItem] -> Maybe ResetCheckpoint
blockedHistoryResetHint history
    | any (isJust . historyItemId) history =
        latestValidCheckpoint history
    | otherwise =
        Nothing

replayBlockReason :: [HistoryItem] -> Maybe ReplayBlockReason
replayBlockReason history =
    let ReplayState{blocked = replayBlocked} = conversationReplayState history
     in replayBlocked

historyItemHasFailure :: HistoryItem -> Bool
historyItemHasFailure =
    isJust . historyItemFailureSignal

historyItemFailureSignal :: HistoryItem -> Maybe Text
historyItemFailureSignal HistoryItem{genericItem = GenericReplayBarrier{reason}} =
    Just reason
historyItemFailureSignal _ =
    Nothing

chatFailureReasonText :: ChatFailureReason -> Text
chatFailureReasonText = \case
    FailureProvider failureReason ->
        failureReason
    FailureContract failureReason ->
        failureReason

failureBarrier :: ChatFailureReason -> HistoryItem
failureBarrier failureReason =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem = GenericReplayBarrier{reason = chatFailureReasonText failureReason}
        , providerItem = Nothing
        }

historyItemToolCalls :: HistoryItem -> [ToolCall]
historyItemToolCalls HistoryItem{genericItem = GenericToolCall{toolCall = genericToolCall'}} =
    [genericToolCall']
historyItemToolCalls _ =
    []

duplicateUnresolvedToolCallReason :: ToolCallId -> Text
duplicateUnresolvedToolCallReason (ToolCallId toolCallIdText) =
    "Active conversation contains multiple unresolved tool calls with id "
        <> toolCallIdText
        <> ". Append resetTo to rewind before continuing."

firstDuplicateHistoryItemId :: [HistoryItem] -> Maybe HistoryItemId
firstDuplicateHistoryItemId =
    go mempty . mapMaybe historyItemId
  where
    go _ [] =
        Nothing
    go seenIds (itemId : remainingItemIds)
        | Set.member itemId seenIds =
            Just itemId
        | otherwise =
            go (Set.insert itemId seenIds) remainingItemIds

duplicateHistoryItemIdReason :: HistoryItemId -> Text
duplicateHistoryItemIdReason duplicateItemId =
    "Conversation contains duplicate HistoryItemId "
        <> show duplicateItemId
        <> ". HistoryItemId values must be unique within one conversation."

replayBlockReasonText :: ReplayBlockReason -> Text
replayBlockReasonText = \case
    ReplayInvalidReset checkpoint ->
        "Invalid reset checkpoint " <> replayResetCheckpointText checkpoint
    ReplayBlocked reason ->
        reason

replayResetCheckpointText :: ResetCheckpoint -> Text
replayResetCheckpointText = \case
    ResetToStart ->
        "start"
    ResetToItem itemId ->
        "item " <> show itemId

decrementItemCount :: Ord k => k -> Map.Map k Int -> Map.Map k Int
decrementItemCount key =
    Map.update
        ( \count ->
            if count <= 1
                then Nothing
                else Just (count - 1)
        )
        key

emptyToolMatchingState :: ToolMatchingState
emptyToolMatchingState =
    ToolMatchingState
        { unresolvedToolCallsById = mempty
        , unresolvedToolCallOrderRev = []
        , unresolvedToolCallCountsByItemIndex = mempty
        , resolvedToolCallItemIndexes = mempty
        , replayableToolResultIndexes = mempty
        , strayToolResultIndexes = mempty
        , duplicateToolCallBlock = Nothing
        }

withStorage
    :: RakeStorage :> es
    => ([HistoryItem] -> Eff es [HistoryItem])
    -> ConversationId
    -> Eff es [HistoryItem]
withStorage = withStorageBy identity

withResumableChat
    :: ( RakeStorage :> es
       , IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       , Rake :> es
       )
    => ChatConfig es
    -> ConversationId
    -> Eff es ChatOutcome
withResumableChat chatConfig =
    withStorageBy chatOutcomeAppendedItems (chatOutcome chatConfig)

withResumableStreamingChat
    :: ( RakeStorage :> es
       , IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       , Rake :> es
       )
    => StreamCallbacks es
    -> ChatConfig es
    -> ConversationId
    -> Eff es ChatOutcome
withResumableStreamingChat streamCallbacks chatConfig =
    withStorageBy chatOutcomeAppendedItems (streamChatOutcome streamCallbacks chatConfig)

withStorageBy
    :: RakeStorage :> es
    => (a -> [HistoryItem])
    -> ([HistoryItem] -> Eff es a)
    -> ConversationId
    -> Eff es a
withStorageBy extract action convId = do
    conversation <- getConversation convId
    result <- action conversation
    appendItems convId (stripConversationPrefix conversation (extract result))
    pure result
  where
    stripConversationPrefix conversationToStrip extractedHistory =
        fromMaybe extractedHistory (stripMatchingPrefix conversationToStrip extractedHistory)

    stripMatchingPrefix [] remainingHistory =
        Just remainingHistory
    stripMatchingPrefix (_ : _) [] =
        Nothing
    stripMatchingPrefix (expectedItem : remainingPrefix) (candidateItem : remainingHistory)
        | historyItemsMatchForPrefix expectedItem candidateItem =
            stripMatchingPrefix remainingPrefix remainingHistory
        | otherwise =
            Nothing

    historyItemsMatchForPrefix expectedItem candidateItem =
        setHistoryItemId Nothing expectedItem
            == setHistoryItemId Nothing candidateItem
            && idsCompatible
      where
        idsCompatible = case (historyItemId expectedItem, historyItemId candidateItem) of
            (Just expectedId, Just candidateId) ->
                expectedId == candidateId
            _ ->
                True
