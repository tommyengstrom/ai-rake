module Rake
    ( module X
    , chatOutcome
    , chatOutcomeAppendedItems
    , withResumableChat
    , conversationReplayState
    , validResetCheckpoints
    , latestValidCheckpoint
    , resetToLatestValidCheckpoint
    , itemTexts
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
import Rake.Providers.Chat.Projection (historyItemToGenericItems, historyItemsToGenericItems)
import Rake.Storage.Effect as X
import Rake.Tool as X
import Rake.Types as X
import Relude

data PendingToolCallState = PendingToolCallState
    { pendingToolCall :: ToolCall
    , pendingToolCallItemIndex :: Int
    , pendingGeminiThoughtReplayExchangeId :: Maybe Text
    }

data ToolMatchingState = ToolMatchingState
    { unresolvedToolCallsById :: Map.Map ToolCallId PendingToolCallState
    , unresolvedToolCallOrderRev :: [ToolCallId]
    , unresolvedToolCallCountsByItemIndex :: Map.Map Int Int
    , resolvedToolCallItemIndexes :: Set.Set Int
    , replayableToolResultIndexes :: Set.Set Int
    , strayToolResultIndexes :: Set.Set Int
    , geminiThoughtReplayExchangeIds :: Set.Set Text
    , duplicateToolCallBlock :: Maybe ReplayBlockReason
    }

chatOutcome
    :: ( IOE :> es
       , Error RakeError :> es
       , Rake :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es ChatOutcome
chatOutcome ChatConfig{tools, responseFormat, sampling = samplingOptions, onItem, maxToolRounds} conversation = do
    normalizedConversation <- ensureHistoryItemIds conversation
    handleToolLoop tools responseFormat samplingOptions onItem maxToolRounds conversation normalizedConversation [] 0

chatOutcomeAppendedItems :: ChatOutcome -> [HistoryItem]
chatOutcomeAppendedItems = \case
    ChatFinished{appendedItems} ->
        appendedItems
    ChatPaused{appendedItems} ->
        appendedItems
    ChatFailed{appendedItems} ->
        appendedItems

itemTexts :: HistoryItem -> [Text]
itemTexts historyItem =
    concatMap genericItemTexts (snd (historyItemToGenericItems historyItem))

conversationReplayState :: [HistoryItem] -> ReplayState
conversationReplayState history =
    ReplayState
        { activeHistory = activeItems
        , replayHistory = reverse replayableHistoryRev
        , resumableToolCalls = reverse resumableToolCallsRev
        , pendingArtifacts = reverse pendingArtifactsRev
        , blocked = duplicateHistoryItemIdBlock <|> resetBlock <|> failureBlock <|> duplicateToolCallBlock
        }
  where
    duplicateHistoryItemIdBlock =
        ReplayBlocked . duplicateHistoryItemIdReason
            <$> firstDuplicateHistoryItemId history

    (activeItems, resetBlock) = foldl' applyControlItem ([], Nothing) history

    failureBlock =
        ReplayBlocked <$> viaNonEmpty head [reason | Just reason <- map historyItemFailureSignal activeItems]

    ToolMatchingState
        { unresolvedToolCallsById = matchingUnresolvedToolCallsById
        , unresolvedToolCallOrderRev = matchingUnresolvedToolCallOrderRev
        , unresolvedToolCallCountsByItemIndex = matchingUnresolvedToolCallCountsByItemIndex
        , resolvedToolCallItemIndexes = matchingResolvedToolCallItemIndexes
        , replayableToolResultIndexes = matchingReplayableToolResultIndexes
        , strayToolResultIndexes = matchingStrayToolResultIndexes
        , geminiThoughtReplayExchangeIds = matchingGeminiThoughtReplayExchangeIds
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
        , Just PendingToolCallState{pendingToolCall} <- [Map.lookup toolCallId matchingUnresolvedToolCallsById]
        ]

    (replayableHistoryRev, pendingArtifactsRev) =
        foldl'
            classifyReplayItem
            ([], [])
            (zip [0 ..] activeItems)

    applyControlItem (currentActiveHistory, currentBlocked) historyItem =
        case historyItem of
            HControl (ResetTo checkpoint) ->
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
         in
            case historyItem of
                HLocal LocalToolResult{toolResult = ToolResult{toolCallId}} ->
                    resolveToolResult itemIndex toolCallId stateAfterToolCalls
                _ ->
                    stateAfterToolCalls

    registerPendingToolCall
        itemIndex
        historyItem
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
                                , pendingGeminiThoughtReplayExchangeId =
                                    historyItemGeminiPendingFunctionCallExchangeId historyItem
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
            , geminiThoughtReplayExchangeIds = stateGeminiThoughtReplayExchangeIds
            } =
        case Map.lookup toolCallId stateUnresolvedToolCallsById of
            Nothing ->
                matchingState
                    { strayToolResultIndexes =
                        Set.insert itemIndex stateStrayToolResultIndexes
                    }
            Just PendingToolCallState{pendingToolCallItemIndex, pendingGeminiThoughtReplayExchangeId} ->
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
                    geminiThoughtReplayExchangeIds' =
                        case (pendingGeminiThoughtReplayExchangeId, toolCallItemFullyResolved) of
                            (Just exchangeId, True) ->
                                Set.insert exchangeId stateGeminiThoughtReplayExchangeIds
                            _ ->
                                stateGeminiThoughtReplayExchangeIds
                 in
                    matchingState
                        { unresolvedToolCallsById = unresolvedToolCallsById'
                        , unresolvedToolCallCountsByItemIndex = unresolvedToolCallCountsByItemIndex'
                        , resolvedToolCallItemIndexes = resolvedToolCallItemIndexes'
                        , replayableToolResultIndexes =
                            Set.insert itemIndex stateReplayableToolResultIndexes
                        , geminiThoughtReplayExchangeIds = geminiThoughtReplayExchangeIds'
                        }

    classifyReplayItem (replayItems, pendingItems) (itemIndex, historyItem) =
        case historyItem of
            HControl{} ->
                (replayItems, pendingItems)
            HLocal localItem ->
                case localItem of
                    LocalToolResult{}
                        | Set.member itemIndex matchingReplayableToolResultIndexes ->
                            (historyItem : replayItems, pendingItems)
                        | Set.member itemIndex matchingStrayToolResultIndexes ->
                            (replayItems, pendingItems)
                        | otherwise ->
                            (replayItems, pendingItems)
                    _
                        | Set.member itemIndex unresolvedToolCallItemIndexes ->
                            (replayItems, historyItem : pendingItems)
                        | hasReplayableToolCalls itemIndex historyItem ->
                            (historyItem : replayItems, pendingItems)
                        | not (null (historyItemToolCalls historyItem)) ->
                            (replayItems, historyItem : pendingItems)
                        | otherwise ->
                            (historyItem : replayItems, pendingItems)
            HProvider ProviderHistoryItem{itemLifecycle = ItemCompleted} ->
                if historyItemHasFailure historyItem
                    then
                        (replayItems, historyItem : pendingItems)
                    else if Set.member itemIndex unresolvedToolCallItemIndexes
                        then
                            (replayItems, historyItem : pendingItems)
                    else if hasReplayableToolCalls itemIndex historyItem
                        then
                            (historyItem : replayItems, pendingItems)
                    else if not (null (historyItemToolCalls historyItem))
                        then
                            (replayItems, historyItem : pendingItems)
                    else
                        (historyItem : replayItems, pendingItems)
            HProvider ProviderHistoryItem{itemLifecycle = ItemPending} ->
                if historyItemHasFailure historyItem
                    then
                        (replayItems, historyItem : pendingItems)
                    else if historyItemShouldReplayGeminiThought matchingGeminiThoughtReplayExchangeIds historyItem
                        then
                            (historyItem : replayItems, pendingItems)
                    else if Set.member itemIndex unresolvedToolCallItemIndexes
                        then
                            (replayItems, historyItem : pendingItems)
                    else if hasReplayableToolCalls itemIndex historyItem
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
    fromMaybe [] $
        viaNonEmpty head
            [ messagePartsText parts
            | GenericMessage{role = GenericAssistant, parts, itemLifecycle = ItemCompleted} <- List.reverse (flattenReplayHistory history)
            ]

lastAssistantTextsStrict :: [HistoryItem] -> [Text]
lastAssistantTextsStrict history =
    case replayBlockReason history of
        Just{} ->
            []
        Nothing ->
            concatMap genericItemTexts (assistantMessageTail history)

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
        [ genericItem
        | genericItem@GenericMessage{role = GenericAssistant, itemLifecycle = ItemCompleted} <- List.reverse (flattenReplayHistory history)
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
                [GenericMessage{parts}] ->
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
                pure $
                    either
                        (ToolResponseText . ("Tool error: " <>) . toText)
                        identity
                        result
        pure (toolResult toolCallId toolResponse)
  where
    matchesTool requestedToolName ToolDef{name} = name == requestedToolName

handleToolLoop
    :: ( IOE :> es
       , Error RakeError :> es
       , Rake :> es
       )
    => [ToolDef es]
    -> ResponseFormat
    -> SamplingOptions
    -> (HistoryItem -> Eff es ())
    -> Int
    -> [HistoryItem]
    -> [HistoryItem]
    -> [HistoryItem]
    -> Int
    -> Eff es ChatOutcome
handleToolLoop tools responseFormat samplingOptions onItem maxRounds checkpointHistory conversation accumulated completedRounds = do
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
        , action = roundAction
        } <-
        getLlmResponse
            (toToolDeclaration <$> tools)
            responseFormat
            samplingOptions
            (let ReplayState{replayHistory} = conversationReplayState fullHistoryAfterResume in replayHistory)
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
                    emitFailedOutcome accumulatedHistory (FailureContract (replayBlockReasonText blockedReason))
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
                        toolCallResults <- ensureHistoryItemIds =<< executeToolCalls tools replayDerivedRoundToolCalls
                        traverse_ onItem toolCallResults
                        handleToolLoop
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

flattenReplayHistory :: [HistoryItem] -> [GenericItem]
flattenReplayHistory history =
    let ReplayState{replayHistory} = conversationReplayState history
     in snd (historyItemsToGenericItems replayHistory)

validResetCheckpoints :: [HistoryItem] -> [ResetCheckpoint]
validResetCheckpoints history =
    catMaybes $
        [ guard (isStableReplayPrefix 0) $> ResetToStart
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
                Just (HControl (ResetTo latestCheckpoint))
  where
    currentReplayIsStable =
        case conversationReplayState history of
            ReplayState{blocked = Nothing, pendingArtifacts, resumableToolCalls} ->
                null pendingArtifacts && null resumableToolCalls
            ReplayState{blocked = Just{}} ->
                False

assistantMessageTail :: [HistoryItem] -> [GenericItem]
assistantMessageTail =
    reverse . takeWhile isAssistantMessage . List.reverse . flattenReplayHistory
  where
    isAssistantMessage = \case
        GenericMessage{role = GenericAssistant, itemLifecycle = ItemCompleted} ->
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

messagePartsText :: [MessagePart] -> [Text]
messagePartsText =
    mapMaybe partText
  where
    partText = \case
        PartText{text} ->
            Just text

decodeAssistantMessageParts
    :: forall a
     . FromJSON a
    => String
    -> [MessagePart]
    -> Either RakeError a
decodeAssistantMessageParts label = \case
    parts@(PartText{} : _) ->
        first LlmExpectationError (eitherDecodeStrictText (mconcat (messagePartsText parts)))
    [] ->
        Left (LlmExpectationError (label <> " contains no text content"))

throwIfReplayBlocked
    :: Error RakeError :> es
    => [HistoryItem]
    -> [HistoryItem]
    -> Eff es ()
throwIfReplayBlocked history checkpointHistory =
    maybe (pure ()) throwError (replayBlockedConversation history checkpointHistory)

replayBlockedConversation :: [HistoryItem] -> [HistoryItem] -> Maybe RakeError
replayBlockedConversation history checkpointHistory =
    ConversationBlocked <$> replayBlockReason history <*> pure (blockedHistoryResetHint checkpointHistory)

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
historyItemFailureSignal = \case
    HControl (ReplayBarrier reason) ->
        Just reason
    HProvider ProviderHistoryItem{apiFamily, nativeItem = NativeProviderItem{payload}} ->
        case apiFamily of
            ProviderOpenAIResponses ->
                responsesPayloadFailureSignal payload
            ProviderXAIResponses ->
                responsesPayloadFailureSignal payload
            ProviderGeminiInteractions ->
                Nothing
    _ ->
        Nothing

chatFailureReasonText :: ChatFailureReason -> Text
chatFailureReasonText = \case
    FailureProvider failureReason ->
        failureReason
    FailureContract failureReason ->
        failureReason

failureBarrier :: ChatFailureReason -> HistoryItem
failureBarrier =
    HControl . ReplayBarrier . chatFailureReasonText

responsesPayloadFailureSignal :: Value -> Maybe Text
responsesPayloadFailureSignal = \case
    Object payloadObject ->
        case KM.lookup "status" payloadObject of
            Just (String "failed") ->
                Just "Responses output item status was failed. Append resetTo to rewind before continuing."
            Just (String "cancelled") ->
                Just "Responses output item status was cancelled. Append resetTo to rewind before continuing."
            Just (String "canceled") ->
                Just "Responses output item status was canceled. Append resetTo to rewind before continuing."
            Just (String "expired") ->
                Just "Responses output item status was expired. Append resetTo to rewind before continuing."
            _ ->
                Nothing
    _ ->
        Nothing

historyItemToolCalls :: HistoryItem -> [ToolCall]
historyItemToolCalls historyItem =
    [ genericToolCall'
    | GenericToolCall{toolCall = genericToolCall'} <- snd (historyItemToGenericItems historyItem)
    ]

historyItemGeminiPendingFunctionCallExchangeId :: HistoryItem -> Maybe Text
historyItemGeminiPendingFunctionCallExchangeId historyItem = case historyItem of
    HProvider ProviderHistoryItem
        { apiFamily = ProviderGeminiInteractions
        , itemLifecycle = ItemPending
        , nativeItem = NativeProviderItem{exchangeId = Just providerExchangeId, payload}
        }
            | geminiPayloadType payload == Just "function_call" ->
                Just providerExchangeId
    _ ->
        Nothing

historyItemShouldReplayGeminiThought :: Set.Set Text -> HistoryItem -> Bool
historyItemShouldReplayGeminiThought replayExchangeIds = \case
    HProvider ProviderHistoryItem
        { apiFamily = ProviderGeminiInteractions
        , itemLifecycle = ItemPending
        , nativeItem = NativeProviderItem{exchangeId = Just providerExchangeId, payload}
        } ->
            Set.member providerExchangeId replayExchangeIds
                && geminiPayloadType payload == Just "thought"
    _ ->
        False

geminiPayloadType :: Value -> Maybe Text
geminiPayloadType = \case
    Object payloadObject ->
        KM.lookup "type" payloadObject >>= \case
            String payloadType ->
                Just payloadType
            _ ->
                Nothing
    _ ->
        Nothing

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
        , geminiThoughtReplayExchangeIds = mempty
        , duplicateToolCallBlock = Nothing
        }

withStorage
    :: ( RakeStorage :> es
       )
    => ([HistoryItem] -> Eff es [HistoryItem])
    -> ConversationId
    -> Eff es [HistoryItem]
withStorage = withStorageBy identity

withResumableChat
    :: ( RakeStorage :> es
       , IOE :> es
       , Error RakeError :> es
       , Rake :> es
       )
    => ChatConfig es
    -> ConversationId
    -> Eff es ChatOutcome
withResumableChat chatConfig =
    withStorageBy chatOutcomeAppendedItems (chatOutcome chatConfig)

withStorageBy
    :: ( RakeStorage :> es
       )
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
        setHistoryItemId Nothing expectedItem == setHistoryItemId Nothing candidateItem
            && idsCompatible
      where
        idsCompatible = case (historyItemId expectedItem, historyItemId candidateItem) of
            (Just expectedId, Just candidateId) ->
                expectedId == candidateId
            _ ->
                True
