module Rake
    ( module X
    , chat
    , chatOutcome
    , chatOutcomeItems
    , itemTexts
    , lastAssistantTexts
    , lastAssistantTextsStrict
    , decodeLastAssistant
    , decodeLastAssistantStrict
    , withStorage
    , withStorageBy
    ) where

import Data.Aeson (FromJSON, Value (Object), eitherDecodeStrictText)
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

chat
    :: ( Error RakeError :> es
       , Rake :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es [HistoryItem]
chat chatConfig conversation = do
    outcome <- chatOutcome chatConfig conversation
    case outcome of
        ChatFinished{historyItems} ->
            pure historyItems
        ChatPaused{pauseReason = PauseToolLoopLimit maxRounds} ->
            throwError (ToolLoopLimitExceeded maxRounds)
        ChatPaused{pauseReason} ->
            throwError (LlmExpectationError (toString (chatPauseReasonText pauseReason)))
        ChatFailed{failureReason = FailureProvider failureReason} ->
            throwError (ProviderTerminalFailure failureReason)
        ChatFailed{failureReason = FailureContract failureReason} ->
            throwError (LlmExpectationError (toString failureReason))

chatOutcome
    :: ( Error RakeError :> es
       , Rake :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es ChatOutcome
chatOutcome ChatConfig{tools, responseFormat, sampling = samplingOptions, onItem, maxToolRounds} conversation =
    handleToolLoop tools responseFormat samplingOptions onItem maxToolRounds conversation [] 0

chatOutcomeItems :: ChatOutcome -> [HistoryItem]
chatOutcomeItems = \case
    ChatFinished{historyItems} ->
        historyItems
    ChatPaused{historyItems} ->
        historyItems
    ChatFailed{historyItems} ->
        historyItems

itemTexts :: HistoryItem -> [Text]
itemTexts historyItem =
    concatMap genericItemTexts (snd (historyItemToGenericItems historyItem))

lastAssistantTexts :: [HistoryItem] -> [Text]
lastAssistantTexts history =
    fromMaybe [] $
        viaNonEmpty head
            [ messagePartsText parts
            | GenericMessage{role = GenericAssistant, parts, itemLifecycle = ItemCompleted} <- List.reverse (flattenGenericHistory history)
            ]

lastAssistantTextsStrict :: [HistoryItem] -> [Text]
lastAssistantTextsStrict history =
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
        | genericItem@GenericMessage{role = GenericAssistant, itemLifecycle = ItemCompleted} <- List.reverse (flattenGenericHistory history)
        ]

decodeLastAssistantStrict
    :: forall a
     . FromJSON a
    => [HistoryItem]
    -> Either RakeError a
decodeLastAssistantStrict history =
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
    :: ( Error RakeError :> es
       , Rake :> es
       )
    => [ToolDef es]
    -> ResponseFormat
    -> SamplingOptions
    -> (HistoryItem -> Eff es ())
    -> Int
    -> [HistoryItem]
    -> [HistoryItem]
    -> Int
    -> Eff es ChatOutcome
handleToolLoop tools responseFormat samplingOptions onItem maxRounds conversation accumulated completedRounds = do
    let fullHistory = conversation <> accumulated
    resumedToolResults <- executeToolCalls tools (unresolvedPendingToolCalls fullHistory)
    traverse_ onItem resumedToolResults
    let accumulatedAfterResume = accumulated <> resumedToolResults

    ProviderRound
        { historyItems = roundHistoryItems
        , action = roundAction
        } <-
        getLlmResponse (toToolDeclaration <$> tools) responseFormat samplingOptions (conversation <> accumulatedAfterResume)
    traverse_ onItem roundHistoryItems

    let accumulatedHistory = accumulatedAfterResume <> roundHistoryItems

    case roundAction of
        ProviderRoundDone ->
            pure ChatFinished{historyItems = accumulatedHistory}
        ProviderRoundPaused pauseReason ->
            pure
                ChatPaused
                    { historyItems = accumulatedHistory
                    , pauseReason
                    }
        ProviderRoundFailed failureReason ->
            pure
                ChatFailed
                    { historyItems = accumulatedHistory
                    , failureReason
                    }
        ProviderRoundNeedsLocalTools toolCalls ->
            if completedRounds >= maxRounds
                then
                    pure
                        ChatPaused
                            { historyItems = accumulatedHistory
                            , pauseReason = PauseToolLoopLimit maxRounds
                            }
                else do
                    toolCallResults <- executeToolCalls tools toolCalls
                    traverse_ onItem toolCallResults
                    handleToolLoop
                        tools
                        responseFormat
                        samplingOptions
                        onItem
                        maxRounds
                        conversation
                        (accumulatedHistory <> toolCallResults)
                        (completedRounds + 1)
  where
    toToolDeclaration ToolDef{name, description, parameterSchema} =
        ToolDeclaration{name, description, parameterSchema}

flattenGenericHistory :: [HistoryItem] -> [GenericItem]
flattenGenericHistory history =
    snd (historyItemsToGenericItems history)

unresolvedPendingToolCalls :: [HistoryItem] -> [ToolCall]
unresolvedPendingToolCalls history =
    fst $
        foldl'
            step
            ([], Set.empty)
            (List.reverse (flattenGenericHistory history))
  where
    step (pendingToolCalls, resolvedToolCallIds) = \case
        GenericToolResult{toolResult = ToolResult{toolCallId}} ->
            (pendingToolCalls, Set.insert toolCallId resolvedToolCallIds)
        GenericToolCall{toolCall = pendingToolCall@ToolCall{toolCallId}, itemLifecycle = ItemPending}
            | Set.notMember toolCallId resolvedToolCallIds ->
                (pendingToolCall : pendingToolCalls, resolvedToolCallIds)
        _ ->
            (pendingToolCalls, resolvedToolCallIds)

assistantMessageTail :: [HistoryItem] -> [GenericItem]
assistantMessageTail =
    reverse . takeWhile isAssistantMessage . List.reverse . flattenGenericHistory
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

chatPauseReasonText :: ChatPauseReason -> Text
chatPauseReasonText = \case
    PauseIncomplete reason ->
        reason
    PauseProviderWaiting reason ->
        reason
    PauseToolLoopLimit maxRounds ->
        "Tool loop limit exceeded after " <> show maxRounds <> " rounds"

withStorage
    :: ( RakeStorage :> es
       )
    => ([HistoryItem] -> Eff es [HistoryItem])
    -> ConversationId
    -> Eff es [HistoryItem]
withStorage = withStorageBy identity

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
        fromMaybe extractedHistory (List.stripPrefix conversationToStrip extractedHistory)
