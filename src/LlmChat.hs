module LlmChat
    ( module X
    , chat
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
import Effectful
import Effectful.Error.Static
import LlmChat.Effect as X
import LlmChat.Providers.Responses.Internal (historyItemToGenericItems)
import LlmChat.Storage.Effect as X
import LlmChat.Tool as X
import LlmChat.Types as X
import Relude

chat
    :: ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es [HistoryItem]
chat ChatConfig{tools, responseFormat, onItem, maxToolRounds} conversation =
    handleToolLoop tools responseFormat onItem maxToolRounds conversation [] 0

itemTexts :: HistoryItem -> [Text]
itemTexts historyItem =
    concatMap genericItemTexts (snd (historyItemToGenericItems historyItem))

lastAssistantTexts :: [HistoryItem] -> [Text]
lastAssistantTexts history =
    fromMaybe [] $
        viaNonEmpty head
            [ messagePartsText parts
            | GenericMessage{role = GenericAssistant, parts} <- List.reverse (flattenGenericHistory history)
            ]

lastAssistantTextsStrict :: [HistoryItem] -> [Text]
lastAssistantTextsStrict history =
    concatMap genericItemTexts (assistantMessageTail history)

decodeLastAssistant
    :: forall a
     . FromJSON a
    => [HistoryItem]
    -> Either LlmChatError a
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
        | genericItem@GenericMessage{role = GenericAssistant} <- List.reverse (flattenGenericHistory history)
        ]

decodeLastAssistantStrict
    :: forall a
     . FromJSON a
    => [HistoryItem]
    -> Either LlmChatError a
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

collectToolCalls :: [HistoryItem] -> [ToolCall]
collectToolCalls historyItems =
    [ genericToolCall'
    | historyItem <- historyItems
    , GenericToolCall{toolCall = genericToolCall'} <- snd (historyItemToGenericItems historyItem)
    ]

handleToolLoop
    :: ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es]
    -> ResponseFormat
    -> (HistoryItem -> Eff es ())
    -> Int
    -> [HistoryItem]
    -> [HistoryItem]
    -> Int
    -> Eff es [HistoryItem]
handleToolLoop tools responseFormat onItem maxRounds conversation accumulated completedRounds = do
    response <- getLlmResponse (toToolDeclaration <$> tools) responseFormat (conversation <> accumulated)
    traverse_ onItem response

    let toolCalls = collectToolCalls response
    if null toolCalls
        then pure (accumulated <> response)
        else
            if completedRounds >= maxRounds
                then throwError (ToolLoopLimitExceeded maxRounds)
                else do
                    toolCallResults <- executeToolCalls tools toolCalls
                    traverse_ onItem toolCallResults
                    handleToolLoop
                        tools
                        responseFormat
                        onItem
                        maxRounds
                        conversation
                        (accumulated <> response <> toolCallResults)
                        (completedRounds + 1)
  where
    toToolDeclaration ToolDef{name, description, parameterSchema} =
        ToolDeclaration{name, description, parameterSchema}

flattenGenericHistory :: [HistoryItem] -> [GenericItem]
flattenGenericHistory =
    concatMap (snd . historyItemToGenericItems)

assistantMessageTail :: [HistoryItem] -> [GenericItem]
assistantMessageTail =
    reverse . takeWhile isAssistantMessage . List.reverse . flattenGenericHistory
  where
    isAssistantMessage = \case
        GenericMessage{role = GenericAssistant} ->
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
    -> Either LlmChatError a
decodeAssistantMessageParts label = \case
    parts@(PartText{} : _) ->
        first LlmExpectationError (eitherDecodeStrictText (mconcat (messagePartsText parts)))
    [] ->
        Left (LlmExpectationError (label <> " contains no text content"))

withStorage
    :: ( LlmChatStorage :> es
       )
    => ([HistoryItem] -> Eff es [HistoryItem])
    -> ConversationId
    -> Eff es [HistoryItem]
withStorage = withStorageBy identity

withStorageBy
    :: ( LlmChatStorage :> es
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
