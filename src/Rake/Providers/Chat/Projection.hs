module Rake.Providers.Chat.Projection
    ( historyItemToGenericItems
    , historyItemsToGenericItems
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Vector qualified as Vector
import Rake.Types
import Relude

historyItemToGenericItems :: HistoryItem -> ([Text], [GenericItem])
historyItemToGenericItems = \case
    HLocal localItem ->
        ([], [localItemToGenericItem localItem])
    HProvider ProviderHistoryItem{apiFamily, itemLifecycle, nativeItem = NativeProviderItem{payload}} ->
        case apiFamily of
            ProviderOpenAIResponses ->
                responsesPayloadToGenericItems itemLifecycle payload
            ProviderXAIResponses ->
                responsesPayloadToGenericItems itemLifecycle payload
            ProviderGeminiInteractions ->
                geminiPayloadToGenericItems itemLifecycle payload
    HControl{} ->
        ([], [])

historyItemsToGenericItems :: [HistoryItem] -> ([Text], [GenericItem])
historyItemsToGenericItems =
    foldMap historyItemToGenericItems

localItemToGenericItem :: LocalItem -> GenericItem
localItemToGenericItem = \case
    LocalMessage{role, parts} ->
        GenericMessage{itemLifecycle = ItemCompleted, role, parts}
    LocalToolCall{toolCall = localToolCall} ->
        GenericToolCall{toolCall = localToolCall, itemLifecycle = ItemCompleted}
    LocalToolResult{toolResult = localToolResult} ->
        GenericToolResult{toolResult = localToolResult, itemLifecycle = ItemCompleted}

responsesPayloadToGenericItems :: ItemLifecycle -> Value -> ([Text], [GenericItem])
responsesPayloadToGenericItems itemLifecycle payload =
    case payload of
        Object itemObject ->
            case (lookupText "type" itemObject, lookupText "role" itemObject) of
                (Just "function_call", _) ->
                    case parseResponsesToolCall itemObject of
                        Nothing ->
                            (["Dropped malformed function_call item"], [])
                        Just genericToolCall' ->
                            ([], [GenericToolCall{toolCall = genericToolCall', itemLifecycle}])
                (Just "function_call_output", _) ->
                    case parseResponsesToolResult itemObject of
                        Nothing ->
                            (["Dropped malformed function_call_output item"], [])
                        Just genericToolResult' ->
                            ([], [GenericToolResult{toolResult = genericToolResult', itemLifecycle}])
                (Just "message", _) ->
                    responsesMessageObjectToGeneric itemLifecycle itemObject
                (Nothing, Just _) ->
                    responsesMessageObjectToGeneric itemLifecycle itemObject
                (Just unsupportedType, _) ->
                    (["Dropped unsupported native response item type: " <> unsupportedType], [])
                _ ->
                    (["Dropped malformed native response item"], [])
        _ ->
            (["Dropped non-object native response item"], [])

responsesMessageObjectToGeneric :: ItemLifecycle -> Object -> ([Text], [GenericItem])
responsesMessageObjectToGeneric itemLifecycle itemObject =
    case roleTextToGenericRole =<< lookupText "role" itemObject of
        Nothing ->
            (["Dropped message item with unsupported role"], [])
        Just role ->
            case extractResponsesContentParts (KM.lookup "content" itemObject) of
                (notes, []) ->
                    (notes <> ["Dropped message item without supported content parts"], [])
                (notes, parts) ->
                    (notes, [GenericMessage{itemLifecycle, role, parts}])

extractResponsesContentParts :: Maybe Value -> ([Text], [MessagePart])
extractResponsesContentParts = \case
    Nothing ->
        (["Missing content field"], [])
    Just (String content) ->
        ([], [PartText content])
    Just (Array contentParts) ->
        foldMap partToGeneric (Vector.toList contentParts)
    Just _ ->
        (["Unsupported message content format"], [])
  where
    partToGeneric :: Value -> ([Text], [MessagePart])
    partToGeneric = \case
        String content ->
            ([], [PartText content])
        Object partObject ->
            responsesMessagePartObjectToGeneric partObject
        _ ->
            (["Dropped unsupported message part"], [])

responsesMessagePartObjectToGeneric :: Object -> ([Text], [MessagePart])
responsesMessagePartObjectToGeneric partObject =
    case lookupText "type" partObject of
        Just "input_text" ->
            parseTextPart
        Just "output_text" ->
            parseTextPart
        Just "text" ->
            parseTextPart
        Just unsupportedType ->
            (["Dropped unsupported message part type: " <> unsupportedType], [])
        Nothing ->
            case lookupText "text" partObject of
                Just text ->
                    ([], [PartText text])
                Nothing ->
                    (["Dropped unsupported message part"], [])
  where
    parseTextPart =
        case lookupText "text" partObject of
            Just text ->
                ([], [PartText text])
            Nothing ->
                (["Dropped malformed text message part"], [])

parseResponsesToolCall :: Object -> Maybe ToolCall
parseResponsesToolCall itemObject = do
    toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    toolName <- lookupText "name" itemObject
    toolArgs <- parseToolArgs =<< KM.lookup "arguments" itemObject
    pure ToolCall{toolCallId, toolName, toolArgs}

parseResponsesToolResult :: Object -> Maybe ToolResult
parseResponsesToolResult itemObject = do
    toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    outputValue <- KM.lookup "output" itemObject
    pure ToolResult{toolCallId, toolResponse = parseToolResponse outputValue}

geminiPayloadToGenericItems :: ItemLifecycle -> Value -> ([Text], [GenericItem])
geminiPayloadToGenericItems itemLifecycle = \case
    Object itemObject ->
        case lookupText "type" itemObject of
            Just "text" ->
                case lookupText "text" itemObject of
                    Just text ->
                        ([], [GenericMessage{itemLifecycle, role = GenericAssistant, parts = [PartText text]}])
                    Nothing ->
                        (["Dropped malformed Gemini text content"], [])
            Just "function_call" ->
                case parseGeminiToolCall itemObject of
                    Just genericToolCall' ->
                        ([], [GenericToolCall{toolCall = genericToolCall', itemLifecycle}])
                    Nothing ->
                        (["Dropped malformed Gemini function_call content"], [])
            Just "function_result" ->
                case parseGeminiToolResult itemObject of
                    Just genericToolResult' ->
                        ([], [GenericToolResult{toolResult = genericToolResult', itemLifecycle}])
                    Nothing ->
                        (["Dropped malformed Gemini function_result content"], [])
            Just "thought" ->
                (["Dropped Gemini thought content during generic projection"], [])
            Just unsupportedType ->
                (["Dropped unsupported Gemini content type: " <> unsupportedType], [])
            Nothing ->
                case lookupText "text" itemObject of
                    Just text ->
                        ([], [GenericMessage{itemLifecycle, role = GenericAssistant, parts = [PartText text]}])
                    Nothing ->
                        (["Dropped malformed Gemini native content"], [])
    _ ->
        (["Dropped non-object Gemini native content"], [])

parseGeminiToolCall :: Object -> Maybe ToolCall
parseGeminiToolCall itemObject = do
    toolCallId <-
        ToolCallId
            <$> ( lookupText "id" itemObject
                    <|> lookupText "call_id" itemObject
                )
    toolName <- lookupText "name" itemObject
    toolArgs <- parseToolArgs =<< KM.lookup "arguments" itemObject
    pure ToolCall{toolCallId, toolName, toolArgs}

parseGeminiToolResult :: Object -> Maybe ToolResult
parseGeminiToolResult itemObject = do
    toolCallId <-
        ToolCallId
            <$> ( lookupText "call_id" itemObject
                    <|> lookupText "id" itemObject
                )
    resultValue <- KM.lookup "result" itemObject
    pure ToolResult{toolCallId, toolResponse = parseGeminiToolResponse resultValue}

parseGeminiToolResponse :: Value -> ToolResponse
parseGeminiToolResponse = \case
    Array contentParts ->
        case traverse geminiTextContent (Vector.toList contentParts) of
            Just texts ->
                parseToolResponse (String (mconcat texts))
            Nothing ->
                ToolResponseJson (Array contentParts)
    other ->
        parseToolResponse other
  where
    geminiTextContent = \case
        Object partObject ->
            case lookupText "type" partObject of
                Just "text" ->
                    lookupText "text" partObject
                Nothing ->
                    lookupText "text" partObject
                _ ->
                    Nothing
        _ ->
            Nothing

parseToolResponse :: Value -> ToolResponse
parseToolResponse = \case
    String outputText ->
        case eitherDecodeStrictText outputText of
            Right parsedJson ->
                ToolResponseJson parsedJson
            Left{} ->
                ToolResponseText outputText
    other ->
        ToolResponseJson other

parseToolArgs :: Value -> Maybe (Map Text Value)
parseToolArgs = \case
    String argumentsText ->
        case eitherDecodeStrictText argumentsText of
            Right (Object objectValue) ->
                Just (Map.mapKeys Key.toText (KM.toMap objectValue))
            _ ->
                Nothing
    Object objectValue ->
        Just (Map.mapKeys Key.toText (KM.toMap objectValue))
    _ ->
        Nothing

roleTextToGenericRole :: Text -> Maybe GenericRole
roleTextToGenericRole = \case
    "system" -> Just GenericSystem
    "developer" -> Just GenericDeveloper
    "user" -> Just GenericUser
    "assistant" -> Just GenericAssistant
    _ -> Nothing

lookupText :: Key.Key -> Object -> Maybe Text
lookupText key objectValue = KM.lookup key objectValue >>= \case
    String text ->
        Just text
    _ ->
        Nothing
