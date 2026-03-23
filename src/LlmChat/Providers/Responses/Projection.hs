module LlmChat.Providers.Responses.Projection
    ( historyItemToGenericItems
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Vector qualified as Vector
import LlmChat.Types
import Relude

historyItemToGenericItems :: HistoryItem -> ([Text], [GenericItem])
historyItemToGenericItems = \case
    HLocal localItem ->
        ([], [localItemToGenericItem localItem])
    HOpenAIResponses (OpenAIResponsesItem NativeResponseItem{payload}) ->
        providerPayloadToGenericItems payload
    HXAIResponses (XAIResponsesItem NativeResponseItem{payload}) ->
        providerPayloadToGenericItems payload

localItemToGenericItem :: LocalItem -> GenericItem
localItemToGenericItem = \case
    LocalMessage{role, parts} ->
        GenericMessage{role, parts}
    LocalToolCall{toolCall = localToolCall} ->
        GenericToolCall localToolCall
    LocalToolResult{toolResult = localToolResult} ->
        GenericToolResult localToolResult

providerPayloadToGenericItems :: Value -> ([Text], [GenericItem])
providerPayloadToGenericItems payload =
    case payload of
        Object itemObject ->
            case (lookupText "type" itemObject, lookupText "role" itemObject) of
                (Just "function_call", _) ->
                    case parseToolCall itemObject of
                        Nothing ->
                            (["Dropped malformed function_call item"], [])
                        Just genericToolCall' ->
                            ([], [GenericToolCall genericToolCall'])
                (Just "function_call_output", _) ->
                    case parseToolResult itemObject of
                        Nothing ->
                            (["Dropped malformed function_call_output item"], [])
                        Just genericToolResult' ->
                            ([], [GenericToolResult genericToolResult'])
                (Just "message", _) ->
                    messageObjectToGeneric itemObject
                (Nothing, Just _) ->
                    messageObjectToGeneric itemObject
                (Just unsupportedType, _) ->
                    (["Dropped unsupported native response item type: " <> unsupportedType], [])
                _ ->
                    (["Dropped malformed native response item"], [])
        _ ->
            (["Dropped non-object native response item"], [])

messageObjectToGeneric :: Object -> ([Text], [GenericItem])
messageObjectToGeneric itemObject =
    case roleTextToGenericRole =<< lookupText "role" itemObject of
        Nothing ->
            (["Dropped message item with unsupported role"], [])
        Just role ->
            case extractContentParts (KM.lookup "content" itemObject) of
                (notes, []) ->
                    (notes <> ["Dropped message item without supported content parts"], [])
                (notes, parts) ->
                    (notes, [GenericMessage{role, parts}])

extractContentParts :: Maybe Value -> ([Text], [MessagePart])
extractContentParts = \case
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
            messagePartObjectToGeneric partObject
        _ ->
            (["Dropped unsupported message part"], [])

messagePartObjectToGeneric :: Object -> ([Text], [MessagePart])
messagePartObjectToGeneric partObject =
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

parseToolCall :: Object -> Maybe ToolCall
parseToolCall itemObject = do
    ToolCallId toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    toolName <- lookupText "name" itemObject
    toolArgs <- parseToolArgs =<< KM.lookup "arguments" itemObject
    pure ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs}

parseToolResult :: Object -> Maybe ToolResult
parseToolResult itemObject = do
    toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    outputValue <- KM.lookup "output" itemObject
    pure ToolResult{toolCallId, toolResponse = parseToolResponse outputValue}

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
