{-# LANGUAGE RecordWildCards #-}

module LlmChat.Providers.Responses.Request
    ( ProviderTag (..)
    , ResponsesProviderConfig (..)
    , buildRequestBody
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as Vector
import Effectful
import LlmChat.Effect
import LlmChat.Internal.Schema (normalizeStructuredOutputSchema)
import LlmChat.Providers.Responses.Projection (historyItemToGenericItems)
import LlmChat.Types
import Relude

data ProviderTag
    = ProviderOpenAI
    | ProviderXAI
    deriving stock (Show, Eq)

data ResponsesProviderConfig es = ResponsesProviderConfig
    { providerTag :: ProviderTag
    , apiKey :: Text
    , baseUrl :: Text
    , model :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

buildRequestBody
    :: ResponsesProviderConfig es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildRequestBody ResponsesProviderConfig{..} tools responseFormat samplingOptions history = do
    input <- fmap concat $ traverse (renderHistoryItemForProvider providerTag requestLogger) history
    pure $
        object $
            [ "model" .= model
            , "input" .= input
            , "store" .= False
            ]
                <> samplingFields
                <> toolFields
                <> responseFormatFields
  where
    samplingFields =
        catMaybes
            [ ("temperature" .=) <$> temperature
            , ("top_p" .=) <$> topP
            ]

    SamplingOptions{temperature, topP} = samplingOptions

    toolFields
        | null tools = []
        | otherwise = ["tools" .= fmap toolDeclarationToValue tools]

    responseFormatFields =
        maybe [] (\formatValue -> ["text" .= object ["format" .= formatValue]]) $
            responseFormatToValue responseFormat

renderHistoryItemForProvider
    :: ProviderTag
    -> (NativeMsgFormat -> Eff es ())
    -> HistoryItem
    -> Eff es [Value]
renderHistoryItemForProvider providerTag requestLogger historyItem =
    case nativePayloadForProvider providerTag historyItem of
        Just payload ->
            pure [payload]
        Nothing -> do
            let (notes, genericItems) = historyItemToGenericItems historyItem
            traverse_ (requestLogger . NativeConversionNote) notes
            pure (concatMap genericItemToProviderInput genericItems)

nativePayloadForProvider :: ProviderTag -> HistoryItem -> Maybe Value
nativePayloadForProvider providerTag = \case
    HOpenAIResponses (OpenAIResponsesItem NativeResponseItem{payload})
        | providerTag == ProviderOpenAI ->
            Just payload
    HXAIResponses (XAIResponsesItem NativeResponseItem{payload})
        | providerTag == ProviderXAI ->
            Just payload
    _ ->
        Nothing

responseFormatToValue :: ResponseFormat -> Maybe Value
responseFormatToValue = \case
    Unstructured ->
        Nothing
    JsonValue ->
        Just $ object ["type" .= ("json_object" :: Text)]
    JsonSchema schema ->
        Just $
            object
                [ "type" .= ("json_schema" :: Text)
                , "name" .= ("response_format" :: Text)
                , "schema" .= schema
                ]

toolDeclarationToValue :: ToolDeclaration -> Value
toolDeclarationToValue ToolDeclaration{name, description, parameterSchema} =
    object $
        [ "type" .= ("function" :: Text)
        , "name" .= name
        , "description" .= description
        , "parameters" .= fromMaybe emptyToolParametersSchema parameterSchema
        ]

emptyToolParametersSchema :: Value
emptyToolParametersSchema =
    normalizeStructuredOutputSchema $
        object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]

genericItemToProviderInput :: GenericItem -> [Value]
genericItemToProviderInput = \case
    GenericMessage{role, parts} ->
        [messageValue (genericRoleToText role) (messagePartsValue role parts)]
    GenericToolCall{toolCall = genericToolCall'} ->
        [toolCallValue genericToolCall']
    GenericToolResult{toolResult = genericToolResult'} ->
        [toolResultValue genericToolResult']

messagePartsValue :: GenericRole -> [MessagePart] -> Value
messagePartsValue role = \case
    [] ->
        String ""
    [PartText{text}] ->
        String text
    parts ->
        Array . Vector.fromList $
            [ object
                [ "type" .= messageTextPartType role
                , "text" .= text
                ]
            | PartText{text} <- parts
            ]

messageTextPartType :: GenericRole -> Text
messageTextPartType = \case
    GenericAssistant ->
        "output_text"
    GenericSystem ->
        "input_text"
    GenericDeveloper ->
        "input_text"
    GenericUser ->
        "input_text"

messageValue :: Text -> Value -> Value
messageValue role content =
    object
        [ "role" .= role
        , "content" .= content
        ]

toolCallValue :: ToolCall -> Value
toolCallValue ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs} =
    object
        [ "type" .= ("function_call" :: Text)
        , "call_id" .= toolCallId
        , "name" .= toolName
        , "arguments" .= encodeObjectText toolArgs
        ]

toolResultValue :: ToolResult -> Value
toolResultValue ToolResult{toolCallId = ToolCallId toolCallId, toolResponse} =
    object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= toolCallId
        , "output" .= toolResponseWireOutput toolResponse
        ]

toolResponseWireOutput :: ToolResponse -> Text
toolResponseWireOutput = \case
    ToolResponseText{text} ->
        text
    ToolResponseJson{json} ->
        valueToCompactText json

genericRoleToText :: GenericRole -> Text
genericRoleToText = \case
    GenericSystem -> "system"
    GenericDeveloper -> "developer"
    GenericUser -> "user"
    GenericAssistant -> "assistant"

valueToCompactText :: Value -> Text
valueToCompactText =
    TL.toStrict . encodeToLazyText

encodeObjectText :: Map Text Value -> Text
encodeObjectText args =
    TL.toStrict $
        encodeToLazyText $
            Object (KM.fromMap (Map.mapKeys Key.fromText args))
