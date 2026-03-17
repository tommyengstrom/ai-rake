{-# LANGUAGE RecordWildCards #-}

module LlmChat.Providers.Responses.Internal
    ( ProviderTag (..)
    , ResponsesProviderConfig (..)
    , defaultWarningLogger
    , historyItemToGenericItems
    , runResponsesProvider
    ) where

import Debug.Trace qualified as DebugTrace
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as Vector
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Internal.Schema (closeOpenObjectSchemas)
import LlmChat.Types
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data ProviderTag
    = ProviderOpenAI
    | ProviderXAI
    deriving stock (Show, Eq)

data OpenAIProvider = OpenAIProvider

data XAIProvider = XAIProvider

class ResponsesProvider provider where
    providerTagValue :: provider -> ProviderTag
    wrapNativeHistoryItem :: provider -> NativeResponseItem -> HistoryItem
    genericItemToProviderInput :: provider -> GenericItem -> [Value]
    genericItemToProviderInput _ = genericItemToProviderInputShared
    providerPayloadToGenericItems :: provider -> Value -> ([Text], [GenericItem])
    providerPayloadToGenericItems _ = providerPayloadToGenericItemsShared

instance ResponsesProvider OpenAIProvider where
    providerTagValue _ = ProviderOpenAI
    wrapNativeHistoryItem _ = HOpenAIResponses . OpenAIResponsesItem

instance ResponsesProvider XAIProvider where
    providerTagValue _ = ProviderXAI
    wrapNativeHistoryItem _ = HXAIResponses . XAIResponsesItem

data ResponsesProviderConfig es = ResponsesProviderConfig
    { providerTag :: ProviderTag
    , apiKey :: Text
    , baseUrl :: Text
    , model :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

type ResponsesAPI =
    "v1"
        Servant.:> "responses"
        Servant.:> Header "Authorization" Text
        Servant.:> Header "OpenAI-Organization" Text
        Servant.:> Header "OpenAI-Project" Text
        Servant.:> ReqBody '[JSON] Value
        Servant.:> Post '[JSON] Value

responsesApi :: Proxy ResponsesAPI
responsesApi = Proxy

runResponsesProvider
    :: forall es a
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => ResponsesProviderConfig es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runResponsesProvider config@ResponsesProviderConfig{..} eff = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postResponse = client responsesApi

    interpretWith eff \_ -> \case
        GetLlmResponse tools responseFormat samplingOptions history -> do
            requestBody <- buildRequestBody config tools responseFormat samplingOptions history
            requestLogger (NativeMsgOut requestBody)
            responseValue <-
                liftIO
                    ( runClientM
                        ( postResponse
                            (Just ("Bearer " <> apiKey))
                            organizationId
                            projectId
                            requestBody
                        )
                        clientEnv
                    )
                    >>= \case
                        Left err -> do
                            requestLogger (NativeRequestFailure err)
                            throwError (LlmClientError err)
                        Right response -> pure response
            requestLogger (NativeMsgIn responseValue)
            either throwError pure $ decodeResponse providerTag responseValue
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildRequestBody
    :: ResponsesProviderConfig es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildRequestBody ResponsesProviderConfig{..} tools responseFormat samplingOptions history = do
    input <- case providerTag of
        ProviderOpenAI ->
            fmap concat $ traverse (renderHistoryItemWithProvider OpenAIProvider requestLogger) history
        ProviderXAI ->
            fmap concat $ traverse (renderHistoryItemWithProvider XAIProvider requestLogger) history
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

defaultWarningLogger :: Applicative f => Text -> NativeMsgFormat -> f ()
defaultWarningLogger providerName = \case
    NativeConversionNote note ->
        logWarning (warningPrefix <> toString note)
    NativeRequestFailure err ->
        logWarning (warningPrefix <> "Provider request failed: " <> show err)
    NativeMsgOut{} ->
        pure ()
    NativeMsgIn{} ->
        pure ()
  where
    warningPrefix = "[llmchat-effectful:" <> toString providerName <> "] "
    logWarning warningMessage =
        DebugTrace.trace warningMessage () `seq` pure ()

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
    closeOpenObjectSchemas $
        object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]

renderHistoryItemWithProvider
    :: ResponsesProvider provider
    => provider
    -> (NativeMsgFormat -> Eff es ())
    -> HistoryItem
    -> Eff es [Value]
renderHistoryItemWithProvider provider requestLogger = \case
    HLocal localItem ->
        pure (genericItemToProviderInput provider (localItemToGenericItem localItem))
    HOpenAIResponses (OpenAIResponsesItem NativeResponseItem{payload})
        | providerTagValue provider == ProviderOpenAI ->
            pure [payload]
        | otherwise -> do
            let (notes, genericItems) = providerPayloadToGenericItems OpenAIProvider payload
            traverse_ (requestLogger . NativeConversionNote) notes
            pure (concatMap (genericItemToProviderInput provider) genericItems)
    HXAIResponses (XAIResponsesItem NativeResponseItem{payload})
        | providerTagValue provider == ProviderXAI ->
            pure [payload]
        | otherwise -> do
            let (notes, genericItems) = providerPayloadToGenericItems XAIProvider payload
            traverse_ (requestLogger . NativeConversionNote) notes
            pure (concatMap (genericItemToProviderInput provider) genericItems)

genericItemToProviderInputShared :: GenericItem -> [Value]
genericItemToProviderInputShared = \case
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

decodeResponse :: ProviderTag -> Value -> Either LlmChatError [HistoryItem]
decodeResponse providerTag responseValue = do
    responseObject <- expectObject "response" responseValue
    let responseId = lookupText "id" responseObject
    outputValue <-
        maybe
            (Left (LlmExpectationError "Responses API returned no output array"))
            Right
            (KM.lookup "output" responseObject)
    outputItems <- expectArray "response.output" outputValue
    forM (Vector.toList outputItems) $ \payload -> do
        payloadObject <- expectObject "response.output item" payload
        let nativeItemId = lookupText "id" payloadObject
            nativeItem =
                NativeResponseItem
                    { responseId
                    , nativeItemId
                    , payload
                    }
        pure $ case providerTag of
            ProviderOpenAI ->
                wrapNativeHistoryItem OpenAIProvider nativeItem
            ProviderXAI ->
                wrapNativeHistoryItem XAIProvider nativeItem

historyItemToGenericItems :: HistoryItem -> ([Text], [GenericItem])
historyItemToGenericItems = \case
    HLocal localItem ->
        ([], [localItemToGenericItem localItem])
    HOpenAIResponses (OpenAIResponsesItem NativeResponseItem{payload}) ->
        providerPayloadToGenericItems OpenAIProvider payload
    HXAIResponses (XAIResponsesItem NativeResponseItem{payload}) ->
        providerPayloadToGenericItems XAIProvider payload

localItemToGenericItem :: LocalItem -> GenericItem
localItemToGenericItem = \case
    LocalMessage{role, parts} ->
        GenericMessage{role, parts}
    LocalToolCall{toolCall = localToolCall} ->
        GenericToolCall localToolCall
    LocalToolResult{toolResult = localToolResult} ->
        GenericToolResult localToolResult

providerPayloadToGenericItemsShared :: Value -> ([Text], [GenericItem])
providerPayloadToGenericItemsShared payload =
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
                    ([unsupportedItemTypeNote unsupportedType], [])
                _ ->
                    (["Dropped malformed native response item"], [])
        _ ->
            (["Dropped non-object native response item"], [])
  where
    unsupportedItemTypeNote itemType =
        "Dropped unsupported native response item type: " <> itemType

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

valueToCompactText :: Value -> Text
valueToCompactText =
    TL.toStrict . encodeToLazyText

encodeObjectText :: Map Text Value -> Text
encodeObjectText args =
    TL.toStrict $
        encodeToLazyText $
            Object (KM.fromMap (Map.mapKeys Key.fromText args))

roleTextToGenericRole :: Text -> Maybe GenericRole
roleTextToGenericRole = \case
    "system" -> Just GenericSystem
    "developer" -> Just GenericDeveloper
    "user" -> Just GenericUser
    "assistant" -> Just GenericAssistant
    _ -> Nothing

lookupText :: Key -> Object -> Maybe Text
lookupText key objectValue = KM.lookup key objectValue >>= \case
    String text -> Just text
    _ -> Nothing

expectObject :: Text -> Value -> Either LlmChatError Object
expectObject label = \case
    Object objectValue ->
        Right objectValue
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an object"))

expectArray :: Text -> Value -> Either LlmChatError (Vector.Vector Value)
expectArray label = \case
    Array values ->
        Right values
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an array"))
