{-# LANGUAGE RecordWildCards #-}

module Rake.Providers.Chat.Responses
    ( ResponsesProviderTag (..)
    , ResponsesProviderConfig (..)
    , runResponsesChatProvider
    , decodeResponsesResponse
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client.TLS (newTlsManager)
import Rake.Effect
import Rake.Internal.Schema (normalizeStructuredOutputSchema)
import Rake.Providers.Chat.Projection (historyItemToGenericItems, historyItemsToGenericItems)
import Rake.Providers.Internal (runChatProvider, valueToCompactText)
import Rake.Types
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data ResponsesProviderTag
    = ResponsesProviderOpenAI
    | ResponsesProviderXAI
    deriving stock (Show, Eq)

data ResponsesProviderConfig es = ResponsesProviderConfig
    { providerTag :: ResponsesProviderTag
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

runResponsesChatProvider
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       )
    => ResponsesProviderConfig es
    -> Eff (Rake ': es) a
    -> Eff es a
runResponsesChatProvider config@ResponsesProviderConfig{..} eff = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postResponse = client responsesApi

    runChatProvider (\tools responseFormat samplingOptions history -> do
            requestBody <- buildResponsesRequestBody config tools responseFormat samplingOptions history
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
                        Right response ->
                            pure response
            requestLogger (NativeMsgIn responseValue)
            either throwError pure (decodeResponsesResponse providerTag responseValue)
        ) eff
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildResponsesRequestBody
    :: ResponsesProviderConfig es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildResponsesRequestBody ResponsesProviderConfig{providerTag, requestLogger, model} tools responseFormat samplingOptions history = do
    input <- fmap concat $ traverse (renderHistoryItemForResponses providerTag requestLogger) history
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

renderHistoryItemForResponses
    :: ResponsesProviderTag
    -> (NativeMsgFormat -> Eff es ())
    -> HistoryItem
    -> Eff es [Value]
renderHistoryItemForResponses providerTag requestLogger historyItem =
    case reusableNativePayloadForResponsesProvider providerTag historyItem of
        Just payload ->
            pure [payload]
        Nothing -> do
            let (notes, genericItems) = historyItemToGenericItems historyItem
            traverse_ (requestLogger . NativeConversionNote) notes
            fmap concat (traverse (renderGenericItemForResponses requestLogger) genericItems)

reusableNativePayloadForResponsesProvider :: ResponsesProviderTag -> HistoryItem -> Maybe Value
reusableNativePayloadForResponsesProvider providerTag = \case
    HProvider ProviderHistoryItem{apiFamily, itemLifecycle, nativeItem = NativeProviderItem{payload}}
        | apiFamily == responsesProviderApiFamily providerTag
        , shouldReuseResponsesNativePayload itemLifecycle payload ->
            Just payload
    _ ->
        Nothing

shouldReuseResponsesNativePayload :: ItemLifecycle -> Value -> Bool
shouldReuseResponsesNativePayload itemLifecycle _payload =
    itemLifecycle == ItemCompleted

responsesProviderApiFamily :: ResponsesProviderTag -> ProviderApiFamily
responsesProviderApiFamily = \case
    ResponsesProviderOpenAI ->
        ProviderOpenAIResponses
    ResponsesProviderXAI ->
        ProviderXAIResponses

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
    object
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

renderGenericItemForResponses
    :: (NativeMsgFormat -> Eff es ())
    -> GenericItem
    -> Eff es [Value]
renderGenericItemForResponses requestLogger = \case
    GenericMessage{role, parts, itemLifecycle} -> do
        traverse_ (requestLogger . NativeConversionNote) (pendingAssistantAnnotationNote role itemLifecycle)
        pure [messageValue (genericRoleToText role) (messagePartsValue role itemLifecycle parts)]
    GenericToolCall{toolCall = genericToolCall'} ->
        pure [toolCallValue genericToolCall']
    GenericToolResult{toolResult = genericToolResult'} ->
        pure [toolResultValue genericToolResult']

messagePartsValue :: GenericRole -> ItemLifecycle -> [MessagePart] -> Value
messagePartsValue role itemLifecycle =
    messagePartsValueForRole role . annotatePendingAssistantParts role itemLifecycle

messagePartsValueForRole :: GenericRole -> [MessagePart] -> Value
messagePartsValueForRole role = \case
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

annotatePendingAssistantParts :: GenericRole -> ItemLifecycle -> [MessagePart] -> [MessagePart]
annotatePendingAssistantParts role itemLifecycle parts
    | role == GenericAssistant && itemLifecycle == ItemPending =
        case parts of
            [] ->
                [PartText pendingAssistantAnnotation]
            PartText{text} : rest ->
                PartText{ text = pendingAssistantAnnotation <> text } : rest
    | otherwise =
        parts
  where
    pendingAssistantAnnotation =
        "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n"

pendingAssistantAnnotationNote :: GenericRole -> ItemLifecycle -> [Text]
pendingAssistantAnnotationNote role itemLifecycle
    | role == GenericAssistant && itemLifecycle == ItemPending =
        ["Rendered pending assistant text as annotated assistant content for Responses input"]
    | otherwise =
        []

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

encodeObjectText :: Map Text Value -> Text
encodeObjectText args =
    valueToCompactText $
        Object (KM.fromMap (Map.mapKeys Key.fromText args))

decodeResponsesResponse :: ResponsesProviderTag -> Value -> Either RakeError ProviderRound
decodeResponsesResponse providerTag responseValue = do
    responseObject <- expectObject "response" responseValue
    let responseId = lookupText "id" responseObject
        responseStatus = lookupText "status" responseObject
    outputItems <- case KM.lookup "output" responseObject of
        Just outputValue ->
            expectArray "response.output" outputValue
        Nothing ->
            Right Vector.empty
    parsedOutputItems <- forM (Vector.toList outputItems) $ \payload -> do
        payloadObject <- expectObject "response.output item" payload
        pure (payload, payloadObject)
    let outputStatuses = mapMaybe (lookupText "status" . snd) parsedOutputItems
        classificationHistoryItems =
            [ HProvider
                ProviderHistoryItem
                    { apiFamily = responsesProviderApiFamily providerTag
                    , itemLifecycle = ItemCompleted
                    , nativeItem =
                        NativeProviderItem
                            { exchangeId = responseId
                            , nativeItemId = Nothing
                            , payload
                            }
                    }
            | (payload, _) <- parsedOutputItems
            ]
        (projectionNotes, projectedItems) =
            historyItemsToGenericItems classificationHistoryItems
        toolCalls = collectToolCalls projectedItems
        roundAction =
            responsesRoundAction
                responseStatus
                outputStatuses
                projectedItems
                toolCalls
                projectionNotes
        roundItemLifecycle = providerRoundItemLifecycle roundAction
    roundItems <- forM parsedOutputItems $ \(payload, payloadObject) -> do
        let nativeItemId = lookupText "id" payloadObject
        pure $
            HProvider
                ProviderHistoryItem
                    { apiFamily = responsesProviderApiFamily providerTag
                    , itemLifecycle = roundItemLifecycle
                    , nativeItem =
                        NativeProviderItem
                            { exchangeId = responseId
                            , nativeItemId
                            , payload
                            }
                    }
    pure ProviderRound{roundItems, action = roundAction}

responsesRoundAction
    :: Maybe Text
    -> [Text]
    -> [GenericItem]
    -> [ToolCall]
    -> [Text]
    -> ProviderRoundAction
responsesRoundAction responseStatus outputStatuses projectedItems toolCalls projectionNotes
    | Just failureReason <- responsesFailureReason responseStatus outputStatuses =
        ProviderRoundFailed failureReason
    | not (null toolCalls) =
        ProviderRoundNeedsLocalTools toolCalls
    | Just pauseReason <- responsesPauseReason responseStatus outputStatuses =
        ProviderRoundPaused pauseReason
    | hasAssistantMessage projectedItems =
        ProviderRoundDone
    | otherwise =
        ProviderRoundFailed $
            FailureContract $
                appendProjectionNotes
                    "Responses response completed without tool calls or assistant message"
                    projectionNotes

responsesFailureReason :: Maybe Text -> [Text] -> Maybe ChatFailureReason
responsesFailureReason responseStatus outputStatuses =
    asum
        (map statusFailureReason (statusContexts "Responses response status was " responseStatus outputStatuses))

responsesPauseReason :: Maybe Text -> [Text] -> Maybe ChatPauseReason
responsesPauseReason responseStatus outputStatuses =
    asum
        (map statusIncompletePause (statusContexts "Responses response status was " responseStatus outputStatuses))
        <|> asum
            (map statusWaitingPause (statusContexts "Responses response status was " responseStatus outputStatuses))

statusContexts :: Text -> Maybe Text -> [Text] -> [(Text, Text)]
statusContexts topPrefix maybeTopLevelStatus outputStatuses =
    maybe [] (\statusText -> [(topPrefix, statusText)]) maybeTopLevelStatus
        <> map (("Responses output item status was ",)) outputStatuses

statusFailureReason :: (Text, Text) -> Maybe ChatFailureReason
statusFailureReason (prefix, statusText) = case statusText of
    "failed" ->
        Just (FailureProvider (prefix <> statusText))
    "cancelled" ->
        Just (FailureProvider (prefix <> statusText))
    "canceled" ->
        Just (FailureProvider (prefix <> statusText))
    "expired" ->
        Just (FailureProvider (prefix <> statusText))
    "completed" ->
        Nothing
    "incomplete" ->
        Nothing
    "queued" ->
        Nothing
    "in_progress" ->
        Nothing
    "pending" ->
        Nothing
    "processing" ->
        Nothing
    otherStatus ->
        Just (FailureContract ("Unsupported Responses status: " <> otherStatus))

statusIncompletePause :: (Text, Text) -> Maybe ChatPauseReason
statusIncompletePause (prefix, statusText) = case statusText of
    "incomplete" ->
        Just (PauseIncomplete (prefix <> statusText))
    _ ->
        Nothing

statusWaitingPause :: (Text, Text) -> Maybe ChatPauseReason
statusWaitingPause (prefix, statusText) = case statusText of
    "queued" ->
        Just (PauseProviderWaiting (prefix <> statusText))
    "in_progress" ->
        Just (PauseProviderWaiting (prefix <> statusText))
    "pending" ->
        Just (PauseProviderWaiting (prefix <> statusText))
    "processing" ->
        Just (PauseProviderWaiting (prefix <> statusText))
    _ ->
        Nothing

providerRoundItemLifecycle :: ProviderRoundAction -> ItemLifecycle
providerRoundItemLifecycle = \case
    ProviderRoundDone ->
        ItemCompleted
    ProviderRoundNeedsLocalTools{} ->
        ItemPending
    ProviderRoundPaused{} ->
        ItemPending
    ProviderRoundFailed{} ->
        ItemPending

collectToolCalls :: [GenericItem] -> [ToolCall]
collectToolCalls genericItems =
    [ genericToolCall'
    | GenericToolCall{toolCall = genericToolCall'} <- genericItems
    ]

hasAssistantMessage :: [GenericItem] -> Bool
hasAssistantMessage =
    any \case
        GenericMessage{role = GenericAssistant} ->
            True
        _ ->
            False

appendProjectionNotes :: Text -> [Text] -> Text
appendProjectionNotes base notes =
    if null notes
        then base
        else base <> ". Projection notes: " <> mconcat (intersperse "; " notes)

expectObject :: Text -> Value -> Either RakeError Object
expectObject label = \case
    Object objectValue ->
        Right objectValue
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an object"))

expectArray :: Text -> Value -> Either RakeError (Vector.Vector Value)
expectArray label = \case
    Array values ->
        Right values
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an array"))

lookupText :: Key.Key -> Object -> Maybe Text
lookupText key objectValue = KM.lookup key objectValue >>= \case
    String text ->
        Just text
    _ ->
        Nothing
