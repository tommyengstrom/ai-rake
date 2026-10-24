{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.Internal.Responses
    ( ResponsesProviderTag (..)
    , ResponsesProviderConfig (..)
    , runResponsesChatProvider
    , decodeResponsesResponse
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Rake.Effect
import Rake.MediaStorage.Effect
import Rake.Providers.OpenRouter.Internal.Http
    ( protectStreamingInternalAction
    , providerClientError
    , runChatProvider
    , runStreamingSseRequest
    , validateProviderRound
    , valueToCompactText
    )
import Rake.Providers.OpenRouter.Internal.Projection (classifyResponsesPayload)
import Rake.Providers.OpenRouter.Internal.Request (requestJson)
import Rake.Providers.OpenRouter.Internal.Sse
import Rake.Providers.OpenRouter.Settings
import Rake.Schema (normalizeStructuredOutputSchema)
import Rake.Types
import Relude
import Servant.Client

data ResponsesProviderTag
    = ResponsesProviderOpenRouter Text
    deriving stock (Show, Eq)

data ResponsesProviderConfig es = ResponsesProviderConfig
    { providerTag :: ResponsesProviderTag
    , apiKey :: Text
    , baseUrl :: Text
    , model :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , reasoningConfig :: Maybe Value
    , httpManager :: Maybe HttpClient.Manager
    , requestTimeoutMilliseconds :: Int
    , extraConfig :: Object
    , providerRouting :: Object
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

runResponsesChatProvider
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ResponsesProviderConfig es
    -> Eff (Rake ': es) a
    -> Eff es a
runResponsesChatProvider config@ResponsesProviderConfig{..} eff = do
    when (requestTimeoutMilliseconds <= 0 || requestTimeoutMilliseconds > maxBound `div` 1000)
        $ throwError
            (LlmExpectationError "Request timeout must be positive and fit in microseconds")
    manager <-
        maybe
            ( liftIO
                $ newTlsManagerWith tlsManagerSettings{HttpClient.managerRetryableException = const False}
            )
            pure
            httpManager
    parsedBaseUrl <-
        either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)

    runChatProvider
        ( \tools responseFormat samplingOptions history -> do
            requestBody <-
                buildResponsesRequestBody config tools responseFormat samplingOptions history
            responseValue <-
                requestJson
                    ( OpenRouterSettings
                        apiKey
                        baseUrl
                        mempty
                        (Just manager)
                        requestTimeoutMilliseconds
                        requestLogger
                    )
                    "POST"
                    ["responses"]
                    (Just requestBody)
            either throwError pure (decodeResponsesResponse providerTag responseValue)
        )
        ( \streamCallbacks tools responseFormat samplingOptions history -> do
            requestBody <-
                buildResponsesRequestBody config tools responseFormat samplingOptions history
            let streamingRequestBody = enableStreamingRequestBody requestBody
            requestLogger (NativeMsgOut streamingRequestBody)
            streamingRequest <- liftIO (buildResponsesStreamingRequest config streamingRequestBody)
            maybeFinalResponseValue <-
                runStreamingSseRequest
                    parsedBaseUrl
                    manager
                    streamingRequest
                    (\clientErr -> requestLogger (NativeRequestFailure (providerClientError clientErr)))
                    (handleResponsesStreamEvent requestLogger streamCallbacks)
            finalResponseValue <-
                maybe
                    ( throwError
                        (LlmInvalidResponseError "Responses stream ended without a terminal response event")
                    )
                    pure
                    maybeFinalResponseValue
            either throwError pure (decodeResponsesResponse providerTag finalResponseValue)
        )
        eff
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildResponsesRequestBody
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ResponsesProviderConfig es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildResponsesRequestBody
    ResponsesProviderConfig
        { providerTag
        , requestLogger
        , model
        , reasoningConfig
        , extraConfig
        , providerRouting
        }
    tools
    responseFormat
    samplingOptions
    history = do
        -- The portable system-message contract uses the latest system snapshot.
        let (maybeSystemSnapshot, chronologicalHistory) = splitRenderableResponsesHistory history
        leadingSystemInput <-
            fmap concat
                $ traverse
                    (renderHistoryItemForResponses providerTag requestLogger)
                    (maybeToList maybeSystemSnapshot)
        chronologicalInput <-
            fmap concat
                $ traverse
                    (renderHistoryItemForResponses providerTag requestLogger)
                    chronologicalHistory
        let input = leadingSystemInput <> chronologicalInput
        either throwError pure
            $ assembleRequest
                (defaultOpenRouterSettings ""){provider = providerRouting}
                model
                ( KM.union
                    extraConfig
                    (KM.singleton "include" (toJSON (["reasoning.encrypted_content"] :: [Text])))
                )
                ["input", "messages", "tools", "text", "store", "stream", "previous_response_id"]
                ( KM.fromList
                    $ ["input" .= input, "store" .= False]
                    <> samplingFields
                    <> reasoningFields
                    <> toolFields
                    <> responseFormatFields
                )
      where
        samplingFields =
            catMaybes
                [ ("temperature" .=) <$> temperature
                , ("top_p" .=) <$> topP
                ]

        SamplingOptions{temperature, topP} = samplingOptions

        reasoningFields =
            maybe [] (\configValue -> ["reasoning" .= configValue]) reasoningConfig

        toolFields
            | null tools = []
            | otherwise = ["tools" .= fmap toolDeclarationToValue tools]

        responseFormatFields =
            maybe [] (\formatValue -> ["text" .= object ["format" .= formatValue]])
                $ responseFormatToValue responseFormat

buildResponsesStreamingRequest
    :: ResponsesProviderConfig es -> Value -> IO HttpClient.Request
buildResponsesStreamingRequest
    ResponsesProviderConfig
        { apiKey
        , baseUrl
        , organizationId
        , projectId
        , requestTimeoutMilliseconds
        }
    requestBody = do
        request <-
            HttpClient.parseRequest (toString (Text.dropWhileEnd (== '/') baseUrl) <> "/responses")
        pure
            request
                { HttpClient.method = "POST"
                , HttpClient.requestHeaders =
                    catMaybes
                        [ Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))
                        , ("OpenAI-Organization",) . TextEncoding.encodeUtf8 <$> organizationId
                        , ("OpenAI-Project",) . TextEncoding.encodeUtf8 <$> projectId
                        , Just ("Content-Type", "application/json")
                        , Just ("Accept", "text/event-stream")
                        ]
                , HttpClient.requestBody = HttpClient.RequestBodyLBS (encode requestBody)
                , HttpClient.responseTimeout =
                    HttpClient.responseTimeoutMicro (requestTimeoutMilliseconds * 1000)
                , HttpClient.redirectCount = 0
                }

enableStreamingRequestBody :: Value -> Value
enableStreamingRequestBody = \case
    Object requestObject ->
        Object (KM.insert "stream" (Bool True) requestObject)
    otherValue ->
        otherValue

handleResponsesStreamEvent
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => (NativeMsgFormat -> Eff es ())
    -> StreamCallbacks es
    -> Maybe Text
    -> BS.ByteString
    -> Eff es (SseStep Value)
handleResponsesStreamEvent requestLogger streamCallbacks _ payload
    | payload == "[DONE]" =
        pure SseStop
    | otherwise =
        case eitherDecodeStrict' payload of
            Left err ->
                throwError
                    ( LlmInvalidResponseError
                        ( "Responses stream event was not valid JSON: "
                            <> err
                        )
                    )
            Right eventValue -> do
                protectStreamingInternalAction
                    (RequestLoggerFailed . ("responses: " <>))
                    (requestLogger (NativeMsgIn eventValue))
                emitResponsesStreamDelta streamCallbacks eventValue
                pure
                    $ maybe
                        SseContinue
                        SseFinish
                        (responsesTerminalResponse eventValue)

emitResponsesStreamDelta :: StreamCallbacks es -> Value -> Eff es ()
emitResponsesStreamDelta StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} = \case
    Object eventObject
        | lookupText "type" eventObject == Just "response.output_text.delta"
        , Just deltaText <- lookupText "delta" eventObject ->
            onAssistantTextDelta deltaText
        | lookupText "type" eventObject == Just "response.refusal.delta"
        , Just refusalText <- lookupText "delta" eventObject ->
            onAssistantRefusalDelta refusalText
    _ ->
        pure ()

responsesTerminalResponse :: Value -> Maybe Value
responsesTerminalResponse = \case
    Object eventObject -> do
        responseValue@(Object responseObject) <- KM.lookup "response" eventObject
        statusText <- lookupText "status" responseObject
        guard (any (== statusText) terminalStatuses)
        pure responseValue
    _ ->
        Nothing
  where
    terminalStatuses :: [Text]
    terminalStatuses =
        [ "completed"
        , "incomplete"
        , "failed"
        , "cancelled"
        , "canceled"
        , "expired"
        ]

renderHistoryItemForResponses
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ResponsesProviderTag
    -> (NativeMsgFormat -> Eff es ())
    -> HistoryItem
    -> Eff es [Value]
renderHistoryItemForResponses providerTag requestLogger =
    renderCanonicalHistoryItemForResponses providerTag requestLogger

splitRenderableResponsesHistory :: [HistoryItem] -> (Maybe HistoryItem, [HistoryItem])
splitRenderableResponsesHistory history =
    ( latestSystemSnapshot history
    , filter (not . isSystemHistoryItem) history
    )

latestSystemSnapshot :: [HistoryItem] -> Maybe HistoryItem
latestSystemSnapshot =
    -- Responses providers get one effective system message. Keeping only the
    -- latest snapshot matches our portable GenericSystem semantics and avoids
    -- sending stale system instructions for compatibility reasons.
    viaNonEmpty last . filter isSystemHistoryItem

isSystemHistoryItem :: HistoryItem -> Bool
isSystemHistoryItem HistoryItem{genericItem = GenericMessage{role = GenericSystem}} =
    True
isSystemHistoryItem _ =
    False

responsesProviderApiFamily :: ResponsesProviderTag -> ProviderApiFamily
responsesProviderApiFamily = \case
    ResponsesProviderOpenRouter model ->
        ProviderApiFamily ("openrouter.responses:" <> model)

responseFormatToValue :: ResponseFormat -> Maybe Value
responseFormatToValue = \case
    Unstructured ->
        Nothing
    JsonValue ->
        Just $ object ["type" .= ("json_object" :: Text)]
    JsonSchema schema ->
        Just
            $ object
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
    normalizeStructuredOutputSchema
        $ object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]

renderCanonicalHistoryItemForResponses
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ResponsesProviderTag
    -> (NativeMsgFormat -> Eff es ())
    -> HistoryItem
    -> Eff es [Value]
renderCanonicalHistoryItemForResponses
    providerTag
    requestLogger
    HistoryItem
        { itemLifecycle = lifecycle
        , genericItem = genericHistoryItem
        , providerItem = maybeProviderItem
        } =
        case genericHistoryItem of
            GenericMessage{role = GenericContext, parts} -> do
                content <-
                    messagePartsValue
                        (responsesProviderApiFamily providerTag)
                        GenericUser
                        lifecycle
                        (PartText "Application context (not user speech):\n" : parts)
                pure [messageValue "user" content]
            GenericMessage{role, parts} -> do
                traverse_
                    (requestLogger . NativeConversionNote)
                    (pendingAssistantAnnotationNote role lifecycle)
                content <- messagePartsValue (responsesProviderApiFamily providerTag) role lifecycle parts
                pure [messageValue (genericRoleToText role) content]
            GenericToolCall{toolCall = genericToolCall'} ->
                pure [toolCallValue genericToolCall']
            GenericToolResult{toolResult = genericToolResult'} ->
                pure [toolResultValue genericToolResult']
            GenericResetTo{} ->
                pure []
            GenericReplayBarrier{} ->
                pure []
            GenericNonPortable ->
                case maybeProviderItem of
                    Just ProviderItem{apiFamily, payload}
                        | lifecycle == ItemCompleted
                        , apiFamily == responsesProviderApiFamily providerTag ->
                            pure [payload]
                        | lifecycle == ItemCompleted -> do
                            requestLogger
                                ( NativeConversionNote
                                    ( "Dropped opaque continuation from "
                                        <> providerApiFamilyText apiFamily
                                        <> " when switching to "
                                        <> providerApiFamilyText (responsesProviderApiFamily providerTag)
                                    )
                                )
                            pure []
                    _ -> pure []

messagePartsValue
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ProviderApiFamily
    -> GenericRole
    -> ItemLifecycle
    -> [MessagePart]
    -> Eff es Value
messagePartsValue providerFamily role lifecycle =
    messagePartsValueForRole providerFamily role
        . annotatePendingAssistantParts role lifecycle

messagePartsValueForRole
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ProviderApiFamily
    -> GenericRole
    -> [MessagePart]
    -> Eff es Value
messagePartsValueForRole providerFamily role parts = do
    renderedParts <- traverse (renderResponsesMessagePart providerFamily) parts
    pure
        $ case renderedParts of
            [] ->
                String ""
            [RenderedResponsesTextPart text] ->
                String text
            _ ->
                Array
                    . Vector.fromList
                    $ map (renderedResponsesPartValue role) renderedParts

data RenderedResponsesPart
    = RenderedResponsesTextPart Text
    | RenderedResponsesNativePart Value

renderResponsesMessagePart
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ProviderApiFamily
    -> MessagePart
    -> Eff es RenderedResponsesPart
renderResponsesMessagePart providerFamily = \case
    PartText{text} ->
        pure (RenderedResponsesTextPart text)
    PartRefusal{text} ->
        pure (RenderedResponsesTextPart text)
    PartImage{blobId} ->
        resolveResponsesMediaPart providerFamily blobId
    PartAudio{blobId} ->
        resolveResponsesMediaPart providerFamily blobId
    PartFile{blobId} ->
        resolveResponsesMediaPart providerFamily blobId
  where
    resolveResponsesMediaPart targetProviderFamily blobId = do
        maybeRequestPart <- lookupMediaReference targetProviderFamily blobId
        case maybeRequestPart of
            Just requestPart ->
                pure (RenderedResponsesNativePart requestPart)
            Nothing ->
                let MediaBlobId blobIdText = blobId
                 in throwError
                        ( LlmExpectationError
                            ( "No stored media reference is available for blob "
                                <> toString blobIdText
                                <> " when rendering "
                                <> toString (providerApiFamilyText targetProviderFamily)
                            )
                        )

renderedResponsesPartValue :: GenericRole -> RenderedResponsesPart -> Value
renderedResponsesPartValue role = \case
    RenderedResponsesTextPart text ->
        object
            [ "type" .= messageTextPartType role
            , "text" .= text
            ]
    RenderedResponsesNativePart requestPart ->
        requestPart

annotatePendingAssistantParts
    :: GenericRole -> ItemLifecycle -> [MessagePart] -> [MessagePart]
annotatePendingAssistantParts role lifecycle parts
    | role == GenericAssistant && lifecycle == ItemPending =
        case parts of
            [] ->
                [PartText pendingAssistantAnnotation]
            PartText{text} : rest ->
                PartText{text = pendingAssistantAnnotation <> text} : rest
            PartRefusal{text} : rest ->
                PartRefusal{text = pendingAssistantAnnotation <> text} : rest
            _ ->
                PartText pendingAssistantAnnotation : parts
    | otherwise =
        parts
  where
    pendingAssistantAnnotation =
        "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n"

pendingAssistantAnnotationNote :: GenericRole -> ItemLifecycle -> [Text]
pendingAssistantAnnotationNote role lifecycle
    | role == GenericAssistant && lifecycle == ItemPending =
        ["Rendered pending assistant text as annotated assistant content for Responses input"]
    | otherwise =
        []

messageTextPartType :: GenericRole -> Text
messageTextPartType = \case
    GenericAssistant ->
        "output_text"
    GenericSystem ->
        "input_text"
    GenericContext ->
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
    GenericContext -> "user"
    GenericUser -> "user"
    GenericAssistant -> "assistant"

encodeObjectText :: Map Text Value -> Text
encodeObjectText args =
    valueToCompactText
        $ Object (KM.fromMap (Map.mapKeys Key.fromText args))

decodeResponsesResponse :: ResponsesProviderTag -> Value -> Either RakeError ProviderRound
decodeResponsesResponse providerTag responseValue = do
    responseObject <- expectObject "response" responseValue
    let responseId = lookupText "id" responseObject
        responseStatus = lookupText "status" responseObject
        responseApiFamily = responsesProviderApiFamily providerTag
    outputItems <- case KM.lookup "output" responseObject of
        Just outputValue ->
            expectArray "response.output" outputValue
        Nothing ->
            Right Vector.empty
    parsedOutputItems <- forM (Vector.toList outputItems) $ \payload -> do
        payloadObject <- expectObject "response.output item" payload
        pure (payload, payloadObject)
    let statusContexts =
            catMaybes
                [ statusContext
                    "Responses response status was "
                    responseStatus
                    (providerFailureDetail responseObject)
                ]
                <> mapMaybe
                    ( \(_, payloadObject) ->
                        statusContext
                            "Responses output item status was "
                            (lookupText "status" payloadObject)
                            (providerFailureDetail payloadObject)
                    )
                    parsedOutputItems
        classifiedOutputItems =
            [ let (notes, canonicalItem, mediaReferences) = classifyResponsesPayload responseApiFamily responseId payload
               in (payload, payloadObject, notes, canonicalItem, mediaReferences)
            | (payload, payloadObject) <- parsedOutputItems
            ]
        projectionNotes =
            concatMap (\(_, _, notes, _, _) -> notes) classifiedOutputItems
        projectedItems =
            [ canonicalItem
            | (_, _, _, canonicalItem, _) <- classifiedOutputItems
            ]
        roundMediaReferences =
            concatMap (\(_, _, _, _, mediaReferences) -> mediaReferences) classifiedOutputItems
        toolCalls = collectToolCalls projectedItems
        roundAction =
            responsesRoundAction
                statusContexts
                projectedItems
                toolCalls
                projectionNotes
        roundItemLifecycle = providerRoundItemLifecycle roundAction
    roundItems <- forM classifiedOutputItems $ \(payload, payloadObject, _, canonicalItem, _) -> do
        let nativeItemId = lookupText "id" payloadObject
            decodedLifecycle :: ItemLifecycle
            decodedLifecycle = case roundAction of
                ProviderRoundNeedsLocalTools{}
                    | lookupText "type" payloadObject == Just "reasoning"
                    , lookupText "status" payloadObject == Just "completed"
                        || (lookupText "status" payloadObject == Nothing && responseStatus == Just "completed") ->
                        ItemCompleted
                _ -> roundItemLifecycle
        pure
            $ HistoryItem
                { historyItemIdField = Nothing
                , itemLifecycle = decodedLifecycle
                , genericItem = canonicalItem
                , providerItem =
                    Just
                        ProviderItem
                            { apiFamily = responseApiFamily
                            , exchangeId = responseId
                            , nativeItemId
                            , payload
                            , availableLocalTools = []
                            }
                }
    validateProviderRound
        ProviderRound{roundItems, mediaReferences = roundMediaReferences, action = roundAction}

responsesRoundAction
    :: [(Text, Text, Maybe Text)]
    -> [GenericItem]
    -> [ToolCall]
    -> [Text]
    -> ProviderRoundAction
responsesRoundAction statusContexts projectedItems toolCalls projectionNotes
    | Just failureReason <- responsesFailureReason statusContexts =
        ProviderRoundFailed failureReason
    | not (null toolCalls) =
        ProviderRoundNeedsLocalTools toolCalls
    | Just pauseReason <- responsesPauseReason statusContexts =
        ProviderRoundPaused pauseReason
    | hasAssistantMessage projectedItems =
        ProviderRoundDone
    | otherwise =
        ProviderRoundFailed
            $ FailureContract
            $ appendProjectionNotes
                "Responses response completed without tool calls or assistant message"
                projectionNotes

responsesFailureReason :: [(Text, Text, Maybe Text)] -> Maybe ChatFailureReason
responsesFailureReason =
    asum . map statusFailureReason

responsesPauseReason :: [(Text, Text, Maybe Text)] -> Maybe ChatPauseReason
responsesPauseReason contexts =
    asum (map statusIncompletePause contexts)
        <|> asum (map statusWaitingPause contexts)

statusContext :: Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text, Maybe Text)
statusContext prefix maybeStatus maybeDetail =
    (\statusText -> (prefix, statusText, maybeDetail)) <$> maybeStatus

statusFailureReason :: (Text, Text, Maybe Text) -> Maybe ChatFailureReason
statusFailureReason (prefix, statusText, maybeDetail) = case statusText of
    "failed" ->
        Just (FailureProvider (appendProviderFailureDetail (prefix <> statusText) maybeDetail))
    "cancelled" ->
        Just (FailureProvider (appendProviderFailureDetail (prefix <> statusText) maybeDetail))
    "canceled" ->
        Just (FailureProvider (appendProviderFailureDetail (prefix <> statusText) maybeDetail))
    "expired" ->
        Just (FailureProvider (appendProviderFailureDetail (prefix <> statusText) maybeDetail))
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
        Just
            ( FailureContract
                (appendProviderFailureDetail ("Unsupported Responses status: " <> otherStatus) maybeDetail)
            )

statusIncompletePause :: (Text, Text, Maybe Text) -> Maybe ChatPauseReason
statusIncompletePause (prefix, statusText, _) = case statusText of
    "incomplete" ->
        Just (PauseIncomplete (prefix <> statusText))
    _ ->
        Nothing

statusWaitingPause :: (Text, Text, Maybe Text) -> Maybe ChatPauseReason
statusWaitingPause (prefix, statusText, _) = case statusText of
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

appendProviderFailureDetail :: Text -> Maybe Text -> Text
appendProviderFailureDetail base maybeDetail =
    maybe base ((base <> ": ") <>) maybeDetail

providerFailureDetail :: Object -> Maybe Text
providerFailureDetail objectValue =
    (KM.lookup "error" objectValue >>= providerErrorValueDetail)
        <|> lookupText "message" objectValue
        <|> lookupText "failure_reason" objectValue

providerErrorValueDetail :: Value -> Maybe Text
providerErrorValueDetail = \case
    Object errorObject ->
        combineErrorCodeAndMessage
            (lookupText "code" errorObject)
            (lookupText "message" errorObject)
            <|> Just (valueToCompactText (Object errorObject))
    String errorText ->
        Just errorText
    otherValue ->
        Just (valueToCompactText otherValue)

combineErrorCodeAndMessage :: Maybe Text -> Maybe Text -> Maybe Text
combineErrorCodeAndMessage maybeCode maybeMessage =
    case (maybeCode, maybeMessage) of
        (Just code, Just message) ->
            Just (code <> ": " <> message)
        (Just code, Nothing) ->
            Just code
        (Nothing, Just message) ->
            Just message
        (Nothing, Nothing) ->
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
        Left (LlmInvalidResponseError ("Expected " <> toString label <> " to be an object"))

expectArray :: Text -> Value -> Either RakeError (Vector.Vector Value)
expectArray label = \case
    Array values ->
        Right values
    _ ->
        Left (LlmInvalidResponseError ("Expected " <> toString label <> " to be an array"))

lookupText :: Key.Key -> Object -> Maybe Text
lookupText key objectValue =
    KM.lookup key objectValue >>= \case
        String text ->
            Just text
        _ ->
            Nothing
