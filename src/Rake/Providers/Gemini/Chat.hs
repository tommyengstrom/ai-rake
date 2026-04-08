{-# LANGUAGE RecordWildCards #-}

module Rake.Providers.Gemini.Chat
    ( GeminiChatSettings (..)
    , defaultGeminiChatSettings
    , GeminiGenerationConfig (..)
    , defaultGeminiGenerationConfig
    , GeminiThinkingLevel (..)
    , GeminiThinkingSummaries (..)
    , GeminiToolChoice (..)
    , GeminiAllowedTools (..)
    , GeminiToolChoiceMode (..)
    , GeminiSearchType (..)
    , GeminiTool (..)
    , GeminiComputerUseConfig (..)
    , GeminiComputerUseEnvironment (..)
    , GeminiMcpServerConfig (..)
    , GeminiFileSearchConfig (..)
    , GeminiGoogleMapsConfig (..)
    , decodeGeminiResponse
    , runRakeGeminiChat
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (newTlsManager)
import Rake.Effect
import Rake.Internal.Sse
import Rake.MediaStorage.Effect
import Rake.Providers.Chat.Projection (classifyGeminiPayloads)
import Rake.Providers.Internal
    ( defaultWarningLogger
    , protectStreamingInternalAction
    , runChatProvider
    , runStreamingSseRequest
    , valueToCompactText
    )
import Rake.Types
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data GeminiChatSettings es = GeminiChatSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , systemInstruction :: Maybe Text
    , providerTools :: [GeminiTool]
    , generationConfig :: GeminiGenerationConfig
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultGeminiChatSettings :: Text -> GeminiChatSettings es
defaultGeminiChatSettings apiKey =
    GeminiChatSettings
        { apiKey
        , model = "gemini-3-flash-preview"
        , baseUrl = "https://generativelanguage.googleapis.com"
        , systemInstruction = Nothing
        , providerTools = []
        , generationConfig = defaultGeminiGenerationConfig
        , requestLogger = defaultWarningLogger "gemini.chat"
        }

data GeminiGenerationConfig = GeminiGenerationConfig
    { temperature :: Maybe Double
    , topP :: Maybe Double
    , seed :: Maybe Int
    , stopSequences :: [Text]
    , thinkingLevel :: Maybe GeminiThinkingLevel
    , thinkingSummaries :: Maybe GeminiThinkingSummaries
    , maxOutputTokens :: Maybe Int
    , toolChoice :: Maybe GeminiToolChoice
    }
    deriving stock (Show, Eq, Generic)

defaultGeminiGenerationConfig :: GeminiGenerationConfig
defaultGeminiGenerationConfig =
    GeminiGenerationConfig
        { temperature = Nothing
        , topP = Nothing
        , seed = Nothing
        , stopSequences = []
        , thinkingLevel = Nothing
        , thinkingSummaries = Nothing
        , maxOutputTokens = Nothing
        , toolChoice = Nothing
        }

data GeminiThinkingLevel
    = GeminiThinkingMinimal
    | GeminiThinkingLow
    | GeminiThinkingMedium
    | GeminiThinkingHigh
    deriving stock (Show, Eq, Generic)

data GeminiThinkingSummaries
    = GeminiThinkingSummariesAuto
    | GeminiThinkingSummariesNone
    deriving stock (Show, Eq, Generic)

data GeminiToolChoiceMode
    = GeminiToolChoiceAuto
    | GeminiToolChoiceAny
    | GeminiToolChoiceNone
    | GeminiToolChoiceValidated
    deriving stock (Show, Eq, Generic)

data GeminiAllowedTools = GeminiAllowedTools
    { mode :: Maybe GeminiToolChoiceMode
    , tools :: [Text]
    }
    deriving stock (Show, Eq, Generic)

data GeminiToolChoice
    = GeminiToolChoiceModeOnly GeminiToolChoiceMode
    | GeminiToolChoiceAllowedTools GeminiAllowedTools
    deriving stock (Show, Eq, Generic)

data GeminiSearchType
    = GeminiWebSearch
    | GeminiImageSearch
    deriving stock (Show, Eq, Generic)

data GeminiComputerUseEnvironment
    = GeminiComputerUseBrowser
    deriving stock (Show, Eq, Generic)

data GeminiComputerUseConfig = GeminiComputerUseConfig
    { environment :: Maybe GeminiComputerUseEnvironment
    , excludedPredefinedFunctions :: [Text]
    }
    deriving stock (Show, Eq, Generic)

data GeminiMcpServerConfig = GeminiMcpServerConfig
    { name :: Maybe Text
    , url :: Maybe Text
    , headers :: Maybe Value
    , allowedTools :: Maybe GeminiAllowedTools
    }
    deriving stock (Show, Eq, Generic)

data GeminiFileSearchConfig = GeminiFileSearchConfig
    { fileSearchStoreNames :: [Text]
    , topK :: Maybe Int
    , metadataFilter :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

data GeminiGoogleMapsConfig = GeminiGoogleMapsConfig
    { enableWidget :: Maybe Bool
    , latitude :: Maybe Double
    , longitude :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)

data GeminiTool
    = GeminiGoogleSearchTool [GeminiSearchType]
    | GeminiCodeExecutionTool
    | GeminiUrlContextTool
    | GeminiComputerUseTool GeminiComputerUseConfig
    | GeminiMcpServerTool GeminiMcpServerConfig
    | GeminiFileSearchTool GeminiFileSearchConfig
    | GeminiGoogleMapsTool GeminiGoogleMapsConfig
    deriving stock (Show, Eq, Generic)

instance ToJSON GeminiThinkingLevel where
    toJSON = String . \case
        GeminiThinkingMinimal -> "minimal"
        GeminiThinkingLow -> "low"
        GeminiThinkingMedium -> "medium"
        GeminiThinkingHigh -> "high"

instance ToJSON GeminiThinkingSummaries where
    toJSON = String . \case
        GeminiThinkingSummariesAuto -> "auto"
        GeminiThinkingSummariesNone -> "none"

instance ToJSON GeminiToolChoiceMode where
    toJSON = String . \case
        GeminiToolChoiceAuto -> "auto"
        GeminiToolChoiceAny -> "any"
        GeminiToolChoiceNone -> "none"
        GeminiToolChoiceValidated -> "validated"

instance ToJSON GeminiAllowedTools where
    toJSON GeminiAllowedTools{mode, tools} =
        object $
            catMaybes
                [ ("mode" .=) <$> mode
                , Just ("tools" .= tools)
                ]

instance ToJSON GeminiToolChoice where
    toJSON = \case
        GeminiToolChoiceModeOnly mode ->
            toJSON mode
        GeminiToolChoiceAllowedTools allowedTools ->
            object ["allowed_tools" .= allowedTools]

instance ToJSON GeminiSearchType where
    toJSON = String . \case
        GeminiWebSearch -> "web_search"
        GeminiImageSearch -> "image_search"

instance ToJSON GeminiComputerUseEnvironment where
    toJSON = String . \case
        GeminiComputerUseBrowser -> "browser"

instance ToJSON GeminiTool where
    toJSON = \case
        GeminiGoogleSearchTool searchTypes ->
            object $
                ["type" .= ("google_search" :: Text)]
                    <> [ "search_types" .= searchTypes
                       | not (null searchTypes)
                       ]
        GeminiCodeExecutionTool ->
            object ["type" .= ("code_execution" :: Text)]
        GeminiUrlContextTool ->
            object ["type" .= ("url_context" :: Text)]
        GeminiComputerUseTool GeminiComputerUseConfig{environment, excludedPredefinedFunctions} ->
            object $
                [ "type" .= ("computer_use" :: Text)
                ]
                    <> catMaybes
                        [ ("environment" .=) <$> environment
                        , if null excludedPredefinedFunctions
                            then Nothing
                            else Just ("excludedPredefinedFunctions" .= excludedPredefinedFunctions)
                        ]
        GeminiMcpServerTool GeminiMcpServerConfig{name, url, headers, allowedTools} ->
            object $
                [ "type" .= ("mcp_server" :: Text)
                ]
                    <> catMaybes
                        [ ("name" .=) <$> name
                        , ("url" .=) <$> url
                        , ("headers" .=) <$> headers
                        , ("allowed_tools" .=) <$> allowedTools
                        ]
        GeminiFileSearchTool GeminiFileSearchConfig{fileSearchStoreNames, topK, metadataFilter} ->
            object $
                [ "type" .= ("file_search" :: Text)
                ]
                    <> catMaybes
                        [ if null fileSearchStoreNames
                            then Nothing
                            else Just ("file_search_store_names" .= fileSearchStoreNames)
                        , ("top_k" .=) <$> topK
                        , ("metadata_filter" .=) <$> metadataFilter
                        ]
        GeminiGoogleMapsTool GeminiGoogleMapsConfig{enableWidget, latitude, longitude} ->
            object $
                [ "type" .= ("google_maps" :: Text)
                ]
                    <> catMaybes
                        [ ("enable_widget" .=) <$> enableWidget
                        , ("latitude" .=) <$> latitude
                        , ("longitude" .=) <$> longitude
                        ]

type GeminiInteractionsAPI =
    "v1beta"
        Servant.:> "interactions"
        Servant.:> Header "x-goog-api-key" Text
        Servant.:> ReqBody '[JSON] Value
        Servant.:> Post '[JSON] Value

geminiInteractionsApi :: Proxy GeminiInteractionsAPI
geminiInteractionsApi = Proxy

runRakeGeminiChat
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => GeminiChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeGeminiChat settings@GeminiChatSettings{apiKey, baseUrl, requestLogger} eff = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postInteraction = client geminiInteractionsApi

    runChatProvider
        (\tools responseFormat samplingOptions history -> do
            requestBody <- buildGeminiRequestBody settings tools responseFormat samplingOptions history
            requestLogger (NativeMsgOut requestBody)
            responseValue <-
                liftIO
                    (runClientM (postInteraction (Just apiKey) requestBody) clientEnv)
                    >>= \case
                        Left err -> do
                            requestLogger (NativeRequestFailure err)
                            throwError (LlmClientError err)
                        Right response ->
                            pure response
            requestLogger (NativeMsgIn responseValue)
            either throwError pure (decodeGeminiResponse responseValue)
        )
        (\streamCallbacks tools responseFormat samplingOptions history -> do
            requestBody <- buildGeminiRequestBody settings tools responseFormat samplingOptions history
            let streamingRequestBody = enableStreamingRequestBody requestBody
            requestLogger (NativeMsgOut streamingRequestBody)
            streamingRequest <- liftIO (buildGeminiStreamingRequest baseUrl apiKey streamingRequestBody)
            maybeFinalResponseValue <-
                runStreamingSseRequest
                    parsedBaseUrl
                    manager
                    streamingRequest
                    (\clientErr -> requestLogger (NativeRequestFailure clientErr))
                    (handleGeminiStreamEvent requestLogger streamCallbacks)
            finalResponseValue <-
                maybe
                    (throwError (LlmExpectationError "Gemini stream ended without a terminal interaction event"))
                    pure
                    maybeFinalResponseValue
            either throwError pure (decodeGeminiResponse finalResponseValue)
        )
        eff
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildGeminiStreamingRequest :: Text -> Text -> Value -> IO HttpClient.Request
buildGeminiStreamingRequest baseUrl apiKey requestBody = do
    request <- HttpClient.parseRequest (toString baseUrl <> "/v1beta/interactions?alt=sse")
    pure
        request
            { HttpClient.method = "POST"
            , HttpClient.requestHeaders =
                [ ("x-goog-api-key", TextEncoding.encodeUtf8 apiKey)
                , ("Content-Type", "application/json")
                , ("Accept", "text/event-stream")
                ]
            , HttpClient.requestBody = HttpClient.RequestBodyLBS (encode requestBody)
            , HttpClient.responseTimeout = HttpClient.responseTimeoutNone
            }

enableStreamingRequestBody :: Value -> Value
enableStreamingRequestBody = \case
    Object requestObject ->
        Object (KM.insert "stream" (Bool True) requestObject)
    otherValue ->
        otherValue

handleGeminiStreamEvent
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => (NativeMsgFormat -> Eff es ())
    -> StreamCallbacks es
    -> Maybe Text
    -> BS.ByteString
    -> Eff es (SseStep Value)
handleGeminiStreamEvent requestLogger streamCallbacks _ payload
    | payload == "[DONE]" =
        pure SseStop
    | otherwise =
        case eitherDecodeStrict' payload of
            Left err ->
                throwError
                    ( LlmExpectationError
                        ( "Gemini stream event was not valid JSON: "
                            <> err
                        )
                    )
            Right eventValue -> do
                protectStreamingInternalAction
                    (RequestLoggerFailed . ("gemini: " <>))
                    (requestLogger (NativeMsgIn eventValue))
                emitGeminiStreamDelta streamCallbacks eventValue
                pure $
                    maybe
                        SseContinue
                        SseFinish
                        (geminiTerminalInteraction eventValue)

emitGeminiStreamDelta :: StreamCallbacks es -> Value -> Eff es ()
emitGeminiStreamDelta StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} = \case
    Object eventObject
        | lookupText "event_type" eventObject == Just "content.delta"
        , Just (Object deltaObject) <- KM.lookup "delta" eventObject ->
            case lookupText "type" deltaObject of
                Just "text"
                    | Just deltaText <- lookupText "text" deltaObject ->
                        onAssistantTextDelta deltaText
                Just "refusal"
                    | Just refusalText <- lookupText "refusal" deltaObject <|> lookupText "text" deltaObject ->
                        onAssistantRefusalDelta refusalText
                _ ->
                    pure ()
    _ ->
        pure ()

geminiTerminalInteraction :: Value -> Maybe Value
geminiTerminalInteraction = \case
    Object eventObject
        | lookupText "event_type" eventObject == Just "interaction.complete" ->
            KM.lookup "interaction" eventObject
        | Just interactionValue@(Object interactionObject) <- KM.lookup "interaction" eventObject
        , Just statusText <- lookupText "status" interactionObject
        , any (== statusText) terminalStatuses ->
            Just interactionValue
    _ ->
        Nothing
  where
    terminalStatuses :: [Text]
    terminalStatuses =
        [ "completed"
        , "requires_action"
        , "incomplete"
        , "failed"
        , "cancelled"
        , "canceled"
        , "expired"
        ]

buildGeminiRequestBody
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => GeminiChatSettings es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildGeminiRequestBody GeminiChatSettings{model, providerTools, systemInstruction, generationConfig, requestLogger = _requestLogger} tools responseFormat samplingOptions history = do
    -- Gemini also gets one effective system instruction. We therefore collapse
    -- GenericSystem to the latest snapshot for compatibility with the shared
    -- portable semantics used across providers.
    let (maybeSystemSnapshot, chronologicalHistory) = splitRenderableGeminiHistory history
    renderedHistory <- renderGeminiHistory chronologicalHistory
    renderedSystemInstruction <- traverse historyItemSystemInstruction maybeSystemSnapshot
    let RenderedGeminiHistory
            { renderedTurns = renderedTurnValues
            } = renderedHistory
    pure $
        object $
            [ "model" .= model
            , "input" .= reverse renderedTurnValues
            , "store" .= False
            ]
                <> catMaybes
                    [ ("system_instruction" .=) <$> combinedSystemInstruction systemInstruction renderedSystemInstruction
                    , if null allTools
                        then Nothing
                        else Just ("tools" .= allTools)
                    , ("response_format" .=) <$> responseFormatSchema responseFormat
                    , ("response_mime_type" .= ("application/json" :: Text))
                        <$ guard (hasStructuredResponseFormat responseFormat)
                    , ("generation_config" .=) <$> generationConfigValue generationConfig samplingOptions
                    ]
  where
    allTools =
        map localToolDeclarationToGeminiTool tools
            <> fmap toJSON providerTools

renderGeminiHistory
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => [HistoryItem]
    -> Eff es RenderedGeminiHistory
renderGeminiHistory =
    foldlM renderGeminiHistoryItem initialRenderedGeminiHistory

renderGeminiHistoryItem
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => RenderedGeminiHistory
    -> HistoryItem
    -> Eff es RenderedGeminiHistory
renderGeminiHistoryItem renderedHistory historyEntry =
    renderGeminiCanonicalHistoryItem renderedHistory historyEntry

appendGeminiTurn :: Text -> Value -> [Value] -> [Value]
appendGeminiTurn role content = \case
    Object existingTurn : rest
        | turnRole existingTurn == Just role ->
            Object (KM.insert "content" (appendTurnContent content existingTurn) existingTurn) : rest
    turns ->
        turnValue role [content] : turns
  where
    turnRole turnObject =
        KM.lookup "role" turnObject >>= \case
            String turnRoleText ->
                Just turnRoleText
            _ ->
                Nothing

    appendTurnContent newContent turnObject = case KM.lookup "content" turnObject of
        Just (Array contentParts) ->
            Array (contentParts <> Vector.singleton newContent)
        _ ->
            Array (Vector.singleton newContent)

turnValue :: Text -> [Value] -> Value
turnValue role content =
    object
        [ "role" .= role
        , "content" .= content
        ]

data RenderedGeminiHistory = RenderedGeminiHistory
    { renderedTurns :: [Value]
    , toolCallNames :: Map Text Text
    , openModelTurnHasFunctionCall :: Bool
    }

initialRenderedGeminiHistory :: RenderedGeminiHistory
initialRenderedGeminiHistory =
    RenderedGeminiHistory
        { renderedTurns = []
        , toolCallNames = mempty
        , openModelTurnHasFunctionCall = False
        }

combinedSystemInstruction :: Maybe Text -> Maybe Text -> Maybe Text
combinedSystemInstruction settingsInstruction historyInstruction =
    if null allBlocks
        then Nothing
        else Just (mconcat (intersperse "\n\n" allBlocks))
  where
    allBlocks = catMaybes [settingsInstruction, historyInstruction]

renderGeminiCanonicalHistoryItem
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => RenderedGeminiHistory
    -> HistoryItem
    -> Eff es RenderedGeminiHistory
renderGeminiCanonicalHistoryItem renderedHistory HistoryItem
    { itemLifecycle = lifecycle
    , genericItem = genericHistoryItem
    , providerItem = maybeProviderItem
    } =
    case genericHistoryItem of
        GenericMessage{role = GenericUser, parts} -> do
            renderedContentParts <- messagePartsToGeminiContentParts ProviderGeminiInteractions parts
            pure (appendGeminiTurnBlocks "user" renderedContentParts renderedHistory)
        GenericMessage{role = GenericAssistant, parts} -> do
            renderedContentParts <- messagePartsToGeminiContentParts ProviderGeminiInteractions parts
            pure
                ( appendGeminiTurnBlocks
                    "model"
                    (annotatePendingAssistantContentParts lifecycle renderedContentParts)
                    renderedHistory
                )
        GenericMessage{role = GenericSystem} ->
            pure renderedHistory
        GenericToolCall{toolCall = ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs, continuationAttachments}} ->
            let geminiContinuationPayloads =
                    [ continuationPayload
                    | ToolCallContinuation
                        { continuationProviderFamily = ProviderGeminiInteractions
                        , continuationPayload
                        } <- continuationAttachments
                    ]
                renderedHistoryWithContinuations =
                    foldl' (flip (appendGeminiTurnBlock "model")) renderedHistory geminiContinuationPayloads
                RenderedGeminiHistory{openModelTurnHasFunctionCall = sawFunctionCallInCurrentModelTurn} =
                    renderedHistoryWithContinuations
                shouldInjectDummyThoughtSignature =
                    null geminiContinuationPayloads
                        && not sawFunctionCallInCurrentModelTurn
             in
                pure $
                    appendGeminiTurnBlock
                        "model"
                        (genericGeminiFunctionCallValue shouldInjectDummyThoughtSignature toolCallId toolName toolArgs)
                        (registerToolCall toolCallId toolName renderedHistoryWithContinuations)
        GenericToolResult{toolResult = ToolResult{toolCallId = ToolCallId toolCallId, toolResponse}} -> do
            let RenderedGeminiHistory{toolCallNames} = renderedHistory
            toolName <-
                maybe
                    (throwError (LlmExpectationError "Gemini function_result requires the preceding tool call name"))
                    pure
                    (Map.lookup toolCallId toolCallNames)
            pure $
                appendGeminiTurnBlock
                    "user"
                    ( object
                        [ "type" .= ("function_result" :: Text)
                        , "name" .= toolName
                        , "call_id" .= toolCallId
                        , "result" .= geminiToolResultValue toolResponse
                        ]
                    )
                    renderedHistory
        GenericResetTo{} ->
            pure renderedHistory
        GenericReplayBarrier{} ->
            pure renderedHistory
        GenericNonPortable ->
            pure $
                case maybeProviderItem of
                    Just ProviderItem{apiFamily, payload}
                        | lifecycle == ItemCompleted
                        , apiFamily == ProviderGeminiInteractions ->
                            appendGeminiTurnBlock "model" payload renderedHistory
                    _ ->
                        renderedHistory

appendGeminiTurnBlock :: Text -> Value -> RenderedGeminiHistory -> RenderedGeminiHistory
appendGeminiTurnBlock role content renderedHistory@RenderedGeminiHistory{renderedTurns, openModelTurnHasFunctionCall} =
    renderedHistory
        { renderedTurns = appendGeminiTurn role content renderedTurns
        , openModelTurnHasFunctionCall =
            case role of
                "model" ->
                    if extendsExistingTurn
                        then openModelTurnHasFunctionCall || isGeminiFunctionCallPayload content
                        else isGeminiFunctionCallPayload content
                _ ->
                    False
        }
  where
    extendsExistingTurn =
        case renderedTurns of
            Object existingTurn : _
                | renderedTurnRole existingTurn == Just role ->
                    True
            _ ->
                False

isGeminiFunctionCallPayload :: Value -> Bool
isGeminiFunctionCallPayload = \case
    Object payloadObject ->
        lookupText "type" payloadObject == Just "function_call"
    _ ->
        False

-- Gemini 3 function calling validates the current-turn function call against a
-- thought signature. When replaying a foreign tool continuation into Gemini,
-- there is no provider-issued signature to preserve, so we attach Google's
-- documented dummy signature to the first replayed generic function_call in
-- that step. Same-provider Gemini continuation instead carries provider-owned
-- continuation payloads on the pending ToolCall itself.
-- Docs: https://ai.google.dev/gemini-api/docs/thought-signatures
geminiForeignTraceDummyThoughtSignature :: Text
geminiForeignTraceDummyThoughtSignature =
    "context_engineering_is_the_way_to_go"

genericGeminiFunctionCallValue :: Bool -> Text -> Text -> Map Text Value -> Value
genericGeminiFunctionCallValue includeDummyThoughtSignature toolCallId toolName toolArgs =
    object $
        [ "type" .= ("function_call" :: Text)
        , "id" .= toolCallId
        , "name" .= toolName
        , "arguments" .= toolArgs
        ]
            <> [ "thought_signature" .= geminiForeignTraceDummyThoughtSignature
               | includeDummyThoughtSignature
               ]

registerToolCall :: Text -> Text -> RenderedGeminiHistory -> RenderedGeminiHistory
registerToolCall toolCallId toolName renderedHistory@RenderedGeminiHistory{toolCallNames} =
    renderedHistory{toolCallNames = Map.insert toolCallId toolName toolCallNames}

renderedTurnRole :: Object -> Maybe Text
renderedTurnRole turnObject =
    KM.lookup "role" turnObject >>= \case
        String turnRoleText ->
            Just turnRoleText
        _ ->
            Nothing

appendGeminiTurnBlocks :: Text -> [Value] -> RenderedGeminiHistory -> RenderedGeminiHistory
appendGeminiTurnBlocks role contentParts history =
    foldl' (\accumulatedHistory content -> appendGeminiTurnBlock role content accumulatedHistory) history contentParts

splitRenderableGeminiHistory :: [HistoryItem] -> (Maybe HistoryItem, [HistoryItem])
splitRenderableGeminiHistory history =
    ( latestGeminiSystemSnapshot history
    , filter (not . isGeminiSystemHistoryItem) history
    )

latestGeminiSystemSnapshot :: [HistoryItem] -> Maybe HistoryItem
latestGeminiSystemSnapshot =
    viaNonEmpty last . filter isGeminiSystemHistoryItem

isGeminiSystemHistoryItem :: HistoryItem -> Bool
isGeminiSystemHistoryItem HistoryItem{genericItem = GenericMessage{role = GenericSystem}} =
    True
isGeminiSystemHistoryItem _ =
    False

historyItemSystemInstruction
    :: Error RakeError :> es
    => HistoryItem
    -> Eff es Text
historyItemSystemInstruction HistoryItem{genericItem = GenericMessage{role = GenericSystem, parts}} =
    messagePartsToText parts
historyItemSystemInstruction _ =
    throwError (LlmExpectationError "Gemini system instruction can only be rendered from a system message")

messagePartsToText
    :: Error RakeError :> es
    => [MessagePart]
    -> Eff es Text
messagePartsToText =
    fmap mconcat . traverse partText
  where
    partText = \case
        PartText{text} ->
            pure text
        PartRefusal{text} ->
            pure text
        PartImage{} ->
            throwError (LlmExpectationError "Generic media message parts require a blob resolver before they can be rendered to provider input")
        PartAudio{} ->
            throwError (LlmExpectationError "Generic media message parts require a blob resolver before they can be rendered to provider input")
        PartFile{} ->
            throwError (LlmExpectationError "Generic media message parts require a blob resolver before they can be rendered to provider input")

messagePartsToGeminiContentParts
    :: ( Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => ProviderApiFamily
    -> [MessagePart]
    -> Eff es [Value]
messagePartsToGeminiContentParts providerFamily =
    traverse (messagePartToGeminiContentPart providerFamily)
  where
    messagePartToGeminiContentPart targetProviderFamily = \case
        PartText{text} ->
            pure (textContentValue text)
        PartRefusal{text} ->
            pure (textContentValue text)
        PartImage{blobId} ->
            resolveMediaContentPart targetProviderFamily blobId
        PartAudio{blobId} ->
            resolveMediaContentPart targetProviderFamily blobId
        PartFile{blobId} ->
            resolveMediaContentPart targetProviderFamily blobId

    resolveMediaContentPart targetProviderFamily blobId = do
        maybeRequestPart <- lookupMediaReference targetProviderFamily blobId
        case maybeRequestPart of
            Just requestPart ->
                pure requestPart
            Nothing ->
                let MediaBlobId blobIdText = blobId
                 in
                throwError
                    ( LlmExpectationError
                        ( "No stored media reference is available for blob "
                            <> toString blobIdText
                            <> " when rendering "
                            <> toString (providerApiFamilyText targetProviderFamily)
                        )
                    )

annotatePendingAssistantContentParts :: ItemLifecycle -> [Value] -> [Value]
annotatePendingAssistantContentParts lifecycle contentParts
    | lifecycle == ItemPending =
        textContentValue "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n" : contentParts
    | otherwise =
        contentParts

textContentValue :: Text -> Value
textContentValue text =
    object
        [ "type" .= ("text" :: Text)
        , "text" .= text
        ]

geminiToolResultValue :: ToolResponse -> Value
geminiToolResultValue = \case
    ToolResponseText{text} ->
        geminiToolResultTextParts text
    ToolResponseJson{json} ->
        geminiToolResultTextParts (valueToCompactText json)

geminiToolResultTextParts :: Text -> Value
geminiToolResultTextParts text =
    toJSON
        ( [ object
                [ "type" .= ("text" :: Text)
                , "text" .= text
                ]
          ]
            :: [Value]
        )

localToolDeclarationToGeminiTool :: ToolDeclaration -> Value
localToolDeclarationToGeminiTool ToolDeclaration{name, description, parameterSchema} =
    object $
        [ "type" .= ("function" :: Text)
        , "name" .= name
        , "description" .= description
        , "parameters" .= fromMaybe emptyToolParametersSchema parameterSchema
        ]

emptyToolParametersSchema :: Value
emptyToolParametersSchema =
    object []

responseFormatSchema :: ResponseFormat -> Maybe Value
responseFormatSchema = \case
    Unstructured ->
        Nothing
    JsonValue ->
        Just $
            object
                [ "type" .= ("object" :: Text)
                , "additionalProperties" .= True
                ]
    JsonSchema schema ->
        Just schema

hasStructuredResponseFormat :: ResponseFormat -> Bool
hasStructuredResponseFormat = \case
    Unstructured ->
        False
    JsonValue ->
        True
    JsonSchema{} ->
        True

generationConfigValue :: GeminiGenerationConfig -> SamplingOptions -> Maybe Value
generationConfigValue GeminiGenerationConfig{temperature, topP, seed, stopSequences, thinkingLevel, thinkingSummaries, maxOutputTokens, toolChoice} SamplingOptions{temperature = samplingTemperature, topP = samplingTopP} =
    if null generationFields
        then Nothing
        else Just (object generationFields)
  where
    generationFields =
        catMaybes
            [ ("temperature" .=) <$> (samplingTemperature <|> temperature)
            , ("top_p" .=) <$> (samplingTopP <|> topP)
            , ("seed" .=) <$> seed
            , if null stopSequences
                then Nothing
                else Just ("stop_sequences" .= stopSequences)
            , ("thinking_level" .=) <$> thinkingLevel
            , ("thinking_summaries" .=) <$> thinkingSummaries
            , ("max_output_tokens" .=) <$> maxOutputTokens
            , ("tool_choice" .=) <$> toolChoice
            ]

decodeGeminiResponse :: Value -> Either RakeError ProviderRound
decodeGeminiResponse responseValue = do
    responseObject <- expectObject "interaction" responseValue
    let interactionStatus = lookupText "status" responseObject
    outputs <- case KM.lookup "outputs" responseObject of
        Just outputsValue ->
            expectArray "interaction.outputs" outputsValue
        Nothing ->
            Right Vector.empty
    let outputPayloads = Vector.toList outputs
        interactionExchangeId =
            lookupText "id" responseObject
                <|> geminiFallbackExchangeId outputPayloads
        classifiedOutputItems =
            classifyGeminiPayloads outputPayloads
        projectionNotes =
            concatMap (\(_, notes, _) -> notes) classifiedOutputItems
        projectedItems =
            [ canonicalItem
            | (_, _, canonicalItem) <- classifiedOutputItems
            ]
        toolCalls = collectToolCalls projectedItems
        roundAction =
            geminiRoundAction
                interactionStatus
                projectedItems
                toolCalls
                projectionNotes
        roundItemLifecycle = providerRoundItemLifecycle roundAction
    roundItems <- forM classifiedOutputItems $ \(payload, _, canonicalItem) -> do
        let rawProviderItem =
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , exchangeId = interactionExchangeId
                    , nativeItemId =
                        case payload of
                            Object payloadObject ->
                                geminiNativeItemId payloadObject
                            _ ->
                                Nothing
                    , payload
                    }
        pure $
            case canonicalItem of
                GenericNonPortable ->
                    nonPortableHistoryItem roundItemLifecycle rawProviderItem
                _ ->
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = roundItemLifecycle
                        , genericItem = canonicalItem
                        , providerItem = Just rawProviderItem
                        }
    pure ProviderRound{roundItems, mediaReferences = [], action = roundAction}

geminiRoundAction
    :: Maybe Text
    -> [GenericItem]
    -> [ToolCall]
    -> [Text]
    -> ProviderRoundAction
geminiRoundAction interactionStatus projectedItems toolCalls projectionNotes
    | Just failureAction <- interactionStatus >>= geminiTerminalAction =
        failureAction
    | not (null toolCalls) =
        ProviderRoundNeedsLocalTools toolCalls
    | Just "requires_action" <- interactionStatus =
        ProviderRoundFailed $
            FailureContract $
                appendProjectionNotes
                    "Gemini interaction required local tools but returned no projected tool calls"
                    projectionNotes
    | Just statusAction <- geminiStatusAction =<< interactionStatus =
        statusAction "Gemini interaction status was "
    | hasAssistantMessage projectedItems =
        ProviderRoundDone
    | otherwise =
        ProviderRoundFailed $
            FailureContract $
                appendProjectionNotes
                    "Gemini interaction completed without tool calls or assistant message"
                    projectionNotes

geminiTerminalAction :: Text -> Maybe ProviderRoundAction
geminiTerminalAction interactionStatus = case interactionStatus of
    "failed" ->
        Just (ProviderRoundFailed (FailureProvider ("Gemini interaction status was " <> interactionStatus)))
    "cancelled" ->
        Just (ProviderRoundFailed (FailureProvider ("Gemini interaction status was " <> interactionStatus)))
    "canceled" ->
        Just (ProviderRoundFailed (FailureProvider ("Gemini interaction status was " <> interactionStatus)))
    "expired" ->
        Just (ProviderRoundFailed (FailureProvider ("Gemini interaction status was " <> interactionStatus)))
    _ ->
        Nothing

geminiStatusAction :: Text -> Maybe (Text -> ProviderRoundAction)
geminiStatusAction = \case
    "completed" ->
        Nothing
    "requires_action" ->
        Nothing
    "incomplete" ->
        Just (ProviderRoundPaused . PauseIncomplete . (<> "incomplete"))
    "queued" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "queued"))
    "in_progress" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "in_progress"))
    "pending" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "pending"))
    "processing" ->
        Just (ProviderRoundPaused . PauseProviderWaiting . (<> "processing"))
    "failed" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "failed"))
    "cancelled" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "cancelled"))
    "canceled" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "canceled"))
    "expired" ->
        Just (ProviderRoundFailed . FailureProvider . (<> "expired"))
    otherStatus ->
        Just (\_ -> ProviderRoundFailed (FailureContract ("Unsupported Gemini interaction status: " <> otherStatus)))

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

geminiNativeItemId :: Object -> Maybe Text
geminiNativeItemId payloadObject =
    lookupText "id" payloadObject
        <|> lookupText "call_id" payloadObject
        <|> lookupText "signature" payloadObject

geminiFallbackExchangeId :: [Value] -> Maybe Text
geminiFallbackExchangeId =
    viaNonEmpty head . mapMaybe payloadNativeItemId
  where
    payloadNativeItemId = \case
        Object payloadObject ->
            geminiNativeItemId payloadObject
        _ ->
            Nothing

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
