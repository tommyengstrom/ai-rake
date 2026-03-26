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
import Data.Map qualified as Map
import Data.Vector qualified as Vector
import Effectful
import Effectful.Dispatch.Dynamic (interpretWith)
import Effectful.Error.Static
import Network.HTTP.Client.TLS (newTlsManager)
import Rake.Effect
import Rake.Providers.Chat.Projection (historyItemToGenericItems, historyItemsToGenericItems)
import Rake.Providers.Internal (defaultWarningLogger, valueToCompactText)
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
       )
    => GeminiChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeGeminiChat settings@GeminiChatSettings{apiKey, baseUrl, requestLogger} eff = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postInteraction = client geminiInteractionsApi

    interpretWith eff \_ -> \case
        GetLlmResponse tools responseFormat samplingOptions history -> do
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
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildGeminiRequestBody
    :: Error RakeError :> es
    => GeminiChatSettings es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> SamplingOptions
    -> [HistoryItem]
    -> Eff es Value
buildGeminiRequestBody GeminiChatSettings{model, providerTools, systemInstruction, generationConfig, requestLogger} tools responseFormat samplingOptions history = do
    renderedHistory <- renderGeminiHistory requestLogger history
    let RenderedGeminiHistory
            { sawNonLeadingInstruction = hasNonLeadingInstruction
            , renderedTurns = renderedTurnValues
            } = renderedHistory
    when
        hasNonLeadingInstruction
        ( requestLogger
            ( NativeConversionNote
                "Gemini moved non-leading system/developer messages into system_instruction"
            )
        )
    pure $
        object $
            [ "model" .= model
            , "input" .= reverse renderedTurnValues
            , "store" .= False
            ]
                <> catMaybes
                    [ ("system_instruction" .=) <$> combinedSystemInstruction systemInstruction renderedHistory
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
    :: Error RakeError :> es
    => (NativeMsgFormat -> Eff es ())
    -> [HistoryItem]
    -> Eff es RenderedGeminiHistory
renderGeminiHistory requestLogger =
    foldlM (renderGeminiHistoryItem requestLogger) initialRenderedGeminiHistory

renderGeminiHistoryItem
    :: Error RakeError :> es
    => (NativeMsgFormat -> Eff es ())
    -> RenderedGeminiHistory
    -> HistoryItem
    -> Eff es RenderedGeminiHistory
renderGeminiHistoryItem requestLogger renderedHistory historyEntry = case historyEntry of
    HProvider ProviderHistoryItem
        { apiFamily = ProviderGeminiInteractions
        , itemLifecycle
        , nativeItem = NativeProviderItem{payload}
        }
        | shouldReuseGeminiNativePayload itemLifecycle payload ->
            pure (appendGeminiNativePayload payload renderedHistory)
    fallbackHistoryItem -> do
        let (notes, genericItems) = historyItemToGenericItems fallbackHistoryItem
        traverse_ (requestLogger . NativeConversionNote) notes
        foldlM renderGeminiGenericItem renderedHistory genericItems

shouldReuseGeminiNativePayload :: ItemLifecycle -> Value -> Bool
shouldReuseGeminiNativePayload itemLifecycle payload =
    (itemLifecycle == ItemCompleted && isGeminiAssistantPayload payload)
        || (itemLifecycle == ItemPending && (isGeminiFunctionCallPayload payload || isGeminiThoughtPayload payload))

isGeminiAssistantPayload :: Value -> Bool
isGeminiAssistantPayload = \case
    Object payloadObject ->
        lookupText "text" payloadObject /= Nothing
            && isGeminiTextType (lookupText "type" payloadObject)
    _ ->
        False

isGeminiTextType :: Maybe Text -> Bool
isGeminiTextType = \case
    Nothing ->
        True
    Just "text" ->
        True
    _ ->
        False

isGeminiFunctionCallPayload :: Value -> Bool
isGeminiFunctionCallPayload = \case
    Object payloadObject ->
        lookupText "type" payloadObject == Just "function_call"
    _ ->
        False

isGeminiThoughtPayload :: Value -> Bool
isGeminiThoughtPayload = \case
    Object payloadObject ->
        lookupText "type" payloadObject == Just "thought"
    _ ->
        False

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
    { instructionBlocks :: [Text]
    , sawConversationTurn :: Bool
    , sawNonLeadingInstruction :: Bool
    , renderedTurns :: [Value]
    , toolCallNames :: Map Text Text
    , openModelTurnHasFunctionCall :: Bool
    }

initialRenderedGeminiHistory :: RenderedGeminiHistory
initialRenderedGeminiHistory =
    RenderedGeminiHistory
        { instructionBlocks = []
        , sawConversationTurn = False
        , sawNonLeadingInstruction = False
        , renderedTurns = []
        , toolCallNames = mempty
        , openModelTurnHasFunctionCall = False
        }

combinedSystemInstruction :: Maybe Text -> RenderedGeminiHistory -> Maybe Text
combinedSystemInstruction settingsInstruction RenderedGeminiHistory{instructionBlocks} =
    if null allBlocks
        then Nothing
        else Just (mconcat (intersperse "\n\n" allBlocks))
  where
    allBlocks = catMaybes [settingsInstruction] <> instructionBlocks

renderGeminiGenericItem
    :: Error RakeError :> es
    => RenderedGeminiHistory
    -> GenericItem
    -> Eff es RenderedGeminiHistory
renderGeminiGenericItem renderedHistory = \case
    GenericMessage{role = GenericSystem, parts} ->
        pure (appendGeminiInstruction GenericSystem (messagePartsToText parts) renderedHistory)
    GenericMessage{role = GenericDeveloper, parts} ->
        pure (appendGeminiInstruction GenericDeveloper (messagePartsToText parts) renderedHistory)
    GenericMessage{role = GenericUser, parts} ->
        pure (appendGeminiTurnBlock "user" (textContentValue (messagePartsToText parts)) renderedHistory)
    GenericMessage{role = GenericAssistant, parts, itemLifecycle} ->
        pure
            ( appendGeminiTurnBlock
                "model"
                (textContentValue (annotatePendingAssistantText itemLifecycle (messagePartsToText parts)))
                renderedHistory
            )
    GenericToolCall{toolCall = ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs}} ->
        let RenderedGeminiHistory{openModelTurnHasFunctionCall = sawFunctionCallInCurrentModelTurn} =
                renderedHistory
            shouldInjectDummyThoughtSignature =
                not sawFunctionCallInCurrentModelTurn
         in
            pure $
                appendGeminiTurnBlock
                    "model"
                    (genericGeminiFunctionCallValue shouldInjectDummyThoughtSignature toolCallId toolName toolArgs)
                    (registerToolCall toolCallId toolName renderedHistory)
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

appendGeminiInstruction :: GenericRole -> Text -> RenderedGeminiHistory -> RenderedGeminiHistory
appendGeminiInstruction role text renderedHistory@RenderedGeminiHistory{instructionBlocks, sawConversationTurn, sawNonLeadingInstruction} =
    renderedHistory
        { instructionBlocks = instructionBlocks <> [instructionBlock role text]
        , sawNonLeadingInstruction = sawNonLeadingInstruction || sawConversationTurn
        }

appendGeminiTurnBlock :: Text -> Value -> RenderedGeminiHistory -> RenderedGeminiHistory
appendGeminiTurnBlock role content renderedHistory@RenderedGeminiHistory{renderedTurns, openModelTurnHasFunctionCall} =
    renderedHistory
        { sawConversationTurn = True
        , renderedTurns = appendGeminiTurn role content renderedTurns
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

appendGeminiNativePayload :: Value -> RenderedGeminiHistory -> RenderedGeminiHistory
appendGeminiNativePayload payload renderedHistory =
    maybe
        (appendGeminiTurnBlock "model" payload renderedHistory)
        (\(toolCallId, toolName) -> appendGeminiTurnBlock "model" payload (registerToolCall toolCallId toolName renderedHistory))
        (geminiToolCallDetails payload)

-- Gemini 3 function calling validates the current-turn function call against a
-- thought signature. When replaying a foreign tool continuation into Gemini,
-- there is no provider-issued signature to preserve, so we attach Google's
-- documented dummy signature to the first replayed generic function_call in
-- that step. Same-provider Gemini continuation keeps replaying the original
-- native thought/function_call payloads unchanged.
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

geminiToolCallDetails :: Value -> Maybe (Text, Text)
geminiToolCallDetails = \case
    Object payloadObject
        | lookupText "type" payloadObject == Just "function_call" -> do
            toolCallId <- lookupText "id" payloadObject <|> lookupText "call_id" payloadObject
            toolName <- lookupText "name" payloadObject
            pure (toolCallId, toolName)
    _ ->
        Nothing

instructionBlock :: GenericRole -> Text -> Text
instructionBlock role content =
    roleLabel role <> ":\n" <> content
  where
    roleLabel = \case
        GenericSystem ->
            "System"
        GenericDeveloper ->
            "Developer"
        GenericUser ->
            "User"
        GenericAssistant ->
            "Assistant"

messagePartsToText :: [MessagePart] -> Text
messagePartsToText =
    foldMap partText
  where
    partText = \case
        PartText{text} ->
            text

annotatePendingAssistantText :: ItemLifecycle -> Text -> Text
annotatePendingAssistantText itemLifecycle text
    | itemLifecycle == ItemPending =
        "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n" <> text
    | otherwise =
        text

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
        classificationHistoryItems =
            [ HProvider
                ProviderHistoryItem
                    { apiFamily = ProviderGeminiInteractions
                    , itemLifecycle = ItemCompleted
                    , nativeItem =
                        NativeProviderItem
                            { exchangeId = interactionExchangeId
                            , nativeItemId = Nothing
                            , payload
                            }
                    }
            | payload <- outputPayloads
            ]
        (projectionNotes, projectedItems) =
            historyItemsToGenericItems classificationHistoryItems
        toolCalls = collectToolCalls projectedItems
        roundAction =
            geminiRoundAction
                interactionStatus
                projectedItems
                toolCalls
                projectionNotes
        roundItemLifecycle = providerRoundItemLifecycle roundAction
    roundItems <- forM outputPayloads $ \payload -> do
        payloadObject <- expectObject "interaction output" payload
        pure $
            HProvider
                ProviderHistoryItem
                    { apiFamily = ProviderGeminiInteractions
                    , itemLifecycle = roundItemLifecycle
                    , nativeItem =
                        NativeProviderItem
                            { exchangeId = interactionExchangeId
                            , nativeItemId = geminiNativeItemId payloadObject
                            , payload
                            }
                    }
    pure ProviderRound{roundItems, action = roundAction}

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
