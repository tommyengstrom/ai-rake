module Rake.Provider.ResponsesRenderSpec where

import Control.Exception (ErrorCall (ErrorCall), finally)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Error.Static
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Rake
import Rake.Providers.Gemini.Chat
import Rake.Providers.OpenAI.Chat
import Rake.Providers.XAI.Chat
import Relude
import Servant.Client (ClientError (ConnectionError))
import System.Directory (removeFile)
import System.IO qualified as IO
import Test.Hspec

spec :: Spec
spec = describe "Responses request rendering" $ do
    describe "schema preservation" $ do
        it "preserves raw JsonSchema values verbatim" $ do
            let rawSchema =
                    object
                        [ "type" .= ("object" :: Text)
                        , "additionalProperties" .= True
                        ]

            requestBody <-
                captureOpenAIRequestBody
                    (withResponseFormat (JsonSchema rawSchema) defaultChatConfig)
                    [user "hello"]

            lookupPath ["text", "format", "schema"] requestBody `shouldBe` Just rawSchema
            lookupPath ["text", "format", "strict"] requestBody `shouldBe` Nothing

        it "preserves custom raw tool parameterSchema values verbatim" $ do
            let rawSchema =
                    object
                        [ "type" .= ("object" :: Text)
                        , "additionalProperties" .= True
                        ]
                tool =
                    ToolDef
                        { name = "raw_tool"
                        , description = "Raw schema tool"
                        , parameterSchema = Just rawSchema
                        , executeFunction = \_ -> pure (Right "ok")
                        }

            requestBody <-
                captureOpenAIRequestBody
                    (withTools [tool] defaultChatConfig)
                    [user "hello"]

            firstToolParameters requestBody `shouldBe` Just rawSchema

        it "renders the no-argument tool fallback as a closed object with required []" $ do
            let tool =
                    defineToolNoArgument "noop" "No-op tool" (pure (Right "ok"))

            requestBody <-
                captureOpenAIRequestBody
                    (withTools [tool] defaultChatConfig)
                    [user "hello"]

            firstToolParameters requestBody
                `shouldBe` Just
                    ( object
                        [ "type" .= ("object" :: Text)
                        , "properties" .= object []
                        , "required" .= ([] :: [Text])
                        , "additionalProperties" .= False
                        ]
                    )

    describe "sampling options" $ do
        it "renders temperature when explicitly configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    (withSampling (withTemperature (Just 0) defaultSamplingOptions) defaultChatConfig)
                    [user "hello"]

            lookupPath ["temperature"] requestBody `shouldBe` Just (Number 0)

        it "omits temperature when not configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [user "hello"]

            lookupPath ["temperature"] requestBody `shouldBe` Nothing

        it "renders top_p when explicitly configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    (withSampling (withTopP (Just 0.1) defaultSamplingOptions) defaultChatConfig)
                    [user "hello"]

            lookupPath ["top_p"] requestBody `shouldBe` Just (Number 0.1)

        it "omits top_p when not configured" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [user "hello"]

            lookupPath ["top_p"] requestBody `shouldBe` Nothing

    describe "native history rendering" $ do
        it "reuses native OpenAI items unchanged for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([nativeResponsesAssistantPayload] :: [Value]))

        it "projects xAI-native items into generic input for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "reuses native xAI items unchanged for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([nativeResponsesAssistantPayload] :: [Value]))

        it "projects OpenAI-native items into generic input for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "annotates pending OpenAI-native assistant text even for OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingOpenAiNativeItem nativeResponsesAssistantPayload]

            notes `shouldBe` ["Rendered pending assistant text as annotated assistant content for Responses input"]
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedPendingAssistantMessage] :: [Value]))

        it "annotates pending OpenAI-native assistant text without a type field" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingOpenAiNativeItem legacyResponsesAssistantPayload]

            notes `shouldBe` ["Rendered pending assistant text as annotated assistant content for Responses input"]
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedPendingAssistantMessage] :: [Value]))

        it "reuses native Gemini items unchanged for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [geminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content" .= ([nativeGeminiTextPayload] :: [Value])
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "projects Gemini-native items into generic input for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [geminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "annotates pending Gemini-native assistant text when projecting into OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingGeminiNativeItem nativeGeminiTextPayload]

            notes `shouldBe` ["Rendered pending assistant text as annotated assistant content for Responses input"]
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedPendingAssistantMessage] :: [Value]))

        it "annotates pending Gemini-native assistant text even for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content" .= ([pendingGeminiTextContent] :: [Value])
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "annotates pending Gemini-native assistant text without a type field even for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiNativeItem legacyGeminiTextPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content" .= ([pendingGeminiTextContent] :: [Value])
                                ]
                          ]
                            :: [Value]
                        )
                    )

    describe "multipart shared rendering" $ do
        it "renders shared history without conversion notes for OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedHistoryRequest)

        it "renders shared history without conversion notes for xAI requests" $ do
            (requestBody, notes) <-
                captureXAIRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedHistoryRequest)

        it "renders shared history for Gemini with instruction collapsing" $ do
            (requestBody, notes) <-
                captureGeminiRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["system_instruction"] requestBody `shouldBe` Just (String sharedGeminiSystemInstruction)
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedGeminiHistoryRequest)

        it "warns when Gemini must move non-leading instructions into system_instruction" $ do
            (_requestBody, notes) <-
                captureGeminiRender
                    defaultChatConfig
                    [user "hello", system "late system message"]

            notes `shouldBe` ["Gemini moved non-leading system/developer messages into system_instruction"]

        it "renders JSON tool results as JSON text for provider requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ toolResultJson "tool-call-1" (String "ok")
                    , toolResultJson "tool-call-2" (object ["answer" .= (4 :: Int)])
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "type" .= ("function_call_output" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "output" .= ("\"ok\"" :: Text)
                                ]
                          , object
                                [ "type" .= ("function_call_output" :: Text)
                                , "call_id" .= ("tool-call-2" :: Text)
                                , "output" .= ("{\"answer\":4}" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

    describe "tool response parsing" $ do
        it "parses any valid JSON tool output string as JSON" $ do
            forM_ scalarAndCompositeJsonCases $ \outputText ->
                itemTexts (openAiNativeItem (nativeToolResultPayload outputText))
                    `shouldBe` []

        it "keeps non-JSON tool output strings as text" $ do
            itemTexts (openAiNativeItem (nativeToolResultPayload "hello"))
                `shouldBe` ["hello"]

        it "parses Gemini function result strings as text" $ do
            itemTexts (geminiNativeItem (nativeGeminiToolResultPayload "hello"))
                `shouldBe` ["hello"]

    describe "response decoding" $ do
        it "decodes completed OpenAI assistant responses as completed rounds" $ do
            let payload = responsesAssistantPayload "item-openai" "native assistant text"

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai") payload]
                        , action = ProviderRoundDone
                        }

        it "decodes completed xAI assistant responses as completed rounds" $ do
            let payload = responsesAssistantPayload "item-xai" "native assistant text"

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderXAIResponses ItemCompleted "response-xai" (Just "item-xai") payload]
                        , action = ProviderRoundDone
                        }

        it "marks OpenAI tool handoff rounds as pending even when the response is still in progress" $ do
            let payload =
                    responsesToolCallPayload
                        "item-openai-tool"
                        "tool-call-1"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                        "in_progress"
                expectedToolCall =
                    ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "in_progress" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-tool") payload]
                        , action = ProviderRoundNeedsLocalTools [expectedToolCall]
                        }

        it "marks xAI tool handoff rounds as pending even when the item status is still in progress" $ do
            let payload =
                    responsesToolCallPayload
                        "item-xai-tool"
                        "tool-call-1"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                        "in_progress"
                expectedToolCall =
                    ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderXAIResponses ItemPending "response-xai" (Just "item-xai-tool") payload]
                        , action = ProviderRoundNeedsLocalTools [expectedToolCall]
                        }

        it "keeps incomplete OpenAI responses in pending history instead of completing them" $ do
            let payload = responsesAssistantPayload "item-openai" "working on it"

            decodeOpenAIResponse (responsesResponse "response-openai" "incomplete" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai") payload]
                        , action = ProviderRoundPaused (PauseIncomplete "Responses response status was incomplete")
                        }

        it "fails terminal xAI responses" $ do
            decodeXAIResponse (responsesResponse "response-xai" "failed" [])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = []
                        , action = ProviderRoundFailed (FailureProvider "Responses response status was failed")
                        }

        it "fails mixed Responses rounds when any item has terminal failure status, even if a tool call is present" $ do
            let toolPayload =
                    responsesToolCallPayload
                        "item-openai-tool"
                        "tool-call-1"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                        "completed"
                failedPayload =
                    responsesAssistantPayloadWithStatus
                        "item-openai-message"
                        "native assistant text"
                        "failed"

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [toolPayload, failedPayload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems =
                            [ nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-tool") toolPayload
                            , nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-message") failedPayload
                            ]
                        , action = ProviderRoundFailed (FailureProvider "Responses output item status was failed")
                        }

        it "decodes completed Gemini assistant responses as completed rounds" $ do
            let payload = geminiTextPayloadWithId "item-gemini" "native assistant text"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderGeminiInteractions ItemCompleted "interaction-gemini" (Just "item-gemini") payload]
                        , action = ProviderRoundDone
                        }

        it "decodes Gemini requires_action rounds as pending tool handoff" $ do
            let payload =
                    geminiFunctionCallPayload
                        "item-gemini-tool"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                expectedToolCall =
                    ToolCall
                        { toolCallId = "item-gemini-tool"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        }

            decodeGeminiResponse (geminiResponse "interaction-gemini" "requires_action" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini-tool") payload]
                        , action = ProviderRoundNeedsLocalTools [expectedToolCall]
                        }

        it "pauses Gemini in-progress rounds" $ do
            let payload = geminiTextPayloadWithId "item-gemini" "working on it"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "in_progress" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini") payload]
                        , action = ProviderRoundPaused (PauseProviderWaiting "Gemini interaction status was in_progress")
                        }

        it "fails terminal Gemini responses" $ do
            decodeGeminiResponse (geminiResponse "interaction-gemini" "failed" [])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = []
                        , action = ProviderRoundFailed (FailureProvider "Gemini interaction status was failed")
                        }

        it "fails completed Gemini thought-only rounds as contract errors" $ do
            let payload = geminiThoughtPayload "thought-1"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
                `shouldBe` Right
                    ProviderRound
                        { historyItems = [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "thought-1") payload]
                        , action =
                            ProviderRoundFailed
                                ( FailureContract
                                    "Gemini interaction completed without tool calls or assistant message. Projection notes: Dropped Gemini thought content during generic projection"
                                )
                        }

    describe "default logging" $ do
        it "warns on OpenAI conversion notes and request failures" $ do
            output <-
                captureStderrText do
                    let settings :: OpenAIChatSettings '[IOE]
                        settings = defaultOpenAIChatSettings "test-api-key"
                        OpenAIChatSettings{requestLogger = logger} = settings
                    runEff do
                        logger (NativeConversionNote "Dropped unsupported item")
                        logger (NativeRequestFailure (ConnectionError (toException (ErrorCall "boom"))))

            output `shouldSatisfy` T.isInfixOf "[ai-rake:openai.chat] Dropped unsupported item"
            output `shouldSatisfy` T.isInfixOf "[ai-rake:openai.chat] Provider request failed:"

        it "warns on xAI conversion notes" $ do
            output <-
                captureStderrText do
                    let settings :: XAIChatSettings '[IOE]
                        settings = defaultXAIChatSettings "test-api-key"
                        XAIChatSettings{requestLogger = logger} = settings
                    runEff $
                        logger (NativeConversionNote "Dropped unsupported item")

            output `shouldSatisfy` T.isInfixOf "[ai-rake:xai.chat] Dropped unsupported item"

        it "keeps raw request and response bodies silent by default" $ do
            output <-
                captureStderrText do
                    let openAiSettings :: OpenAIChatSettings '[IOE]
                        openAiSettings = defaultOpenAIChatSettings "test-api-key"
                        xaiSettings :: XAIChatSettings '[IOE]
                        xaiSettings = defaultXAIChatSettings "test-api-key"
                        OpenAIChatSettings{requestLogger = openAiLogger} = openAiSettings
                        XAIChatSettings{requestLogger = xaiLogger} = xaiSettings
                    runEff do
                        openAiLogger (NativeMsgOut (object ["hello" .= ("world" :: Text)]))
                        openAiLogger (NativeMsgIn (object ["ok" .= True]))
                        xaiLogger (NativeMsgOut (object ["hello" .= ("world" :: Text)]))
                        xaiLogger (NativeMsgIn (object ["ok" .= True]))

            T.strip output `shouldBe` ""

withTools :: [ToolDef es] -> ChatConfig es -> ChatConfig es
withTools tools ChatConfig{responseFormat, sampling, onItem, maxToolRounds} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds}

withResponseFormat :: ResponseFormat -> ChatConfig es -> ChatConfig es
withResponseFormat responseFormat ChatConfig{tools, sampling, onItem, maxToolRounds} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds}

withSampling :: SamplingOptions -> ChatConfig es -> ChatConfig es
withSampling sampling ChatConfig{tools, responseFormat, onItem, maxToolRounds} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds}

withTemperature :: Maybe Double -> SamplingOptions -> SamplingOptions
withTemperature temperature SamplingOptions{topP} =
    SamplingOptions{temperature, topP}

withTopP :: Maybe Double -> SamplingOptions -> SamplingOptions
withTopP topP SamplingOptions{temperature} =
    SamplingOptions{temperature, topP}

captureOpenAIRequestBody
    :: ChatConfig '[Rake, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBody chatConfig history = do
    (requestBody, _) <- captureOpenAIRender chatConfig history
    pure requestBody

captureOpenAIRender
    :: ChatConfig '[Rake, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let OpenAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , organizationId = defaultOrganizationId
            , projectId = defaultProjectId
            } = defaultOpenAIChatSettings "test-api-key"
        settings :: OpenAIChatSettings '[Error RakeError, IOE]
        settings =
                OpenAIChatSettings
                    { apiKey = defaultApiKey
                    , model = defaultModel
                    , baseUrl = unreachableBaseUrl
                    , organizationId = defaultOrganizationId
                    , projectId = defaultProjectId
                    , requestLogger = recordRequestAndNotes requestRef notesRef
                    }

    result <-
        runEff
            . runErrorNoCallStack
            $ runRakeOpenAIChat settings
            $ void
            $ chat chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

captureXAIRequestBody
    :: ChatConfig '[Rake, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureXAIRequestBody chatConfig history = do
    (requestBody, _) <- captureXAIRender chatConfig history
    pure requestBody

captureXAIRender
    :: ChatConfig '[Rake, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureXAIRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let XAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            } = defaultXAIChatSettings "test-api-key"
        settings :: XAIChatSettings '[Error RakeError, IOE]
        settings =
            XAIChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , requestLogger = recordRequestAndNotes requestRef notesRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            $ runRakeXAIChat settings
            $ void
            $ chat chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

captureGeminiRequestBody
    :: ChatConfig '[Rake, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureGeminiRequestBody chatConfig history = do
    (requestBody, _) <- captureGeminiRender chatConfig history
    pure requestBody

captureGeminiRender
    :: ChatConfig '[Rake, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureGeminiRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let GeminiChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , providerTools = defaultProviderTools
            , generationConfig = defaultGenerationConfig
            } = defaultGeminiChatSettings "test-api-key"
        settings :: GeminiChatSettings '[Error RakeError, IOE]
        settings =
            GeminiChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , systemInstruction = Nothing
                , providerTools = defaultProviderTools
                , generationConfig = defaultGenerationConfig
                , requestLogger = recordRequestAndNotes requestRef notesRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            $ runRakeGeminiChat settings
            $ void
            $ chat chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

recordRequest :: IOE :> es => IORef.IORef (Maybe Value) -> NativeMsgFormat -> Eff es ()
recordRequest requestRef = \case
    NativeMsgOut requestBody ->
        liftIO $ IORef.writeIORef requestRef (Just requestBody)
    _ ->
        pure ()

recordRequestAndNotes
    :: IOE :> es
    => IORef.IORef (Maybe Value)
    -> IORef.IORef [Text]
    -> NativeMsgFormat
    -> Eff es ()
recordRequestAndNotes requestRef notesRef = \case
    NativeMsgOut requestBody ->
        liftIO $ IORef.writeIORef requestRef (Just requestBody)
    NativeConversionNote note ->
        liftIO $ IORef.modifyIORef' notesRef (<> [note])
    _ ->
        pure ()

readRequest :: IORef.IORef (Maybe Value) -> IO Value
readRequest requestRef = do
    maybeRequest <- IORef.readIORef requestRef
    case maybeRequest of
        Just requestBody ->
            pure requestBody
        Nothing -> do
            expectationFailure "Expected request body to be captured before the HTTP failure"
            fail "request body not captured"

captureStderrText :: IO a -> IO Text
captureStderrText action = do
    originalStderr <- hDuplicate IO.stderr
    (tempPath, tempHandle) <- IO.openTempFile "/tmp" "ai-rake-stderr"

    hDuplicateTo tempHandle IO.stderr
    _ <-
        action `finally` do
            IO.hFlush IO.stderr
            hDuplicateTo originalStderr IO.stderr
            IO.hClose originalStderr
            IO.hClose tempHandle

    output <- TIO.readFile tempPath
    removeFile tempPath
    pure output

unreachableBaseUrl :: Text
unreachableBaseUrl = "http://127.0.0.1:1"

nativeResponsesAssistantPayload :: Value
nativeResponsesAssistantPayload =
    object
        [ "id" .= ("native-message-1" :: Text)
        , "type" .= ("message" :: Text)
        , "role" .= ("assistant" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= ("native assistant text" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]

legacyResponsesAssistantPayload :: Value
legacyResponsesAssistantPayload =
    object
        [ "id" .= ("native-message-legacy" :: Text)
        , "role" .= ("assistant" :: Text)
        , "content" .= ("native assistant text" :: Text)
        ]

projectedAssistantMessage :: Value
projectedAssistantMessage =
    object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("native assistant text" :: Text)
        ]

projectedPendingAssistantMessage :: Value
projectedPendingAssistantMessage =
    object
        [ "role" .= ("assistant" :: Text)
        , "content"
            .= ( "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n"
                    <> ("native assistant text" :: Text)
               )
        ]

pendingGeminiTextContent :: Value
pendingGeminiTextContent =
    object
        [ "type" .= ("text" :: Text)
        , "text"
            .= ( "[INCOMPLETE ASSISTANT MESSAGE FROM PREVIOUS PROVIDER]\n"
                    <> ("native assistant text" :: Text)
               )
        ]

nativeGeminiTextPayload :: Value
nativeGeminiTextPayload =
    object
        [ "type" .= ("text" :: Text)
        , "text" .= ("native assistant text" :: Text)
        ]

legacyGeminiTextPayload :: Value
legacyGeminiTextPayload =
    object
        [ "text" .= ("native assistant text" :: Text)
        ]

openAiNativeItem :: Value -> HistoryItem
openAiNativeItem =
    nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai")

pendingOpenAiNativeItem :: Value -> HistoryItem
pendingOpenAiNativeItem =
    nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai")

xaiNativeItem :: Value -> HistoryItem
xaiNativeItem =
    nativeHistoryItem ProviderXAIResponses ItemCompleted "response-xai" (Just "item-xai")

geminiNativeItem :: Value -> HistoryItem
geminiNativeItem =
    nativeHistoryItem ProviderGeminiInteractions ItemCompleted "interaction-gemini" (Just "item-gemini")

pendingGeminiNativeItem :: Value -> HistoryItem
pendingGeminiNativeItem =
    nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini")

nativeHistoryItem :: ProviderApiFamily -> ItemLifecycle -> Text -> Maybe Text -> Value -> HistoryItem
nativeHistoryItem apiFamily itemLifecycle exchangeId nativeItemId payload =
    HProvider
        ProviderHistoryItem
            { apiFamily
            , itemLifecycle
            , nativeItem =
                NativeProviderItem
                    { exchangeId = Just exchangeId
                    , nativeItemId
                    , payload
                    }
            }

responsesResponse :: Text -> Text -> [Value] -> Value
responsesResponse responseId status output =
    object
        [ "id" .= responseId
        , "status" .= status
        , "output" .= output
        ]

responsesAssistantPayload :: Text -> Text -> Value
responsesAssistantPayload itemId textValue =
    responsesAssistantPayloadWithStatus itemId textValue "completed"

responsesAssistantPayloadWithStatus :: Text -> Text -> Text -> Value
responsesAssistantPayloadWithStatus itemId textValue status =
    object
        [ "id" .= itemId
        , "type" .= ("message" :: Text)
        , "role" .= ("assistant" :: Text)
        , "status" .= status
        , "content"
            .= ( [ object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= textValue
                        ]
                   ]
                    :: [Value]
               )
        ]

responsesToolCallPayload :: Text -> Text -> Text -> Value -> Text -> Value
responsesToolCallPayload itemId callId name arguments status =
    object
        [ "id" .= itemId
        , "type" .= ("function_call" :: Text)
        , "call_id" .= callId
        , "name" .= name
        , "arguments" .= arguments
        , "status" .= status
        ]

geminiResponse :: Text -> Text -> [Value] -> Value
geminiResponse interactionId status outputs =
    object
        [ "id" .= interactionId
        , "status" .= status
        , "outputs" .= outputs
        ]

geminiTextPayloadWithId :: Text -> Text -> Value
geminiTextPayloadWithId itemId textValue =
    object
        [ "id" .= itemId
        , "type" .= ("text" :: Text)
        , "text" .= textValue
        ]

geminiFunctionCallPayload :: Text -> Text -> Value -> Value
geminiFunctionCallPayload itemId name arguments =
    object
        [ "id" .= itemId
        , "type" .= ("function_call" :: Text)
        , "name" .= name
        , "arguments" .= arguments
        ]

geminiThoughtPayload :: Text -> Value
geminiThoughtPayload itemId =
    object
        [ "signature" .= itemId
        , "type" .= ("thought" :: Text)
        ]

firstToolParameters :: Value -> Maybe Value
firstToolParameters requestBody = do
    Array tools <- lookupPath ["tools"] requestBody
    toolValue <- viaNonEmpty head (toList tools)
    lookupPath ["parameters"] toolValue

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] value = Just value
lookupPath (fieldName : rest) value = case value of
    Object objectValue ->
        KM.lookup (Key.fromText fieldName) objectValue >>= lookupPath rest
    _ ->
        Nothing

sharedHistory :: [HistoryItem]
sharedHistory =
    [ systemParts [textPart "sys", textPart "tem"]
    , developerText "dev"
    , userText "hello"
    , assistantParts [textPart "partial ", textPart "answer"]
    , toolCall "tool-call-1" "lookup" (fromList [("name", String "John Snow")])
    , toolResultJson "tool-call-1" (String "ok")
    ]

sharedHistoryRequest :: [Value]
sharedHistoryRequest =
    [ object
        [ "role" .= ("system" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("input_text" :: Text)
                        , "text" .= ("sys" :: Text)
                        ]
                   , object
                        [ "type" .= ("input_text" :: Text)
                        , "text" .= ("tem" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]
    , object
        [ "role" .= ("developer" :: Text)
        , "content" .= ("dev" :: Text)
        ]
    , object
        [ "role" .= ("user" :: Text)
        , "content" .= ("hello" :: Text)
        ]
    , object
        [ "role" .= ("assistant" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= ("partial " :: Text)
                        ]
                   , object
                        [ "type" .= ("output_text" :: Text)
                        , "text" .= ("answer" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]
    , object
        [ "type" .= ("function_call" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "name" .= ("lookup" :: Text)
        , "arguments" .= ("{\"name\":\"John Snow\"}" :: Text)
        ]
    , object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "output" .= ("\"ok\"" :: Text)
        ]
    ]

sharedGeminiSystemInstruction :: Text
sharedGeminiSystemInstruction =
    "System:\nsystem\n\nDeveloper:\ndev"

sharedGeminiHistoryRequest :: [Value]
sharedGeminiHistoryRequest =
    [ object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("text" :: Text)
                        , "text" .= ("hello" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]
    , object
        [ "role" .= ("model" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("text" :: Text)
                        , "text" .= ("partial answer" :: Text)
                        ]
                   , object
                        [ "type" .= ("function_call" :: Text)
                        , "id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= object ["name" .= ("John Snow" :: Text)]
                        ]
                   ]
                    :: [Value]
               )
        ]
    , object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( [ object
                        [ "type" .= ("function_result" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "result" .= ("ok" :: Text)
                        ]
                   ]
                    :: [Value]
               )
        ]
    ]

nativeToolResultPayload :: Text -> Value
nativeToolResultPayload outputText =
    object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "output" .= outputText
        ]

nativeGeminiToolResultPayload :: Text -> Value
nativeGeminiToolResultPayload resultText =
    object
        [ "type" .= ("function_result" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "result" .= resultText
        ]

scalarAndCompositeJsonCases :: [Text]
scalarAndCompositeJsonCases =
    [ "123"
    , "true"
    , "null"
    , "\"ok\""
    , "[1,2]"
    , "{\"answer\":4}"
    ]
