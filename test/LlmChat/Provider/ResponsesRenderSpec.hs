module LlmChat.Provider.ResponsesRenderSpec where

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
import LlmChat
import LlmChat.Providers.OpenAI.Responses
import LlmChat.Providers.XAI.Responses
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
                    defaultChatConfig{responseFormat = JsonSchema rawSchema}
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
                    defaultChatConfig{tools = [tool]}
                    [user "hello"]

            firstToolParameters requestBody `shouldBe` Just rawSchema

    describe "native history rendering" $ do
        it "reuses native OpenAI items unchanged for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([nativeAssistantPayload] :: [Value]))

        it "projects xAI-native items into generic input for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "reuses native xAI items unchanged for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([nativeAssistantPayload] :: [Value]))

        it "projects OpenAI-native items into generic input for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

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

    describe "default logging" $ do
        it "warns on OpenAI conversion notes and request failures" $ do
            output <-
                captureStderrText do
                    let settings :: OpenAIResponsesSettings '[IOE]
                        settings = defaultOpenAIResponsesSettings "test-api-key"
                        OpenAIResponsesSettings{requestLogger = logger} = settings
                    runEff do
                        logger (NativeConversionNote "Dropped unsupported item")
                        logger (NativeRequestFailure (ConnectionError (toException (ErrorCall "boom"))))

            output `shouldSatisfy` T.isInfixOf "[llmchat-effectful:openai.responses] Dropped unsupported item"
            output `shouldSatisfy` T.isInfixOf "[llmchat-effectful:openai.responses] Provider request failed:"

        it "warns on xAI conversion notes" $ do
            output <-
                captureStderrText do
                    let settings :: XAIResponsesSettings '[IOE]
                        settings = defaultXAIResponsesSettings "test-api-key"
                        XAIResponsesSettings{requestLogger = logger} = settings
                    runEff $
                        logger (NativeConversionNote "Dropped unsupported item")

            output `shouldSatisfy` T.isInfixOf "[llmchat-effectful:xai.responses] Dropped unsupported item"

        it "keeps raw request and response bodies silent by default" $ do
            output <-
                captureStderrText do
                    let openAiSettings :: OpenAIResponsesSettings '[IOE]
                        openAiSettings = defaultOpenAIResponsesSettings "test-api-key"
                        xaiSettings :: XAIResponsesSettings '[IOE]
                        xaiSettings = defaultXAIResponsesSettings "test-api-key"
                        OpenAIResponsesSettings{requestLogger = openAiLogger} = openAiSettings
                        XAIResponsesSettings{requestLogger = xaiLogger} = xaiSettings
                    runEff do
                        openAiLogger (NativeMsgOut (object ["hello" .= ("world" :: Text)]))
                        openAiLogger (NativeMsgIn (object ["ok" .= True]))
                        xaiLogger (NativeMsgOut (object ["hello" .= ("world" :: Text)]))
                        xaiLogger (NativeMsgIn (object ["ok" .= True]))

            T.strip output `shouldBe` ""

captureOpenAIRequestBody
    :: ChatConfig '[LlmChat, Error LlmChatError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBody chatConfig history = do
    (requestBody, _) <- captureOpenAIRender chatConfig history
    pure requestBody

captureOpenAIRender
    :: ChatConfig '[LlmChat, Error LlmChatError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let OpenAIResponsesSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , organizationId = defaultOrganizationId
            , projectId = defaultProjectId
            } = defaultOpenAIResponsesSettings "test-api-key"
        settings :: OpenAIResponsesSettings '[Error LlmChatError, IOE]
        settings =
                OpenAIResponsesSettings
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
            $ runLlmChatOpenAIResponses settings
            $ void
            $ chat chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

captureXAIRequestBody
    :: ChatConfig '[LlmChat, Error LlmChatError, IOE]
    -> [HistoryItem]
    -> IO Value
captureXAIRequestBody chatConfig history = do
    (requestBody, _) <- captureXAIRender chatConfig history
    pure requestBody

captureXAIRender
    :: ChatConfig '[LlmChat, Error LlmChatError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureXAIRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let XAIResponsesSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            } = defaultXAIResponsesSettings "test-api-key"
        settings :: XAIResponsesSettings '[Error LlmChatError, IOE]
        settings =
            XAIResponsesSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , requestLogger = recordRequestAndNotes requestRef notesRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            $ runLlmChatXAIResponses settings
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
    (tempPath, tempHandle) <- IO.openTempFile "/tmp" "llmchat-effectful-stderr"

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

nativeAssistantPayload :: Value
nativeAssistantPayload =
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

projectedAssistantMessage :: Value
projectedAssistantMessage =
    object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("native assistant text" :: Text)
        ]

openAiNativeItem :: Value -> HistoryItem
openAiNativeItem payload =
    HOpenAIResponses
        ( OpenAIResponsesItem
            NativeResponseItem
                { responseId = Just "response-openai"
                , nativeItemId = Just "item-openai"
                , payload
                }
        )

xaiNativeItem :: Value -> HistoryItem
xaiNativeItem payload =
    HXAIResponses
        ( XAIResponsesItem
            NativeResponseItem
                { responseId = Just "response-xai"
                , nativeItemId = Just "item-xai"
                , payload
                }
        )

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

nativeToolResultPayload :: Text -> Value
nativeToolResultPayload outputText =
    object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= ("tool-call-1" :: Text)
        , "output" .= outputText
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
