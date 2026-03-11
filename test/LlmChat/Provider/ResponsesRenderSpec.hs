module LlmChat.Provider.ResponsesRenderSpec where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.IORef qualified as IORef
import Effectful
import Effectful.Error.Static
import LlmChat
import LlmChat.Providers.OpenAI.Responses
import LlmChat.Providers.XAI.Responses
import Relude
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

captureOpenAIRequestBody
    :: ChatConfig '[LlmChat, Error LlmChatError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBody chatConfig history = do
    requestRef <- IORef.newIORef Nothing
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
                , requestLogger = recordRequest requestRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            $ runLlmChatOpenAIResponses settings
            $ void
            $ chat chatConfig history

    result `shouldSatisfy` isLeft
    readRequest requestRef

captureXAIRequestBody
    :: ChatConfig '[LlmChat, Error LlmChatError, IOE]
    -> [HistoryItem]
    -> IO Value
captureXAIRequestBody chatConfig history = do
    requestRef <- IORef.newIORef Nothing
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
                , requestLogger = recordRequest requestRef
                }

    result <-
        runEff
            . runErrorNoCallStack
            $ runLlmChatXAIResponses settings
            $ void
            $ chat chatConfig history

    result `shouldSatisfy` isLeft
    readRequest requestRef

recordRequest :: IOE :> es => IORef.IORef (Maybe Value) -> NativeMsgFormat -> Eff es ()
recordRequest requestRef = \case
    NativeMsgOut requestBody ->
        liftIO $ IORef.writeIORef requestRef (Just requestBody)
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
