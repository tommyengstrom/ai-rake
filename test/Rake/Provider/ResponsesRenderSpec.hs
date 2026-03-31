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
import Rake.MediaStorage.InMemory
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

        it "renders Gemini no-argument tools with an empty parameters object" $ do
            let tool =
                    defineToolNoArgument "noop" "No-op tool" (pure (Right "ok"))

            requestBody <-
                captureGeminiRequestBody
                    (withTools [tool] defaultChatConfig)
                    [user "hello"]

            firstToolParameters requestBody `shouldBe` Just (object [])

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
        it "projects OpenAI-native items into canonical OpenAI input" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects xAI-native items into generic input for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects xAI-native items into canonical xAI input" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [xaiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "projects OpenAI-native items into generic input for xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "replays completed OpenAI non-portable items verbatim for later OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesReasoningPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([nativeResponsesReasoningPayload] :: [Value]))

        it "drops completed OpenAI non-portable items when rendering xAI requests" $ do
            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesReasoningPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending OpenAI-native assistant text from replay for OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingOpenAiNativeItem nativeResponsesAssistantPayload]

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending OpenAI-native assistant text without a type field" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingOpenAiNativeItem legacyResponsesAssistantPayload]

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "renders refusal parts as assistant text for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [assistantParts [refusalPart "I can't help with that"]]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content" .= ("I can't help with that" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "fails fast on OpenAI renders that contain generic image parts" $ do
            result <-
                runOpenAIRenderResult
                    defaultChatConfig
                    [userParts [imagePart "blob-image-1" (Just "image/png") (Just "diagram")]]

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob blob-image-1 when rendering openai.responses")

        it "replays canonical OpenAI image history in-process when media refs were registered" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
            decodedRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload]) of
                    Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems, mediaReferences} = decodedRound
            requestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    mediaReferences
                    defaultChatConfig
                    (roundItems <> [user "what next?"])

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("input_image" :: Text)
                                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("what next?" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "fails clearly on a fresh media store when replaying canonical OpenAI image history" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
            decodedRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload]) of
                    Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems} = decodedRound
            result <-
                runOpenAIRenderResult
                    defaultChatConfig
                    (roundItems <> [user "what next?"])

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob openai.responses-response-openai-item-openai-image-0 when rendering openai.responses")

        it "replays generic audio history in-process when media refs were registered" $ do
            let requestPart =
                    object
                        [ "type" .= ("input_audio" :: Text)
                        , "input_audio"
                            .= object
                                [ "data" .= ("UklGRg==" :: Text)
                                , "format" .= ("wav" :: Text)
                                ]
                        ]
            requestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    [mediaReference "blob-audio-1" ProviderOpenAIResponses requestPart]
                    defaultChatConfig
                    [userParts [audioPart "blob-audio-1" (Just "audio/wav") (Just "spoken note")]]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ([requestPart] :: [Value])
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "keeps media refs distinct when different Responses rounds reuse the same item id" $ do
            let firstPayload =
                    responsesAssistantPayloadWithContent
                        "shared-item"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/first.png" :: Text)
                            ]
                        ]
                secondPayload =
                    responsesAssistantPayloadWithContent
                        "shared-item"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/second.png" :: Text)
                            ]
                        ]
            firstRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai-1" "completed" [firstPayload]) of
                    Left err -> expectationFailure ("Expected first OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue
            secondRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai-2" "completed" [secondPayload]) of
                    Left err -> expectationFailure ("Expected second OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems = firstItems, mediaReferences = firstMediaReferences} = firstRound
                ProviderRound{roundItems = secondItems, mediaReferences = secondMediaReferences} = secondRound
                allMediaReferences = firstMediaReferences <> secondMediaReferences

            firstRequestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    allMediaReferences
                    defaultChatConfig
                    (firstItems <> [user "what next?"])
            secondRequestBody <-
                captureOpenAIRequestBodyWithMediaReferences
                    allMediaReferences
                    defaultChatConfig
                    (secondItems <> [user "what next?"])

            lookupPath ["input"] firstRequestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("input_image" :: Text)
                                                , "image_url" .= ("https://example.com/first.png" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("what next?" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )
            lookupPath ["input"] secondRequestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("assistant" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("input_image" :: Text)
                                                , "image_url" .= ("https://example.com/second.png" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("what next?" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "projects Gemini-native items into canonical Gemini input" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [geminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("native assistant text" :: Text)
                                                ]
                                           ]
                                            :: [Value]
                                       )
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

        it "drops pending Gemini-native assistant text when projecting into OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    [pendingGeminiNativeItem nativeGeminiTextPayload]

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending Gemini-native assistant text even for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiNativeItem nativeGeminiTextPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "replays completed Gemini non-portable artifacts on later Gemini requests" $ do
            (requestBody, notes) <-
                captureGeminiRender
                    defaultChatConfig
                    [geminiNativeItem (geminiThoughtPayload "thought-1")]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content"
                                    .= ( [ geminiThoughtPayload "thought-1"
                                         ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )
            notes `shouldBe` []

        it "drops completed Gemini non-portable artifacts when rendering OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [geminiNativeItem (geminiThoughtPayload "thought-1")]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "drops pending Gemini-native assistant text without a type field even for Gemini requests" $ do
            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiNativeItem legacyGeminiTextPayload]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON ([] :: [Value]))

        it "fails fast on Gemini renders that contain generic audio parts" $ do
            result <-
                runGeminiRenderResult
                    defaultChatConfig
                    [userParts [audioPart "blob-audio-1" (Just "audio/mpeg") (Just "spoken note")]]

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob blob-audio-1 when rendering gemini.interactions")

        it "renders Gemini continuation attachments alongside resolved tool calls" $ do
            let pendingToolCall = pendingGeminiToolCallWithThoughtItem "John Snow"

            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [ pendingToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content"
                                    .= ( [ geminiThoughtPayload "thought-1"
                                         , geminiFunctionCallPayload "tool-call-1" "lookup" (object ["name" .= ("John Snow" :: Text)])
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
                                                , "result"
                                                    .= ( [ object
                                                                [ "type" .= ("text" :: Text)
                                                                , "text" .= ("Contacts:\n- John Snow" :: Text)
                                                                ]
                                                           ]
                                                            :: [Value]
                                                       )
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "injects a dummy thought signature for foreign tool continuations rendered into Gemini" $ do
            let pendingForeignToolCall =
                    nativeHistoryItem
                        ProviderOpenAIResponses
                        ItemPending
                        "response-openai"
                        (Just "item-openai-tool")
                        (responsesToolCallPayload "item-openai-tool" "tool-call-1" "lookup" (object []) "pending")

            requestBody <-
                captureGeminiRequestBody
                    defaultChatConfig
                    [ pendingForeignToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- Ada"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("model" :: Text)
                                , "content"
                                    .= ( [ object
                                                [ "type" .= ("function_call" :: Text)
                                                , "id" .= ("tool-call-1" :: Text)
                                                , "name" .= ("lookup" :: Text)
                                                , "thought_signature" .= ("context_engineering_is_the_way_to_go" :: Text)
                                                , "arguments" .= object []
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
                                                , "result"
                                                    .= ( [ object
                                                                [ "type" .= ("text" :: Text)
                                                                , "text" .= ("Contacts:\n- Ada" :: Text)
                                                                ]
                                                           ]
                                                            :: [Value]
                                                       )
                                                ]
                                           ]
                                            :: [Value]
                                       )
                                ]
                          ]
                            :: [Value]
                        )
                    )

        it "drops Gemini continuation attachments but keeps portable Gemini tool continuations for OpenAI requests" $ do
            let pendingToolCall = pendingGeminiToolCallWithThoughtItem "John Snow"
                expectedInput =
                    [ object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= ("{\"name\":\"John Snow\"}" :: Text)
                        ]
                    , object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "output" .= ("Contacts:\n- John Snow" :: Text)
                        ]
                    ]

            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ pendingToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON (expectedInput :: [Value]))

        it "drops Gemini continuation attachments but keeps portable Gemini tool continuations for xAI requests" $ do
            let pendingToolCall = pendingGeminiToolCallWithThoughtItem "John Snow"
                expectedInput =
                    [ object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= ("{\"name\":\"John Snow\"}" :: Text)
                        ]
                    , object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "output" .= ("Contacts:\n- John Snow" :: Text)
                        ]
                    ]

            requestBody <-
                captureXAIRequestBody
                    defaultChatConfig
                    [ pendingToolCall
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]

            lookupPath ["input"] requestBody `shouldBe` Just (toJSON (expectedInput :: [Value]))

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

        it "renders JSON tool results as JSON text for completed tool exchanges" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ toolCall "tool-call-1" "lookup" (fromList [("name", String "Ada")])
                    , toolResultJson "tool-call-1" (String "ok")
                    , toolCall "tool-call-2" "lookup" (fromList [("name", String "Grace")])
                    , toolResultJson "tool-call-2" (object ["answer" .= (4 :: Int)])
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "type" .= ("function_call" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "arguments" .= ("{\"name\":\"Ada\"}" :: Text)
                                ]
                          , object
                                [ "type" .= ("function_call_output" :: Text)
                                , "call_id" .= ("tool-call-1" :: Text)
                                , "output" .= ("\"ok\"" :: Text)
                                ]
                          , object
                                [ "type" .= ("function_call" :: Text)
                                , "call_id" .= ("tool-call-2" :: Text)
                                , "name" .= ("lookup" :: Text)
                                , "arguments" .= ("{\"name\":\"Grace\"}" :: Text)
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
                    (providerRound [nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai") payload] [] ProviderRoundDone)

        it "decodes completed xAI assistant responses as completed rounds" $ do
            let payload = responsesAssistantPayload "item-xai" "native assistant text"

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderXAIResponses ItemCompleted "response-xai" (Just "item-xai") payload] [] ProviderRoundDone)

        it "decodes OpenAI refusal message parts into canonical refusal parts" $ do
            let payload =
                    object
                        [ "id" .= ("item-openai-refusal" :: Text)
                        , "type" .= ("message" :: Text)
                        , "role" .= ("assistant" :: Text)
                        , "content"
                            .= ( [ object
                                        [ "type" .= ("refusal" :: Text)
                                        , "refusal" .= ("I can't help with that" :: Text)
                                        ]
                                   ]
                                    :: [Value]
                               )
                        ]

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderOpenAIResponses ItemCompleted "response-openai" (Just "item-openai-refusal") payload] [] ProviderRoundDone)

        it "decodes completed OpenAI image-only assistant responses into canonical image parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [imagePart "openai.responses-response-openai-item-openai-image-0" Nothing Nothing]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-image"
                                    , payload
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-image-0"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "decodes completed OpenAI audio-only assistant responses into canonical audio parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-audio"
                        [ object
                            [ "type" .= ("input_audio" :: Text)
                            , "input_audio"
                                .= object
                                    [ "data" .= ("UklGRg==" :: Text)
                                    , "format" .= ("wav" :: Text)
                                    ]
                            , "transcript" .= ("spoken note" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [audioPart "openai.responses-response-openai-item-openai-audio-0" Nothing (Just "spoken note")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-audio"
                                    , payload
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-audio-0"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_audio" :: Text)
                                , "input_audio"
                                    .= object
                                        [ "data" .= ("UklGRg==" :: Text)
                                        , "format" .= ("wav" :: Text)
                                        ]
                                , "transcript" .= ("spoken note" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "decodes completed xAI file-only assistant responses into canonical file parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-xai-file"
                        [ object
                            [ "type" .= ("input_file" :: Text)
                            , "file_id" .= ("file-123" :: Text)
                            , "filename" .= ("report.pdf" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [filePart "xai.responses-response-xai-item-xai-file-0" Nothing (Just "report.pdf")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderXAIResponses
                                    , exchangeId = Just "response-xai"
                                    , nativeItemId = Just "item-xai-file"
                                    , payload
                                    }
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "xai.responses-response-xai-item-xai-file-0"
                            ProviderXAIResponses
                            ( object
                                [ "type" .= ("input_file" :: Text)
                                , "file_id" .= ("file-123" :: Text)
                                , "filename" .= ("report.pdf" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "decodes completed xAI audio-only assistant responses into canonical audio parts" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-xai-audio"
                        [ object
                            [ "type" .= ("output_audio" :: Text)
                            , "audio_url" .= ("https://example.com/audio.mp3" :: Text)
                            , "transcript" .= ("spoken note" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [audioPart "xai.responses-response-xai-item-xai-audio-0" Nothing (Just "spoken note")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderXAIResponses
                                    , exchangeId = Just "response-xai"
                                    , nativeItemId = Just "item-xai-audio"
                                    , payload
                                    }
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "xai.responses-response-xai-item-xai-audio-0"
                            ProviderXAIResponses
                            ( object
                                [ "type" .= ("output_audio" :: Text)
                                , "audio_url" .= ("https://example.com/audio.mp3" :: Text)
                                , "transcript" .= ("spoken note" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "canonicalizes untyped OpenAI image parts before storing replay refs" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-untyped-image"
                        [ object
                            [ "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [imagePart "openai.responses-response-openai-item-openai-untyped-image-0" Nothing Nothing]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-untyped-image"
                                    , payload
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-untyped-image-0"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "canonicalizes untyped xAI file parts before storing replay refs" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-xai-untyped-file"
                        [ object
                            [ "file_id" .= ("file-123" :: Text)
                            , "filename" .= ("report.pdf" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts = [filePart "xai.responses-response-xai-item-xai-untyped-file-0" Nothing (Just "report.pdf")]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderXAIResponses
                                    , exchangeId = Just "response-xai"
                                    , nativeItemId = Just "item-xai-untyped-file"
                                    , payload
                                    }
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "xai.responses-response-xai-item-xai-untyped-file-0"
                            ProviderXAIResponses
                            ( object
                                [ "type" .= ("input_file" :: Text)
                                , "file_id" .= ("file-123" :: Text)
                                , "filename" .= ("report.pdf" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "preserves mixed OpenAI assistant text and audio parts in canonical order" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-mixed-audio"
                        [ object
                            [ "type" .= ("output_text" :: Text)
                            , "text" .= ("before" :: Text)
                            ]
                        , object
                            [ "type" .= ("input_audio" :: Text)
                            , "input_audio"
                                .= object
                                    [ "data" .= ("UklGRg==" :: Text)
                                    , "format" .= ("wav" :: Text)
                                    ]
                            , "transcript" .= ("spoken note" :: Text)
                            ]
                        , object
                            [ "type" .= ("output_text" :: Text)
                            , "text" .= ("after" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts =
                                    [ textPart "before"
                                    , audioPart "openai.responses-response-openai-item-openai-mixed-audio-1" Nothing (Just "spoken note")
                                    , textPart "after"
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-mixed-audio"
                                    , payload
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-mixed-audio-1"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_audio" :: Text)
                                , "input_audio"
                                    .= object
                                        [ "data" .= ("UklGRg==" :: Text)
                                        , "format" .= ("wav" :: Text)
                                        ]
                                , "transcript" .= ("spoken note" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

        it "preserves mixed OpenAI assistant message parts in canonical order" $ do
            let payload =
                    responsesAssistantPayloadWithContent
                        "item-openai-mixed"
                        [ object
                            [ "type" .= ("output_text" :: Text)
                            , "text" .= ("before" :: Text)
                            ]
                        , object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/diagram.png" :: Text)
                            ]
                        , object
                            [ "type" .= ("refusal" :: Text)
                            , "refusal" .= ("I can't help with that part" :: Text)
                            ]
                        , object
                            [ "type" .= ("input_file" :: Text)
                            , "file_id" .= ("file-456" :: Text)
                            , "filename" .= ("appendix.txt" :: Text)
                            ]
                        ]
                expectedItem =
                    HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts =
                                    [ textPart "before"
                                    , imagePart "openai.responses-response-openai-item-openai-mixed-1" Nothing Nothing
                                    , refusalPart "I can't help with that part"
                                    , filePart "openai.responses-response-openai-item-openai-mixed-3" Nothing (Just "appendix.txt")
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-mixed"
                                    , payload
                                    }
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [expectedItem]
                        [ mediaReference
                            "openai.responses-response-openai-item-openai-mixed-1"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/diagram.png" :: Text)
                                ]
                            )
                        , mediaReference
                            "openai.responses-response-openai-item-openai-mixed-3"
                            ProviderOpenAIResponses
                            ( object
                                [ "type" .= ("input_file" :: Text)
                                , "file_id" .= ("file-456" :: Text)
                                , "filename" .= ("appendix.txt" :: Text)
                                ]
                            )
                        ]
                        ProviderRoundDone
                    )

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
                        , continuationAttachments = []
                        }

            decodeOpenAIResponse (responsesResponse "response-openai" "in_progress" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-tool") payload] [] (ProviderRoundNeedsLocalTools [expectedToolCall]))

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
                        , continuationAttachments = []
                        }

            decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderXAIResponses ItemPending "response-xai" (Just "item-xai-tool") payload] [] (ProviderRoundNeedsLocalTools [expectedToolCall]))

        it "keeps incomplete OpenAI responses in pending history instead of completing them" $ do
            let payload = responsesAssistantPayload "item-openai" "working on it"

            decodeOpenAIResponse (responsesResponse "response-openai" "incomplete" [payload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai") payload]
                        []
                        (ProviderRoundPaused (PauseIncomplete "Responses response status was incomplete"))
                    )

        it "fails terminal xAI responses" $ do
            decodeXAIResponse (responsesResponse "response-xai" "failed" [])
                `shouldBe` Right
                    (providerRound [] [] (ProviderRoundFailed (FailureProvider "Responses response status was failed")))

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
                    ( providerRound
                        [ nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-tool") toolPayload
                        , nativeHistoryItem ProviderOpenAIResponses ItemPending "response-openai" (Just "item-openai-message") failedPayload
                        ]
                        []
                        (ProviderRoundFailed (FailureProvider "Responses output item status was failed"))
                    )

        it "decodes completed Gemini assistant responses as completed rounds" $ do
            let payload = geminiTextPayloadWithId "item-gemini" "native assistant text"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderGeminiInteractions ItemCompleted "interaction-gemini" (Just "item-gemini") payload] [] ProviderRoundDone)

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
                        , continuationAttachments = []
                        }

            decodeGeminiResponse (geminiResponse "interaction-gemini" "requires_action" [payload])
                `shouldBe` Right
                    (providerRound [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini-tool") payload] [] (ProviderRoundNeedsLocalTools [expectedToolCall]))

        it "falls back to the first output identifier as the Gemini exchange id when store=false omits it" $ do
            let thoughtPayload = geminiThoughtPayload "thought-1"
                toolPayload =
                    geminiFunctionCallPayload
                        "item-gemini-tool"
                        "lookup"
                        (object ["name" .= ("Ada" :: Text)])
                rawResponse =
                    object
                        [ "status" .= ("requires_action" :: Text)
                        , "outputs" .= ([thoughtPayload, toolPayload] :: [Value])
                        ]
                expectedToolCall =
                    ToolCall
                        { toolCallId = "item-gemini-tool"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        , continuationAttachments =
                            [ ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload = thoughtPayload
                                }
                            ]
                        }

            decodeGeminiResponse rawResponse
                `shouldBe` Right
                    ( providerRound
                        [pendingGeminiToolCallWithThoughtItemAndExchangeId "Ada" "thought-1" "item-gemini-tool"]
                        []
                        (ProviderRoundNeedsLocalTools [expectedToolCall])
                    )

        it "pauses Gemini in-progress rounds" $ do
            let payload = geminiTextPayloadWithId "item-gemini" "working on it"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "in_progress" [payload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "item-gemini") payload]
                        []
                        (ProviderRoundPaused (PauseProviderWaiting "Gemini interaction status was in_progress"))
                    )

        it "fails terminal Gemini responses" $ do
            decodeGeminiResponse (geminiResponse "interaction-gemini" "failed" [])
                `shouldBe` Right
                    (providerRound [] [] (ProviderRoundFailed (FailureProvider "Gemini interaction status was failed")))

        it "fails completed Gemini thought-only rounds as contract errors" $ do
            let payload = geminiThoughtPayload "thought-1"

            decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [nativeHistoryItem ProviderGeminiInteractions ItemPending "interaction-gemini" (Just "thought-1") payload]
                        []
                        ( ProviderRoundFailed
                            ( FailureContract
                                "Gemini interaction completed without tool calls or assistant message"
                            )
                        )
                    )

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
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBody chatConfig history = do
    requestBody <- captureOpenAIRequestBodyWithMediaReferences [] chatConfig history
    pure requestBody

captureOpenAIRequestBodyWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBodyWithMediaReferences mediaReferences chatConfig history = do
    (requestBody, _) <- captureOpenAIRenderWithMediaReferences mediaReferences chatConfig history
    pure requestBody

captureOpenAIRender
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRender =
    captureOpenAIRenderWithMediaReferences []

captureOpenAIRenderWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRenderWithMediaReferences mediaReferences chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let OpenAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , organizationId = defaultOrganizationId
            , projectId = defaultProjectId
            } = defaultOpenAIChatSettings "test-api-key"
        settings :: OpenAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
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
            . runRakeMediaStorageInMemory
            $ do
                saveMediaReferences mediaReferences
                runRakeOpenAIChat settings
                    $ void
                    $ chatOutcome chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

runOpenAIRenderResult
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Either RakeError ())
runOpenAIRenderResult =
    runOpenAIRenderResultWithMediaReferences []

runOpenAIRenderResultWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Either RakeError ())
runOpenAIRenderResultWithMediaReferences mediaReferences chatConfig history = do
    let OpenAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , organizationId = defaultOrganizationId
            , projectId = defaultProjectId
            } = defaultOpenAIChatSettings "test-api-key"
        settings :: OpenAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            OpenAIChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , organizationId = defaultOrganizationId
                , projectId = defaultProjectId
                , requestLogger = \_ -> pure ()
                }

    runEff
        . runErrorNoCallStack
        . runRakeMediaStorageInMemory
        $ do
            saveMediaReferences mediaReferences
            runRakeOpenAIChat settings
                $ void
                $ chatOutcome chatConfig history

captureXAIRequestBody
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureXAIRequestBody chatConfig history = do
    (requestBody, _) <- captureXAIRender chatConfig history
    pure requestBody

captureXAIRender
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureXAIRender chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let XAIChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            } = defaultXAIChatSettings "test-api-key"
        settings :: XAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
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
            . runRakeMediaStorageInMemory
            $ runRakeXAIChat settings
            $ void
            $ chatOutcome chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

captureGeminiRequestBody
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureGeminiRequestBody chatConfig history = do
    (requestBody, _) <- captureGeminiRender chatConfig history
    pure requestBody

captureGeminiRender
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
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
        settings :: GeminiChatSettings '[RakeMediaStorage, Error RakeError, IOE]
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
            . runRakeMediaStorageInMemory
            $ runRakeGeminiChat settings
            $ void
            $ chatOutcome chatConfig history

    result `shouldSatisfy` isLeft
    requestBody <- readRequest requestRef
    notes <- IORef.readIORef notesRef
    pure (requestBody, notes)

runGeminiRenderResult
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Either RakeError ())
runGeminiRenderResult chatConfig history = do
    let GeminiChatSettings
            { apiKey = defaultApiKey
            , model = defaultModel
            , providerTools = defaultProviderTools
            , generationConfig = defaultGenerationConfig
            } = defaultGeminiChatSettings "test-api-key"
        settings :: GeminiChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            GeminiChatSettings
                { apiKey = defaultApiKey
                , model = defaultModel
                , baseUrl = unreachableBaseUrl
                , systemInstruction = Nothing
                , providerTools = defaultProviderTools
                , generationConfig = defaultGenerationConfig
                , requestLogger = \_ -> pure ()
                }

    runEff
        . runErrorNoCallStack
        . runRakeMediaStorageInMemory
        $ runRakeGeminiChat settings
        $ void
        $ chatOutcome chatConfig history

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

nativeResponsesReasoningPayload :: Value
nativeResponsesReasoningPayload =
    object
        [ "id" .= ("native-reasoning-1" :: Text)
        , "type" .= ("reasoning" :: Text)
        , "encrypted_content" .= ("opaque-reasoning-state" :: Text)
        ]

projectedAssistantMessage :: Value
projectedAssistantMessage =
    object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("native assistant text" :: Text)
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

pendingGeminiToolCallWithThoughtItem :: Text -> HistoryItem
pendingGeminiToolCallWithThoughtItem contactName =
    pendingGeminiToolCallWithThoughtItemAndExchangeId contactName "interaction-gemini" "tool-call-1"

pendingGeminiToolCallWithThoughtItemAndExchangeId :: Text -> Text -> Text -> HistoryItem
pendingGeminiToolCallWithThoughtItemAndExchangeId contactName exchangeId toolCallId =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemPending
        , genericItem =
            GenericToolCall
                { toolCall =
                    ToolCall
                        { toolCallId = ToolCallId toolCallId
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String contactName)]
                        , continuationAttachments =
                            [ ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload = geminiThoughtPayload "thought-1"
                                }
                            ]
                        }
                }
        , providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , exchangeId = Just exchangeId
                    , nativeItemId = Just toolCallId
                    , payload = geminiFunctionCallPayload toolCallId "lookup" (object ["name" .= contactName])
                    }
        }

nativeHistoryItem :: ProviderApiFamily -> ItemLifecycle -> Text -> Maybe Text -> Value -> HistoryItem
nativeHistoryItem apiFamily lifecycle exchangeId nativeItemId payload =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = lifecycle
        , genericItem = classifiedItem
        , providerItem =
            Just
                ProviderItem
                    { apiFamily
                    , exchangeId = Just exchangeId
                    , nativeItemId
                    , payload
                    }
        }
  where
    classifiedItem =
        classifyNativePayload apiFamily payload

classifyNativePayload :: ProviderApiFamily -> Value -> GenericItem
classifyNativePayload apiFamily payload =
    case apiFamily of
        ProviderOpenAIResponses ->
            classifiedRoundItem $
                decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
        ProviderXAIResponses ->
            classifiedRoundItem $
                decodeXAIResponse (responsesResponse "response-xai" "completed" [payload])
        ProviderGeminiInteractions ->
            classifiedRoundItem $
                decodeGeminiResponse (geminiResponse "interaction-gemini" "completed" [payload])
        ProviderApiFamily{} ->
            GenericNonPortable
  where
    classifiedRoundItem = \case
        Right ProviderRound{roundItems = HistoryItem{genericItem = canonicalItem} : _} ->
            canonicalItem
        _ ->
            GenericNonPortable

providerRound :: [HistoryItem] -> [MediaProviderReference] -> ProviderRoundAction -> ProviderRound
providerRound roundItems mediaReferences action =
    ProviderRound{roundItems, mediaReferences, action}

mediaReference :: MediaBlobId -> ProviderApiFamily -> Value -> MediaProviderReference
mediaReference mediaBlobId providerFamily providerRequestPart =
    MediaProviderReference{mediaBlobId, providerFamily, providerRequestPart}

responsesResponse :: Text -> Text -> [Value] -> Value
responsesResponse responseId status output =
    object
        [ "id" .= responseId
        , "status" .= status
        , "output" .= output
        ]

responsesAssistantPayload :: Text -> Text -> Value
responsesAssistantPayload itemId textValue =
    responsesAssistantPayloadWithContentAndStatus
        itemId
        [ object
            [ "type" .= ("output_text" :: Text)
            , "text" .= textValue
            ]
        ]
        "completed"

responsesAssistantPayloadWithStatus :: Text -> Text -> Text -> Value
responsesAssistantPayloadWithStatus itemId textValue status =
    responsesAssistantPayloadWithContentAndStatus
        itemId
        [ object
            [ "type" .= ("output_text" :: Text)
            , "text" .= textValue
            ]
        ]
        status

responsesAssistantPayloadWithContent :: Text -> [Value] -> Value
responsesAssistantPayloadWithContent itemId content =
    responsesAssistantPayloadWithContentAndStatus itemId content "completed"

responsesAssistantPayloadWithContentAndStatus :: Text -> [Value] -> Text -> Value
responsesAssistantPayloadWithContentAndStatus itemId content status =
    object
        [ "id" .= itemId
        , "type" .= ("message" :: Text)
        , "role" .= ("assistant" :: Text)
        , "status" .= status
        , "content" .= content
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
                        , "text" .= ("partial " :: Text)
                        ]
                   , object
                        [ "type" .= ("text" :: Text)
                        , "text" .= ("answer" :: Text)
                        ]
                   , object
                        [ "type" .= ("function_call" :: Text)
                        , "id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "thought_signature" .= ("context_engineering_is_the_way_to_go" :: Text)
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
                        , "result"
                            .= ( [ object
                                        [ "type" .= ("text" :: Text)
                                        , "text" .= ("\"ok\"" :: Text)
                                        ]
                                   ]
                                    :: [Value]
                               )
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
        , "result"
            .= ( [ object
                        [ "type" .= ("text" :: Text)
                        , "text" .= resultText
                        ]
                   ]
                    :: [Value]
               )
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
