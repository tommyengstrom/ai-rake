module Rake.Provider.ResponsesRenderSpec where

import Control.Exception (finally)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Error.Static
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Rake.Chat
import Rake.MediaStorage.InMemory
import Rake.Providers.OpenAI.Chat
import Rake.Providers.OpenAI.Chat qualified as OpenAI
import Relude
import System.Directory (removeFile)
import System.IO qualified as IO
import Test.Hspec

spec :: Spec
spec = describe "OpenAI Responses" $ do
    describe "schema preservation" $ do
        it "preserves raw JsonSchema values verbatim" $ do
            let rawSchema =
                    object
                        [ "type" .= ("object" :: Text)
                        , "additionalProperties" .= True
                        , "properties" .= object
                            [ "value" .= object ["type" .= ("integer" :: Text), "minimum" .= (minBound :: Int), "maximum" .= (maxBound :: Int)] ]
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

    describe "reasoning effort" $ do
        forM_ openAIReasoningEffortCases $ \(reasoningEffort, encodedEffort) ->
            it (toString ("encodes OpenAI " <> show reasoningEffort <> " as " <> encodedEffort)) $ do
                requestBody <-
                    captureOpenAIRequestBodyWithReasoningEffort
                        reasoningEffort
                        defaultChatConfig
                        [user "hello"]

                lookupPath ["reasoning", "effort"] requestBody
                    `shouldBe` Just (String encodedEffort)

    describe "native history rendering" $ do
        it "projects OpenAI-native items into canonical OpenAI input" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesAssistantPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([projectedAssistantMessage] :: [Value]))

        it "replays completed OpenAI non-portable items verbatim for later OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [openAiNativeItem nativeResponsesReasoningPayload]

            lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([nativeResponsesReasoningPayload] :: [Value]))

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
                    ( LlmExpectationError
                        "No stored media reference is available for blob blob-image-1 when rendering openai.responses"
                    )

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
                    Left err ->
                        expectationFailure ("Expected OpenAI response to decode: " <> show err)
                            >> fail "unreachable"
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
                    Left err ->
                        expectationFailure ("Expected OpenAI response to decode: " <> show err)
                            >> fail "unreachable"
                    Right roundValue -> pure roundValue

            let ProviderRound{roundItems} = decodedRound
            result <-
                runOpenAIRenderResult
                    defaultChatConfig
                    (roundItems <> [user "what next?"])

            result
                `shouldBe` Left
                    ( LlmExpectationError
                        "No stored media reference is available for blob openai.responses-response-openai-item-openai-image-0 when rendering openai.responses"
                    )

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
                    Left err ->
                        expectationFailure ("Expected first OpenAI response to decode: " <> show err)
                            >> fail "unreachable"
                    Right roundValue -> pure roundValue
            secondRound <-
                case decodeOpenAIResponse (responsesResponse "response-openai-2" "completed" [secondPayload]) of
                    Left err ->
                        expectationFailure ("Expected second OpenAI response to decode: " <> show err)
                            >> fail "unreachable"
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

    describe "multipart shared rendering" $ do
        it "renders shared history without conversion notes for OpenAI requests" $ do
            (requestBody, notes) <-
                captureOpenAIRender
                    defaultChatConfig
                    sharedHistory

            notes `shouldBe` []
            lookupPath ["input"] requestBody `shouldBe` Just (toJSON sharedHistoryRequest)

        it "renders only the latest system snapshot for OpenAI requests" $ do
            requestBody <-
                captureOpenAIRequestBody
                    defaultChatConfig
                    [ system "old system snapshot"
                    , user "hello"
                    , assistantText "working"
                    , system "new system snapshot"
                    ]

            lookupPath ["input"] requestBody
                `shouldBe` Just
                    ( toJSON
                        ( [ object
                                [ "role" .= ("system" :: Text)
                                , "content" .= ("new system snapshot" :: Text)
                                ]
                          , object
                                [ "role" .= ("user" :: Text)
                                , "content" .= ("hello" :: Text)
                                ]
                          , object
                                [ "role" .= ("assistant" :: Text)
                                , "content" .= ("working" :: Text)
                                ]
                          ]
                            :: [Value]
                        )
                    )
            countSystemRoleMessages requestBody `shouldBe` Just 1

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

    describe "response decoding" $ do
        it "decodes completed OpenAI assistant responses as completed rounds" $ do
            let payload = responsesAssistantPayload "item-openai" "native assistant text"

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [payload])
                `shouldBe` Right
                    ( providerRound
                        [ nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemCompleted
                            "response-openai"
                            (Just "item-openai")
                            payload
                        ]
                        []
                        ProviderRoundDone
                    )

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
                    ( providerRound
                        [ nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemCompleted
                            "response-openai"
                            (Just "item-openai-refusal")
                            payload
                        ]
                        []
                        ProviderRoundDone
                    )

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
                                    , availableLocalTools = []
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
                                , parts =
                                    [ audioPart
                                        "openai.responses-response-openai-item-openai-audio-0"
                                        Nothing
                                        (Just "spoken note")
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-audio"
                                    , payload
                                    , availableLocalTools = []
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
                                , parts =
                                    [imagePart "openai.responses-response-openai-item-openai-untyped-image-0" Nothing Nothing]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-untyped-image"
                                    , payload
                                    , availableLocalTools = []
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
                                    , audioPart
                                        "openai.responses-response-openai-item-openai-mixed-audio-1"
                                        Nothing
                                        (Just "spoken note")
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
                                    , availableLocalTools = []
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
                                    , filePart
                                        "openai.responses-response-openai-item-openai-mixed-3"
                                        Nothing
                                        (Just "appendix.txt")
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai-mixed"
                                    , payload
                                    , availableLocalTools = []
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

        it
            "marks OpenAI tool handoff rounds as pending even when the response is still in progress" $ do
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
                    ( providerRound
                        [ nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemPending
                            "response-openai"
                            (Just "item-openai-tool")
                            payload
                        ]
                        []
                        (ProviderRoundNeedsLocalTools [expectedToolCall])
                    )

        it "keeps incomplete OpenAI responses in pending history instead of completing them" $ do
            let payload = responsesAssistantPayload "item-openai" "working on it"

            decodeOpenAIResponse (responsesResponse "response-openai" "incomplete" [payload])
                `shouldBe` Right
                    ( providerRound
                        [ nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemPending
                            "response-openai"
                            (Just "item-openai")
                            payload
                        ]
                        []
                        (ProviderRoundPaused (PauseIncomplete "Responses response status was incomplete"))
                    )

        it "preserves incomplete reasons without copying response content" $
            forM_
                ( [ (Nothing, ""), (Just Null, ""), (Just (String "malformed"), "")
                  , (Just (object ["reason" .= Null]), "")
                  , (Just (object ["reason" .= (1 :: Int)]), "")
                  ]
                    <> [(Just (object ["reason" .= reason]), ": " <> reason) | reason <- ["max_output_tokens", "content_filter", "future_reason"]]
                    :: [(Maybe Value, Text)]
                ) $ \(details, suffix) ->
                forM_ ([False, True] :: [Bool]) $ \itemLevel -> do
                    let incomplete :: Value
                        incomplete = object $
                            [ "status" .= ("incomplete" :: Text)
                            , "type" .= ("message" :: Text), "role" .= ("assistant" :: Text)
                            , "content" .= ([object ["type" .= ("output_text" :: Text), "text" .= ("private text" :: Text)]] :: [Value])
                            , "message" .= ("private text" :: Text), "failure_reason" .= ("private text" :: Text)
                            , "error" .= object ["message" .= ("private text" :: Text)]
                            ] <> maybe [] (\value -> ["incomplete_details" .= value]) details
                        response :: Value
                        response = if itemLevel
                            then object ["status" .= ("completed" :: Text), "output" .= ([incomplete] :: [Value])]
                            else incomplete
                    case decodeOpenAIResponse response of
                        Right ProviderRound{action, roundItems} -> do
                            action `shouldBe` ProviderRoundPaused
                                (PauseIncomplete ("Responses " <> (if itemLevel then "output item" else "response") <> " status was incomplete" <> suffix))
                            map itemLifecycle roundItems
                                `shouldBe` if itemLevel then [ItemPending] else []
                        Left err -> expectationFailure (show err)

        it "propagates top-level Responses failure details" $ do
            let response =
                    object
                        [ "id" .= ("response-openai" :: Text)
                        , "status" .= ("failed" :: Text)
                        , "error"
                            .= object
                                [ "code" .= ("too_many_tools" :: Text)
                                , "message" .= ("The request contains too many tools." :: Text)
                                ]
                        , "output" .= ([] :: [Value])
                        ]

            decodeOpenAIResponse response
                `shouldBe` Right
                    ( providerRound
                        []
                        []
                        ( ProviderRoundFailed
                            ( FailureProvider
                                "Responses response status was failed: too_many_tools: The request contains too many tools."
                            )
                        )
                    )

        it
            "fails mixed Responses rounds when any item has terminal failure status, even if a tool call is present" $ do
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

            decodeOpenAIResponse
                (responsesResponse "response-openai" "completed" [toolPayload, failedPayload])
                `shouldBe` Right
                    ( providerRound
                        [ nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemPending
                            "response-openai"
                            (Just "item-openai-tool")
                            toolPayload
                        , nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemPending
                            "response-openai"
                            (Just "item-openai-message")
                            failedPayload
                        ]
                        []
                        (ProviderRoundFailed (FailureProvider "Responses output item status was failed"))
                    )

        it "propagates Responses output item failure details" $ do
            let failedPayload =
                    object
                        [ "id" .= ("item-openai-message" :: Text)
                        , "type" .= ("message" :: Text)
                        , "status" .= ("failed" :: Text)
                        , "error"
                            .= object
                                [ "code" .= ("tool_schema_invalid" :: Text)
                                , "message" .= ("Tool schema is invalid." :: Text)
                                ]
                        ]

            decodeOpenAIResponse (responsesResponse "response-openai" "completed" [failedPayload])
                `shouldBe` Right
                    ( providerRound
                        [ nativeHistoryItem
                            ProviderOpenAIResponses
                            ItemPending
                            "response-openai"
                            (Just "item-openai-message")
                            failedPayload
                        ]
                        []
                        ( ProviderRoundFailed
                            ( FailureProvider
                                "Responses output item status was failed: tool_schema_invalid: Tool schema is invalid."
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
                        logger (NativeRequestFailure (ProviderError Nothing "Provider connection failed: boom" Nothing))

            output `shouldSatisfy` T.isInfixOf "[ai-rake:openai.chat] Dropped unsupported item"
            output `shouldSatisfy` T.isInfixOf "[ai-rake:openai.chat] Provider connection failed:"


withTools :: [ToolDef es] -> ChatConfig es -> ChatConfig es
withTools tools ChatConfig{responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout}

withResponseFormat :: ResponseFormat -> ChatConfig es -> ChatConfig es
withResponseFormat responseFormat ChatConfig{tools, sampling, onItem, maxToolRounds, llmCallTimeout} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout}

withSampling :: SamplingOptions -> ChatConfig es -> ChatConfig es
withSampling sampling ChatConfig{tools, responseFormat, onItem, maxToolRounds, llmCallTimeout} =
    ChatConfig{tools, responseFormat, sampling, onItem, maxToolRounds, llmCallTimeout}

withTemperature :: Maybe Double -> SamplingOptions -> SamplingOptions
withTemperature temperature SamplingOptions{topP} =
    SamplingOptions{temperature, topP}

withTopP :: Maybe Double -> SamplingOptions -> SamplingOptions
withTopP topP SamplingOptions{temperature} =
    SamplingOptions{temperature, topP}

openAIReasoningEffortCases :: [(OpenAIReasoningEffort, Text)]
openAIReasoningEffortCases =
    [ (OpenAIReasoningNone, "none")
    , (OpenAIReasoningMinimal, "minimal")
    , (OpenAIReasoningLow, "low")
    , (OpenAIReasoningMedium, "medium")
    , (OpenAIReasoningHigh, "high")
    , (OpenAIReasoningXHigh, "xhigh")
    , (OpenAIReasoningMax, "max")
    ]

captureOpenAIRequestBody
    :: ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBody chatConfig history = do
    requestBody <- captureOpenAIRequestBodyWithMediaReferences [] chatConfig history
    pure requestBody

captureOpenAIRequestBodyWithReasoningEffort
    :: OpenAIReasoningEffort
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBodyWithReasoningEffort reasoningEffort chatConfig history = do
    (requestBody, _) <-
        captureOpenAIRenderWithMediaReferencesAndReasoningEffort
            []
            (Just reasoningEffort)
            chatConfig
            history
    pure requestBody

captureOpenAIRequestBodyWithMediaReferences
    :: [MediaProviderReference]
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO Value
captureOpenAIRequestBodyWithMediaReferences mediaReferences chatConfig history = do
    (requestBody, _) <-
        captureOpenAIRenderWithMediaReferences mediaReferences chatConfig history
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
captureOpenAIRenderWithMediaReferences mediaReferences =
    captureOpenAIRenderWithMediaReferencesAndReasoningEffort mediaReferences Nothing

captureOpenAIRenderWithMediaReferencesAndReasoningEffort
    :: [MediaProviderReference]
    -> Maybe OpenAIReasoningEffort
    -> ChatConfig '[Rake, RakeMediaStorage, Error RakeError, IOE]
    -> [HistoryItem]
    -> IO (Value, [Text])
captureOpenAIRenderWithMediaReferencesAndReasoningEffort mediaReferences configuredReasoningEffort chatConfig history = do
    requestRef <- IORef.newIORef Nothing
    notesRef <- IORef.newIORef []
    let settings :: OpenAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            (defaultOpenAIChatSettings "test-api-key")
                { OpenAI.baseUrl = unreachableBaseUrl
                , OpenAI.reasoningEffort = configuredReasoningEffort
                , OpenAI.requestLogger = recordRequestAndNotes requestRef notesRef
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
    let settings :: OpenAIChatSettings '[RakeMediaStorage, Error RakeError, IOE]
        settings =
            (defaultOpenAIChatSettings "test-api-key")
                { OpenAI.baseUrl = unreachableBaseUrl
                , OpenAI.requestLogger = \_ -> pure ()
                }

    runEff
        . runErrorNoCallStack
        . runRakeMediaStorageInMemory
        $ do
            saveMediaReferences mediaReferences
            runRakeOpenAIChat settings
                $ void
                $ chatOutcome chatConfig history

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

openAiNativeItem :: Value -> HistoryItem
openAiNativeItem =
    nativeHistoryItem
        ProviderOpenAIResponses
        ItemCompleted
        "response-openai"
        (Just "item-openai")

pendingOpenAiNativeItem :: Value -> HistoryItem
pendingOpenAiNativeItem =
    nativeHistoryItem
        ProviderOpenAIResponses
        ItemPending
        "response-openai"
        (Just "item-openai")

nativeHistoryItem
    :: ProviderApiFamily -> ItemLifecycle -> Text -> Maybe Text -> Value -> HistoryItem
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
                    , availableLocalTools = []
                    }
        }
  where
    classifiedItem =
        classifyNativePayload apiFamily payload

classifyNativePayload :: ProviderApiFamily -> Value -> GenericItem
classifyNativePayload apiFamily payload =
    case apiFamily of
        ProviderOpenAIResponses ->
            classifiedRoundItem
                $ decodeOpenAIResponse
                    ( responsesResponse
                        "response-openai"
                        "completed"
                        [payload, responsesAssistantPayload "fixture-completed-assistant" "done"]
                    )
        ProviderXAIResponses -> GenericNonPortable
        ProviderGeminiInteractions -> GenericNonPortable
        ProviderApiFamily{} ->
            GenericNonPortable
  where
    classifiedRoundItem :: Either RakeError ProviderRound -> GenericItem
    classifiedRoundItem = \case
        Right ProviderRound{roundItems = HistoryItem{genericItem = canonicalItem} : _} ->
            canonicalItem
        Right ProviderRound{roundItems = []} ->
            error "Native payload fixture decoded without its requested item"
        Left err ->
            error ("Native payload fixture failed to decode: " <> show err)

providerRound
    :: [HistoryItem] -> [MediaProviderReference] -> ProviderRoundAction -> ProviderRound
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
geminiResponse interactionId status payloads =
    object
        [ "id" .= interactionId
        , "status" .= status
        , "steps" .= map geminiResponseStep payloads
        ]

geminiResponseStep :: Value -> Value
geminiResponseStep payload =
    case payload of
        Object payloadObject
            | KM.lookup "type" payloadObject == Just (String "text") ->
                geminiModelOutputStep [payload]
        _ ->
            payload

geminiModelOutputStep :: [Value] -> Value
geminiModelOutputStep content =
    object
        [ "type" .= ("model_output" :: Text)
        , "content" .= content
        ]

geminiTextPayloadWithId :: Text -> Text -> Value
geminiTextPayloadWithId itemId textValue =
    object
        [ "id" .= itemId
        , "type" .= ("text" :: Text)
        , "text" .= textValue
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

countSystemRoleMessages :: Value -> Maybe Int
countSystemRoleMessages requestBody = do
    Array inputItems <- lookupPath ["input"] requestBody
    pure
        $ length
            [ ()
            | Object itemObject <- toList inputItems
            , KM.lookup "role" itemObject == Just (String "system")
            ]

sharedHistory :: [HistoryItem]
sharedHistory =
    [ systemParts [textPart "sys", textPart "tem"]
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

