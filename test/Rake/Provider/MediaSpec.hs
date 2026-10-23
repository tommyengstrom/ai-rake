{-# LANGUAGE NoOverloadedLists #-}

module Rake.Provider.MediaSpec where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Effectful
import Effectful.Error.Static
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Rake.Error (RakeError (..))
import Rake.Media
import Rake.Providers.Gemini.Images
import Rake.Providers.Gemini.Videos
import Rake.Providers.OpenAI.Images
import Rake.Providers.OpenAI.Images qualified as OpenAI
import Rake.Providers.OpenAI.TTS
import Rake.Providers.XAI.Imagine
import Rake.Providers.XAI.Imagine qualified as XAI
import Rake.Providers.XAI.TTS
import Relude
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, elements, forAll, listOf, oneof, (===))

spec :: Spec
spec = describe "media providers" $ do
    describe "image response parsing" $ do
        it "round-trips generated image payloads and MIME metadata"
            $ forAll genGeneratedImage
            $ \image ->
                fromJSON (toJSON image) === Success image

        it "round-trips image responses with optional provider metadata"
            $ forAll genImageGenerationResponse
            $ \response ->
                fromJSON (toJSON response) === Success response

        it "preserves OpenAI output metadata and nested token usage" $ do
            let responseValue :: Value
                responseValue =
                    object
                        [ "created" .= (123 :: Int)
                        , "model" .= ("gpt-image-2.5-flare" :: Text)
                        , "data" .= [object ["b64_json" .= ("YWJj" :: Text)]]
                        , "output_format" .= ("webp" :: Text)
                        , "size" .= ("1536x864" :: Text)
                        , "quality" .= ("max" :: Text)
                        , "background" .= ("transparent" :: Text)
                        , "usage"
                            .= object
                                [ "input_tokens" .= (12 :: Int)
                                , "output_tokens" .= (34 :: Int)
                                , "input_tokens_details" .= object ["image_tokens" .= (8 :: Int)]
                                ]
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success responseValue

        it "preserves xAI MIME types, resolved models, and exact cost ticks" $ do
            let responseValue :: Value
                responseValue =
                    object
                        [ "model" .= ("grok-imagine-image-2.0" :: Text)
                        , "data"
                            .= [ object
                                    ["url" .= ("https://example.com/image" :: Text), "mime_type" .= ("image/jpeg" :: Text)]
                               ]
                        , "usage"
                            .= object
                                [ "cost_in_usd_ticks" .= (9007199254740993 :: Integer)
                                , "input_tokens" .= Null
                                , "provider_extension" .= object ["value" .= True]
                                ]
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success responseValue

        it "normalizes Gemini model and usage metadata and snake-case inline images" $ do
            let usageValue :: Value
                usageValue = object ["totalTokenCount" .= (42 :: Int)]
                responseValue :: Value
                responseValue =
                    object
                        [ "modelVersion" .= ("gemini-2.5-flash-image" :: Text)
                        , "usageMetadata" .= usageValue
                        , "candidates"
                            .= [ object
                                    [ "content"
                                        .= object
                                            [ "parts"
                                                .= [ object
                                                        [ "inline_data"
                                                            .= object
                                                                ["mime_type" .= ("image/png" :: Text), "data" .= ("YWJj" :: Text)]
                                                        ]
                                                   ]
                                            ]
                                    ]
                               ]
                        ]
                expectedValue :: Value
                expectedValue =
                    object
                        [ "model" .= ("gemini-2.5-flash-image" :: Text)
                        , "usage" .= usageValue
                        , "data" .= [object ["b64_json" .= ("YWJj" :: Text), "mime_type" .= ("image/png" :: Text)]]
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success expectedValue

        it "accepts null optional response metadata" $ do
            let responseValue :: Value
                responseValue =
                    object
                        [ "created" .= Null
                        , "data" .= ([] :: [Value])
                        , "model" .= Null
                        , "usage" .= Null
                        , "output_format" .= Null
                        , "size" .= Null
                        , "quality" .= Null
                        , "background" .= Null
                        ]
            (toJSON <$> fromJSON @ImageGenerationResponse responseValue)
                `shouldBe` Success (object ["data" .= ([] :: [Value])])

        it "parses GPT Image responses with base64 payloads" $ do
            let responseValue =
                    object
                        [ "created" .= (123 :: Int)
                        , "data"
                            .= ( [ object
                                    [ "b64_json" .= ("YWJj" :: Text)
                                    , "revised_prompt" .= ("A refined prompt" :: Text)
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Just 123
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Just "A refined prompt"
                                , mimeType = Nothing
                                }
                            ]
                        }

        it "parses URL-based image responses" $ do
            let responseValue =
                    object
                        [ "data"
                            .= ( [ object
                                    [ "url" .= ("https://example.com/image.png" :: Text)
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Nothing
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Just "https://example.com/image.png"
                                , b64Json = Nothing
                                , revisedPrompt = Nothing
                                , mimeType = Nothing
                                }
                            ]
                        }

        it "parses Gemini inlineData image responses" $ do
            let responseValue =
                    object
                        [ "candidates"
                            .= ( [ object
                                    [ "content"
                                        .= object
                                            [ "parts"
                                                .= ( [ object
                                                        [ "inlineData"
                                                            .= object
                                                                [ "mimeType" .= ("image/jpeg" :: Text)
                                                                , "data" .= ("YWJj" :: Text)
                                                                ]
                                                        ]
                                                     ]
                                                        :: [Value]
                                                   )
                                            ]
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Nothing
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Nothing
                                , mimeType = Just "image/jpeg"
                                }
                            ]
                        }

        it "ignores non-image Gemini content parts" $ do
            let responseValue =
                    object
                        [ "candidates"
                            .= ( [ object
                                    [ "content"
                                        .= object
                                            [ "parts"
                                                .= ( [ object ["text" .= ("ignore me" :: Text)]
                                                     , object
                                                        [ "inlineData"
                                                            .= object
                                                                [ "mimeType" .= ("image/png" :: Text)
                                                                , "data" .= ("YWJj" :: Text)
                                                                ]
                                                        ]
                                                     ]
                                                        :: [Value]
                                                   )
                                            ]
                                    ]
                                 ]
                                    :: [Value]
                               )
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    ImageGenerationResponse
                        { created = Nothing
                        , model = Nothing
                        , outputFormat = Nothing
                        , size = Nothing
                        , quality = Nothing
                        , background = Nothing
                        , usage = Nothing
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Nothing
                                , mimeType = Just "image/png"
                                }
                            ]
                        }

    describe "xAI video parsing" $ do
        it "parses video start responses" $ do
            fromJSON (object ["request_id" .= ("req-1" :: Text)])
                `shouldBe` Success (XAIVideoStartResponse (XAIVideoRequestId "req-1"))

        it "parses completed video responses" $ do
            let responseValue =
                    object
                        [ "status" .= ("done" :: Text)
                        , "model" .= ("grok-imagine-video" :: Text)
                        , "video"
                            .= object
                                [ "url" .= ("https://example.com/video.mp4" :: Text)
                                , "duration" .= (8 :: Int)
                                , "respect_moderation" .= True
                                ]
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    XAIVideoResponse
                        { status = XAIVideoDone
                        , model = Just "grok-imagine-video"
                        , video =
                            Just
                                GeneratedVideo
                                    { url = Just "https://example.com/video.mp4"
                                    , duration = Just 8
                                    , respectModeration = Just True
                                    }
                        }

        it "parses pending video responses" $ do
            fromJSON (object ["status" .= ("pending" :: Text)])
                `shouldBe` Success
                    XAIVideoResponse
                        { status = XAIVideoPending
                        , model = Nothing
                        , video = Nothing
                        }

        it "parses failed video responses" $ do
            fromJSON (object ["status" .= ("failed" :: Text)])
                `shouldBe` Success
                    XAIVideoResponse
                        { status = XAIVideoFailed
                        , model = Nothing
                        , video = Nothing
                        }

        it "encodes failed video statuses" $ do
            toJSON XAIVideoFailed `shouldBe` String "failed"

    describe "Gemini Veo video parsing" $ do
        it "parses pending video operations" $ do
            fromJSON
                ( object
                    [ "name" .= ("models/veo-3.1-generate-preview/operations/op-1" :: Text)
                    ]
                )
                `shouldBe` Success
                    GeminiVideoOperation
                        { name = GeminiVideoOperationName "models/veo-3.1-generate-preview/operations/op-1"
                        , done = False
                        , response = Nothing
                        , error = Nothing
                        }

        it "parses completed video operations" $ do
            let responseValue =
                    object
                        [ "name" .= ("models/veo-3.1-generate-preview/operations/op-1" :: Text)
                        , "done" .= True
                        , "response"
                            .= object
                                [ "generateVideoResponse"
                                    .= object
                                        [ "generatedSamples"
                                            .= ( [ object
                                                    [ "video"
                                                        .= object
                                                            [ "uri" .= ("https://example.com/video.mp4" :: Text)
                                                            , "mimeType" .= ("video/mp4" :: Text)
                                                            ]
                                                    ]
                                                 ]
                                                    :: [Value]
                                               )
                                        ]
                                ]
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    GeminiVideoOperation
                        { name = GeminiVideoOperationName "models/veo-3.1-generate-preview/operations/op-1"
                        , done = True
                        , response =
                            Just
                                GeminiGenerateVideoResponse
                                    { generatedVideos =
                                        [ GeminiGeneratedVideo
                                            { uri = Just "https://example.com/video.mp4"
                                            , mimeType = Just "video/mp4"
                                            , videoBytes = Nothing
                                            }
                                        ]
                                    }
                        , error = Nothing
                        }

        it "parses failed video operations" $ do
            let responseValue =
                    object
                        [ "name" .= ("models/veo-3.1-generate-preview/operations/op-1" :: Text)
                        , "done" .= True
                        , "error"
                            .= object
                                [ "code" .= (400 :: Int)
                                , "message" .= ("blocked" :: Text)
                                , "status" .= ("FAILED_PRECONDITION" :: Text)
                                ]
                        ]

            fromJSON responseValue
                `shouldBe` Success
                    GeminiVideoOperation
                        { name = GeminiVideoOperationName "models/veo-3.1-generate-preview/operations/op-1"
                        , done = True
                        , response = Nothing
                        , error =
                            Just
                                GeminiVideoError
                                    { code = Just 400
                                    , message = Just "blocked"
                                    , status = Just "FAILED_PRECONDITION"
                                    }
                        }

        it "parses RAI filtered video operation diagnostics" $ do
            let filteredReason =
                    "Sorry, we can't create videos with real people's names or likenesses. Please remove the celebrity reference and try again."
                responseValue =
                    object
                        [ "name" .= ("models/veo-3.1-generate-preview/operations/op-1" :: Text)
                        , "done" .= True
                        , "response"
                            .= object
                                [ "generateVideoResponse"
                                    .= object
                                        [ "raiMediaFilteredCount" .= (1 :: Int)
                                        , "raiMediaFilteredReasons" .= ([filteredReason] :: [Text])
                                        ]
                                ]
                        ]

            case fromJSON responseValue of
                Error err ->
                    expectationFailure err
                Success
                    operation@GeminiVideoOperation{done = True, response = Just videoResponse, error = Nothing} -> do
                    case videoResponse of
                        GeminiGenerateVideoResponse{generatedVideos = []} ->
                            pure ()
                        _ ->
                            expectationFailure "Expected no generated videos"
                    raiMediaFilteredCount videoResponse `shouldBe` Just 1
                    raiMediaFilteredReasons videoResponse `shouldBe` [filteredReason]
                    geminiVideoOperationFailureMessage operation `shouldBe` Just filteredReason
                Success _ ->
                    expectationFailure "Expected completed RAI filtered operation"

        it "parses snake_case RAI filtered video diagnostics" $ do
            let filteredReasons = ["filtered one", "filtered two"] :: [Text]
                responseValue =
                    object
                        [ "generated_videos" .= ([] :: [Value])
                        , "rai_media_filtered_count" .= (2 :: Int)
                        , "rai_media_filtered_reasons" .= filteredReasons
                        ]

            case fromJSON responseValue of
                Error err ->
                    expectationFailure err
                Success videoResponse -> do
                    case videoResponse of
                        GeminiGenerateVideoResponse{generatedVideos = []} ->
                            pure ()
                        _ ->
                            expectationFailure "Expected no generated videos"
                    raiMediaFilteredCount videoResponse `shouldBe` Just 2
                    raiMediaFilteredReasons videoResponse `shouldBe` filteredReasons

        it "preserves RAI filtered diagnostics in JSON" $ do
            let filteredReasons = ["filtered one"] :: [Text]
                responseValue =
                    object
                        [ "generatedVideos" .= ([] :: [Value])
                        , "raiMediaFilteredCount" .= (1 :: Int)
                        , "raiMediaFilteredReasons" .= filteredReasons
                        ]

            case fromJSON responseValue of
                Error err ->
                    expectationFailure err
                Success (videoResponse :: GeminiGenerateVideoResponse) ->
                    toJSON videoResponse `shouldBe` responseValue

        it "summarizes Veo operation errors" $ do
            let operation =
                    GeminiVideoOperation
                        { name = GeminiVideoOperationName "models/veo-3.1-generate-preview/operations/op-1"
                        , done = True
                        , response = Nothing
                        , error =
                            Just
                                GeminiVideoError
                                    { code = Just 400
                                    , message = Just "blocked"
                                    , status = Just "FAILED_PRECONDITION"
                                    }
                        }

            geminiVideoOperationFailureMessage operation
                `shouldBe` Just "code=400 status=FAILED_PRECONDITION blocked"

        it "summarizes completed Veo operations with no videos and no provider reason" $ do
            let operation =
                    GeminiVideoOperation
                        { name = GeminiVideoOperationName "models/veo-3.1-generate-preview/operations/op-1"
                        , done = True
                        , response = Just (GeminiGenerateVideoResponse [])
                        , error = Nothing
                        }

            geminiVideoOperationFailureMessage operation
                `shouldBe` Just "Gemini Veo video generation completed but did not return a generated video."

        it "does not summarize Veo operations with a generated video" $ do
            let operation =
                    GeminiVideoOperation
                        { name = GeminiVideoOperationName "models/veo-3.1-generate-preview/operations/op-1"
                        , done = True
                        , response =
                            Just
                                ( GeminiGenerateVideoResponse
                                    [ GeminiGeneratedVideo
                                        { uri = Just "https://example.com/video.mp4"
                                        , mimeType = Just "video/mp4"
                                        , videoBytes = Nothing
                                        }
                                    ]
                                )
                        , error = Nothing
                        }

            geminiVideoOperationFailureMessage operation `shouldBe` Nothing

    describe "request encoding" $ do
        it "defaults to Grok 2.0 with explicit low quality" $ do
            toJSON (defaultXAIImagineImageRequest "draw a lighthouse")
                `shouldBe` object
                    [ "model" .= ("grok-imagine-image-2.0" :: Text)
                    , "prompt" .= ("draw a lighthouse" :: Text)
                    , "quality" .= ("low" :: Text)
                    ]

        it "forwards both OpenAI identifiers without changing provider precedence" $ do
            let request :: OpenAIImageRequest
                request =
                    (defaultOpenAIImageRequest "draw a lighthouse")
                        { user = Just "legacy-user"
                        , safetyIdentifier = Just "hashed-user"
                        }
            toJSON request
                `shouldBe` object
                    [ "model" .= ("gpt-image-2.5-flare" :: Text)
                    , "prompt" .= ("draw a lighthouse" :: Text)
                    , "moderation" .= ("low" :: Text)
                    , "user" .= ("legacy-user" :: Text)
                    , "safety_identifier" .= ("hashed-user" :: Text)
                    ]

        it "encodes default OpenAI image requests for gpt-image-2.5-flare" $ do
            toJSON (defaultOpenAIImageRequest "draw a lighthouse")
                `shouldBe` object
                    [ "model" .= ("gpt-image-2.5-flare" :: Text)
                    , "prompt" .= ("draw a lighthouse" :: Text)
                    , "moderation" .= ("low" :: Text)
                    ]

        it "encodes explicit OpenAI image moderation" $ do
            toJSON
                ( (defaultOpenAIImageRequest "draw a lighthouse")
                    { moderation = Just OpenAIImageModerationAuto
                    }
                )
                `shouldBe` object
                    [ "model" .= ("gpt-image-2.5-flare" :: Text)
                    , "prompt" .= ("draw a lighthouse" :: Text)
                    , "moderation" .= ("auto" :: Text)
                    ]

        it "encodes OpenAI image edits with JSON image references" $ do
            let request :: OpenAIImageRequest
                request =
                    (defaultOpenAIImageRequest "add a moon")
                        { inputImages =
                            [ OpenAIImageUrl "https://example.com/base.png"
                            , OpenAIImageFileId "file-123"
                            ]
                        , mask = Just (OpenAIImageUrl "https://example.com/mask.png")
                        , inputFidelity = Just "high"
                        , outputFormat = Just "png"
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("gpt-image-2.5-flare" :: Text)
                    , "prompt" .= ("add a moon" :: Text)
                    , "images"
                        .= ( [ object ["image_url" .= ("https://example.com/base.png" :: Text)]
                             , object ["file_id" .= ("file-123" :: Text)]
                             ]
                                :: [Value]
                           )
                    , "mask" .= object ["image_url" .= ("https://example.com/mask.png" :: Text)]
                    , "input_fidelity" .= ("high" :: Text)
                    , "moderation" .= ("low" :: Text)
                    , "output_format" .= ("png" :: Text)
                    ]

        it "encodes xAI image generation requests" $ do
            let request :: XAIImagineImageRequest
                request =
                    XAIImagineImageRequest
                        { model = "grok-imagine-image"
                        , prompt = "mountain sunrise"
                        , inputImages = []
                        , n = Just 2
                        , aspectRatio = Just "16:9"
                        , resolution = Just "2k"
                        , responseFormat = Just "b64_json"
                        , quality = Nothing
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("grok-imagine-image" :: Text)
                    , "prompt" .= ("mountain sunrise" :: Text)
                    , "n" .= (2 :: Int)
                    , "aspect_ratio" .= ("16:9" :: Text)
                    , "resolution" .= ("2k" :: Text)
                    , "response_format" .= ("b64_json" :: Text)
                    ]

        it "encodes OpenAI TTS options" $ do
            let options =
                    OpenAITTSOptions
                        { model = OpenAITTSModelGPT4OMiniTTS
                        , voice = OpenAIVoiceVerse
                        , instructions = Just "Speak calmly and clearly"
                        , responseFormat = Just OpenAIAudioFormatWav
                        , speed = Just 1.25
                        }

            toJSON options
                `shouldBe` object
                    [ "model" .= ("gpt-4o-mini-tts" :: Text)
                    , "voice" .= ("verse" :: Text)
                    , "instructions" .= ("Speak calmly and clearly" :: Text)
                    , "response_format" .= ("wav" :: Text)
                    , "speed" .= (1.25 :: Double)
                    ]

        it "encodes xAI TTS options" $ do
            let options =
                    XAITTSOptions
                        { voice = XAITTSVoiceAra
                        , language = XAITTSLanguagePortugueseBrazil
                        , outputFormat =
                            Just
                                XAIOutputFormatMp3
                                    { sampleRate = Just XAISampleRate44100
                                    , bitRate = Just XAIMP3BitRate192000
                                    }
                        , speed = Just 0.75
                        }

            toJSON options
                `shouldBe` object
                    [ "voice_id" .= ("ara" :: Text)
                    , "language" .= ("pt-BR" :: Text)
                    , "output_format"
                        .= object
                            [ "codec" .= ("mp3" :: Text)
                            , "sample_rate" .= (44100 :: Int)
                            , "bit_rate" .= (192000 :: Int)
                            ]
                    , "speed" .= (0.75 :: Double)
                    ]

        it "omits xAI TTS speed by default" $ do
            toJSON defaultXAITTSOptions
                `shouldBe` object
                    [ "voice_id" .= ("eve" :: Text)
                    , "language" .= ("auto" :: Text)
                    ]

        it "encodes xAI image edits with a single source image" $ do
            let request :: XAIImagineImageRequest
                request =
                    XAIImagineImageRequest
                        { model = "grok-imagine-image"
                        , prompt = "turn this into watercolor"
                        , inputImages = ["data:image/png;base64,abc"]
                        , n = Nothing
                        , aspectRatio = Nothing
                        , resolution = Nothing
                        , responseFormat = Nothing
                        , quality = Nothing
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("grok-imagine-image" :: Text)
                    , "prompt" .= ("turn this into watercolor" :: Text)
                    , "image"
                        .= object
                            [ "url" .= ("data:image/png;base64,abc" :: Text)
                            , "type" .= ("image_url" :: Text)
                            ]
                    ]

        it "encodes xAI image edits with multiple source images" $ do
            let request :: XAIImagineImageRequest
                request =
                    XAIImagineImageRequest
                        { model = "grok-imagine-image"
                        , prompt = "combine these"
                        , inputImages =
                            [ "https://example.com/one.png"
                            , "https://example.com/two.png"
                            ]
                        , n = Nothing
                        , aspectRatio = Nothing
                        , resolution = Nothing
                        , responseFormat = Nothing
                        , quality = Nothing
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("grok-imagine-image" :: Text)
                    , "prompt" .= ("combine these" :: Text)
                    , "images"
                        .= ( [ object
                                [ "url" .= ("https://example.com/one.png" :: Text)
                                , "type" .= ("image_url" :: Text)
                                ]
                             , object
                                [ "url" .= ("https://example.com/two.png" :: Text)
                                , "type" .= ("image_url" :: Text)
                                ]
                             ]
                                :: [Value]
                           )
                    ]

        it "encodes Gemini image generation requests" $ do
            let request :: GeminiImageRequest
                request =
                    GeminiImageRequest
                        { model = "gemini-2.5-flash-image"
                        , prompt = "mountain sunrise"
                        , inputImages = []
                        , aspectRatio = Just "16:9"
                        , imageSize = Just "2K"
                        , safetySettings = leastRestrictiveGeminiSafetySettings
                        }

            toJSON request
                `shouldBe` object
                    [ "contents"
                        .= ( [ object
                                [ "role" .= ("user" :: Text)
                                , "parts"
                                    .= ( [ object ["text" .= ("mountain sunrise" :: Text)]
                                         ]
                                            :: [Value]
                                       )
                                ]
                             ]
                                :: [Value]
                           )
                    , "generationConfig"
                        .= object
                            [ "responseModalities"
                                .= (["TEXT", "IMAGE"] :: [Text])
                            , "imageConfig"
                                .= object
                                    [ "aspectRatio" .= ("16:9" :: Text)
                                    , "imageSize" .= ("2K" :: Text)
                                    ]
                            ]
                    , "safetySettings" .= leastRestrictiveGeminiSafetySettingsJson
                    ]

        it "encodes Gemini image edits with inline input images" $ do
            let request :: GeminiImageRequest
                request =
                    GeminiImageRequest
                        { model = "gemini-2.5-flash-image"
                        , prompt = "turn this into watercolor"
                        , inputImages =
                            [ GeminiInlineImage
                                { mimeType = "image/png"
                                , base64Data = "YWJj"
                                }
                            ]
                        , aspectRatio = Nothing
                        , imageSize = Nothing
                        , safetySettings = leastRestrictiveGeminiSafetySettings
                        }

            toJSON request
                `shouldBe` object
                    [ "contents"
                        .= ( [ object
                                [ "role" .= ("user" :: Text)
                                , "parts"
                                    .= ( [ object ["text" .= ("turn this into watercolor" :: Text)]
                                         , object
                                            [ "inline_data"
                                                .= object
                                                    [ "mime_type" .= ("image/png" :: Text)
                                                    , "data" .= ("YWJj" :: Text)
                                                    ]
                                            ]
                                         ]
                                            :: [Value]
                                       )
                                ]
                             ]
                                :: [Value]
                           )
                    , "generationConfig"
                        .= object
                            [ "responseModalities"
                                .= (["TEXT", "IMAGE"] :: [Text])
                            , "imageConfig" .= object []
                            ]
                    , "safetySettings" .= leastRestrictiveGeminiSafetySettingsJson
                    ]

        it "defaults Gemini Veo text-to-video person generation to allow_all" $ do
            toJSON (defaultGeminiVideoRequest "flower blooming")
                `shouldBe` object
                    [ "instances"
                        .= ( [ object
                                [ "prompt" .= ("flower blooming" :: Text)
                                ]
                             ]
                                :: [Value]
                           )
                    , "parameters"
                        .= object
                            [ "personGeneration" .= ("allow_all" :: Text)
                            ]
                    ]

        it "defaults Gemini Veo image requests to adult person generation" $ do
            let request =
                    (defaultGeminiVideoRequest "animate the still")
                        { image =
                            Just
                                GeminiInlineImage
                                    { mimeType = "image/png"
                                    , base64Data = "Zmlyc3Q="
                                    }
                        }

            toJSON request
                `shouldBe` object
                    [ "instances"
                        .= ( [ object
                                [ "prompt" .= ("animate the still" :: Text)
                                , "image"
                                    .= object
                                        [ "bytesBase64Encoded" .= ("Zmlyc3Q=" :: Text)
                                        , "mimeType" .= ("image/png" :: Text)
                                        ]
                                ]
                             ]
                                :: [Value]
                           )
                    , "parameters"
                        .= object
                            [ "personGeneration" .= ("allow_adult" :: Text)
                            ]
                    ]

        it "encodes Gemini Veo first and last frame requests" $ do
            let request :: GeminiVideoRequest
                request =
                    GeminiVideoRequest
                        { model = "veo-3.1-generate-preview"
                        , prompt = "move between these frames"
                        , image =
                            Just
                                GeminiInlineImage
                                    { mimeType = "image/png"
                                    , base64Data = "Zmlyc3Q="
                                    }
                        , lastFrame =
                            Just
                                GeminiInlineImage
                                    { mimeType = "image/png"
                                    , base64Data = "bGFzdA=="
                                    }
                        , durationSeconds = Just 8
                        , aspectRatio = Just "16:9"
                        , resolution = Just "720p"
                        , personGeneration = Just GeminiPersonGenerationAllowAdult
                        , seed = Just 123
                        }

            toJSON request
                `shouldBe` object
                    [ "instances"
                        .= ( [ object
                                [ "prompt" .= ("move between these frames" :: Text)
                                , "image"
                                    .= object
                                        [ "bytesBase64Encoded" .= ("Zmlyc3Q=" :: Text)
                                        , "mimeType" .= ("image/png" :: Text)
                                        ]
                                , "lastFrame"
                                    .= object
                                        [ "bytesBase64Encoded" .= ("bGFzdA==" :: Text)
                                        , "mimeType" .= ("image/png" :: Text)
                                        ]
                                ]
                             ]
                                :: [Value]
                           )
                    , "parameters"
                        .= object
                            [ "durationSeconds" .= (8 :: Int)
                            , "aspectRatio" .= ("16:9" :: Text)
                            , "resolution" .= ("720p" :: Text)
                            , "personGeneration" .= ("allow_adult" :: Text)
                            , "seed" .= (123 :: Int)
                            ]
                    ]

        it "encodes xAI text-to-video requests" $ do
            let request :: XAIImagineVideoRequest
                request =
                    XAIImagineVideoRequest
                        { model = "grok-imagine-video"
                        , prompt = "flower blooming"
                        , imageUrl = Nothing
                        , videoUrl = Nothing
                        , duration = Just 10
                        , aspectRatio = Just "16:9"
                        , resolution = Just "720p"
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("grok-imagine-video" :: Text)
                    , "prompt" .= ("flower blooming" :: Text)
                    , "duration" .= (10 :: Int)
                    , "aspect_ratio" .= ("16:9" :: Text)
                    , "resolution" .= ("720p" :: Text)
                    ]

        it "encodes xAI image-to-video requests" $ do
            let request :: XAIImagineVideoRequest
                request =
                    XAIImagineVideoRequest
                        { model = "grok-imagine-video"
                        , prompt = "animate the sky"
                        , imageUrl = Just "https://example.com/still.png"
                        , videoUrl = Nothing
                        , duration = Just 12
                        , aspectRatio = Nothing
                        , resolution = Nothing
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("grok-imagine-video" :: Text)
                    , "prompt" .= ("animate the sky" :: Text)
                    , "image" .= object ["url" .= ("https://example.com/still.png" :: Text)]
                    , "duration" .= (12 :: Int)
                    ]

        it "preserves explicit xAI video edit fields in JSON" $ do
            let request :: XAIImagineVideoRequest
                request =
                    XAIImagineVideoRequest
                        { model = "grok-imagine-video"
                        , prompt = "add a silver necklace"
                        , imageUrl = Nothing
                        , videoUrl = Just "https://example.com/source.mp4"
                        , duration = Just 10
                        , aspectRatio = Just "1:1"
                        , resolution = Just "480p"
                        }

            toJSON request
                `shouldBe` object
                    [ "model" .= ("grok-imagine-video" :: Text)
                    , "prompt" .= ("add a silver necklace" :: Text)
                    , "video" .= object ["url" .= ("https://example.com/source.mp4" :: Text)]
                    , "duration" .= (10 :: Int)
                    , "aspect_ratio" .= ("1:1" :: Text)
                    , "resolution" .= ("480p" :: Text)
                    ]

    describe "image HTTP requests" $ do
        forM_
            ["gpt-image-2", "gpt-image-2-2026-04-21", "gpt-image-2.5-flare", "gpt-image-2.5-sunburst"] $ \imageModel ->
            forM_ [Nothing, Just "png", Just "webp"] $ \format ->
                forM_ [[], [OpenAIImageUrl "https://example.com/input.png"]] $ \inputImages ->
                    it
                        ( "sends transparent "
                            <> toString imageModel
                            <> " with "
                            <> show format
                            <> " and "
                            <> show (length inputImages)
                            <> " inputs"
                        ) $ do
                        let request :: OpenAIImageRequest
                            request =
                                (defaultOpenAIImageRequest "draw a transparent icon")
                                    { model = imageModel
                                    , background = Just "transparent"
                                    , outputFormat = format
                                    , inputImages
                                    , safetyIdentifier = Just "hashed-user"
                                    }
                        (result, requests) <- captureImageRequest $ \baseUrl ->
                            runLlmError
                                $ generateOpenAIImage
                                    (defaultOpenAIImagesSettings "test-key"){OpenAI.baseUrl = baseUrl}
                                    request
                        result `shouldSatisfy` isRight
                        requests
                            `shouldBe` [
                                           ( if null inputImages then "/v1/images/generations" else "/v1/images/edits"
                                           , toJSON request
                                           )
                                       ]

        forM_ [Nothing, Just "low", Just "medium", Just "auto"] $ \quality ->
            forM_ [[], ["https://example.com/input.png"]] $ \inputImages ->
                it
                    ( "sends Grok quality "
                        <> show quality
                        <> " with "
                        <> show (length inputImages)
                        <> " inputs"
                    ) $ do
                    let request :: XAIImagineImageRequest
                        request = (defaultXAIImagineImageRequest "draw a lighthouse"){XAI.quality = quality, inputImages}
                        expectedBody :: Value
                        expectedBody =
                            object
                                $ ["model" .= ("grok-imagine-image-2.0" :: Text), "prompt" .= ("draw a lighthouse" :: Text)]
                                <> maybe [] (\value -> ["quality" .= value]) quality
                                <> [ "image" .= object ["url" .= imageUrl, "type" .= ("image_url" :: Text)]
                                   | imageUrl <- inputImages
                                   ]
                    (result, requests) <- captureImageRequest $ \baseUrl ->
                        runLlmError
                            $ generateXAIImage (defaultXAIImagineSettings "test-key"){XAI.baseUrl = baseUrl} request
                    result `shouldSatisfy` isRight
                    requests
                        `shouldBe` [ (if null inputImages then "/v1/images/generations" else "/v1/images/edits", expectedBody)
                                   ]

        it "allows legacy Grok when quality is omitted" $ do
            let request :: XAIImagineImageRequest
                request =
                    (defaultXAIImagineImageRequest "draw a lighthouse")
                        { model = "grok-imagine-image"
                        , XAI.quality = Nothing
                        }
            (result, requests) <- captureImageRequest $ \baseUrl ->
                runLlmError
                    $ generateXAIImage (defaultXAIImagineSettings "test-key"){XAI.baseUrl = baseUrl} request
            result `shouldSatisfy` isRight
            requests
                `shouldBe` [
                               ( "/v1/images/generations"
                               , object
                                    ["model" .= ("grok-imagine-image" :: Text), "prompt" .= ("draw a lighthouse" :: Text)]
                               )
                           ]

    describe "request validation" $ do
        it "fails fast on OpenAI masks without input images" $ do
            let request =
                    (defaultOpenAIImageRequest "add a moon")
                        { mask = Just (OpenAIImageUrl "https://example.com/mask.png")
                        }

            result <-
                runLlmError
                    $ generateOpenAIImage (defaultOpenAIImagesSettings "test-key") request

            result
                `shouldBe` Left (LlmExpectationError "OpenAI image masks require at least one input image")

        it "fails fast on OpenAI input fidelity without input images" $ do
            let request =
                    (defaultOpenAIImageRequest "sharpen this")
                        { inputFidelity = Just "high"
                        }

            result <-
                runLlmError
                    $ generateOpenAIImage (defaultOpenAIImagesSettings "test-key") request

            result
                `shouldBe` Left (LlmExpectationError "OpenAI inputFidelity requires at least one input image")

        it "fails fast on transparent backgrounds with JPEG output" $ do
            let request :: OpenAIImageRequest
                request =
                    (defaultOpenAIImageRequest "draw a transparent icon")
                        { OpenAI.background = Just "transparent"
                        , outputFormat = Just "jpeg"
                        }

            result <-
                runLlmError
                    $ generateOpenAIImage (defaultOpenAIImagesSettings "test-key") request

            result
                `shouldBe` Left (LlmExpectationError "OpenAI transparent backgrounds require png or webp output")

        it "rejects invalid Grok quality before HTTP" $ do
            result <-
                runLlmError
                    $ generateXAIImage
                        (defaultXAIImagineSettings "test-key"){XAI.baseUrl = "invalid URL"}
                        (defaultXAIImagineImageRequest "draw a lighthouse"){XAI.quality = Just "high"}
            result
                `shouldBe` Left (LlmExpectationError "xAI image quality must be low, medium, or auto")

        forM_
            (["grok-imagine-image", "grok-imagine-image-pro", "grok-imagine-image-quality"] :: [Text]) $ \legacyModel ->
            it ("rejects quality on legacy model " <> toString legacyModel) $ do
                result <-
                    runLlmError
                        $ generateXAIImage
                            (defaultXAIImagineSettings "test-key"){XAI.baseUrl = "invalid URL"}
                            (defaultXAIImagineImageRequest "draw a lighthouse")
                                { model = legacyModel
                                , XAI.quality = Just "low"
                                }
                result
                    `shouldBe` Left
                        ( LlmExpectationError
                            ("xAI image model " <> toString legacyModel <> " does not support quality")
                        )

        it "fails fast on explicit input fidelity for gpt-image-2" $ do
            let request =
                    (defaultOpenAIImageRequest "sharpen this")
                        { model = "gpt-image-2"
                        , inputImages = [OpenAIImageUrl "https://example.com/base.png"]
                        , inputFidelity = Just "high"
                        }

            result <-
                runLlmError
                    $ generateOpenAIImage (defaultOpenAIImagesSettings "test-key") request

            result
                `shouldBe` Left
                    ( LlmExpectationError
                        "OpenAI gpt-image-2 automatically uses high-fidelity input images and does not support inputFidelity"
                    )

        it "fails fast on OpenAI TTS instructions for unsupported models" $ do
            let settings :: OpenAITTSSettings '[Error RakeError, IOE]
                settings =
                    case defaultOpenAITTSSettings "test-key" of
                        OpenAITTSSettings{apiKey, baseUrl, organizationId, projectId, requestLogger, options = _} ->
                            OpenAITTSSettings
                                { apiKey
                                , baseUrl
                                , organizationId
                                , projectId
                                , options =
                                    defaultOpenAITTSOptions
                                        { model = OpenAITTSModelTTS1
                                        , instructions = Just "Speak gently"
                                        }
                                , requestLogger
                                }

            result <-
                runLlmError
                    $ generateOpenAISpeech settings "hello"

            result
                `shouldBe` Left (LlmExpectationError "OpenAI TTS instructions require gpt-4o-mini-tts")

        it "fails fast on xAI video edits with generation-only fields" $ do
            let request =
                    (defaultXAIImagineVideoRequest "add a silver necklace")
                        { videoUrl = Just "https://example.com/source.mp4"
                        , duration = Just 10
                        , aspectRatio = Just "1:1"
                        , resolution = Just "480p"
                        }

            result <-
                runLlmError
                    $ startXAIVideo (defaultXAIImagineSettings "test-key") request

            result
                `shouldBe` Left
                    ( LlmExpectationError
                        "xAI video edit requests cannot set duration, aspectRatio, or resolution"
                    )

        it "fails fast on xAI requests that set both image and video sources" $ do
            let request =
                    (defaultXAIImagineVideoRequest "add a silver necklace")
                        { imageUrl = Just "https://example.com/base.png"
                        , videoUrl = Just "https://example.com/source.mp4"
                        }

            result <-
                runLlmError
                    $ startXAIVideo (defaultXAIImagineSettings "test-key") request

            result
                `shouldBe` Left (LlmExpectationError "xAI video requests cannot set both imageUrl and videoUrl")

        it "fails fast on Gemini Veo lastFrame without image" $ do
            let request =
                    (defaultGeminiVideoRequest "move between frames")
                        { lastFrame =
                            Just
                                GeminiInlineImage
                                    { mimeType = "image/png"
                                    , base64Data = "bGFzdA=="
                                    }
                        }

            result <-
                runLlmError
                    $ startGeminiVideo (defaultGeminiVideoSettings "test-key") request

            result
                `shouldBe` Left (LlmExpectationError "Gemini Veo lastFrame requires image")

        it "fails fast on Gemini Veo first and last frame requests with a non-8 duration" $ do
            let inlineImage =
                    GeminiInlineImage
                        { mimeType = "image/png"
                        , base64Data = "Zmlyc3Q="
                        }
                request =
                    (defaultGeminiVideoRequest "move between frames")
                        { image = Just inlineImage
                        , lastFrame = Just inlineImage
                        , durationSeconds = Just 4
                        }

            result <-
                runLlmError
                    $ startGeminiVideo (defaultGeminiVideoSettings "test-key") request

            result
                `shouldBe` Left
                    ( LlmExpectationError
                        "Gemini Veo first/last frame interpolation requires durationSeconds=8"
                    )

captureImageRequest
    :: (Text -> IO (Either RakeError ImageGenerationResponse))
    -> IO (Either RakeError ImageGenerationResponse, [(ByteString, Value)])
captureImageRequest action = do
    requests <- newIORef []
    let application :: Wai.Application
        application request respond = do
            body <- Wai.strictRequestBody request
            modifyIORef' requests (<> [(Wai.rawPathInfo request, fromMaybe Null (decode body))])
            respond
                $ Wai.responseLBS
                    status200
                    [(hContentType, "application/json")]
                    (encode (object ["data" .= [object ["b64_json" .= ("YWJj" :: Text)]]]))
    result <- testWithApplication (pure application) $ \port ->
        action ("http://127.0.0.1:" <> show port)
    (result,) <$> readIORef requests

genOptionalText :: Gen (Maybe Text)
genOptionalText = fmap toText <$> arbitrary @(Maybe String)

genGeneratedImage :: Gen GeneratedImage
genGeneratedImage = do
    payload <- toText <$> arbitrary @String
    (url, b64Json) <-
        elements [(Just payload, Nothing), (Nothing, Just payload), (Just payload, Just payload)]
    revisedPrompt <- genOptionalText
    mimeType <- genOptionalText
    pure GeneratedImage{url, b64Json, revisedPrompt, mimeType}

genImageGenerationResponse :: Gen ImageGenerationResponse
genImageGenerationResponse = do
    created <- arbitrary
    images <- listOf genGeneratedImage
    model <- genOptionalText
    outputFormat <- genOptionalText
    size <- genOptionalText
    quality <- genOptionalText
    background <- genOptionalText
    ticks <- arbitrary @Integer
    usage <-
        oneof
            [ pure Nothing
            , pure
                ( Just
                    ( KM.fromList
                        [ ("cost_in_usd_ticks", toJSON ticks)
                        , ("input_tokens_details", object ["text_tokens" .= ticks])
                        ]
                    )
                )
            ]
    pure
        ImageGenerationResponse
            { created
            , images
            , model
            , outputFormat
            , size
            , quality
            , background
            , usage
            }

runLlmError :: Eff '[Error RakeError, IOE] a -> IO (Either RakeError a)
runLlmError =
    runEff
        . runErrorNoCallStack

leastRestrictiveGeminiSafetySettingsJson :: [Value]
leastRestrictiveGeminiSafetySettingsJson =
    [ safetySettingValue "HARM_CATEGORY_HARASSMENT"
    , safetySettingValue "HARM_CATEGORY_HATE_SPEECH"
    , safetySettingValue "HARM_CATEGORY_SEXUALLY_EXPLICIT"
    , safetySettingValue "HARM_CATEGORY_DANGEROUS_CONTENT"
    ]
  where
    safetySettingValue category =
        object
            [ "category" .= (category :: Text)
            , "threshold" .= ("OFF" :: Text)
            ]
