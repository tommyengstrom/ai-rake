module LlmChat.Provider.MediaSpec where

import Data.Aeson
import LlmChat.Media
import LlmChat.Providers.OpenAI.Images
import LlmChat.Providers.XAI.Imagine
import Relude
import Test.Hspec

spec :: Spec
spec = describe "media providers" $ do
    describe "image response parsing" $ do
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
                        , images =
                            [ GeneratedImage
                                { url = Nothing
                                , b64Json = Just "YWJj"
                                , revisedPrompt = Just "A refined prompt"
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
                        , images =
                            [ GeneratedImage
                                { url = Just "https://example.com/image.png"
                                , b64Json = Nothing
                                , revisedPrompt = Nothing
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

    describe "request encoding" $ do
        it "encodes default OpenAI image requests for gpt-image-1.5" $ do
            toJSON (defaultOpenAIImageRequest "draw a lighthouse")
                `shouldBe` object
                    [ "model" .= ("gpt-image-1.5" :: Text)
                    , "prompt" .= ("draw a lighthouse" :: Text)
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
                    [ "model" .= ("gpt-image-1.5" :: Text)
                    , "prompt" .= ("add a moon" :: Text)
                    , "images"
                        .= ( [ object ["image_url" .= ("https://example.com/base.png" :: Text)]
                             , object ["file_id" .= ("file-123" :: Text)]
                             ]
                                :: [Value]
                           )
                    , "mask" .= object ["image_url" .= ("https://example.com/mask.png" :: Text)]
                    , "input_fidelity" .= ("high" :: Text)
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

        it "encodes xAI video edit requests without generation-only fields" $ do
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
                    ]
