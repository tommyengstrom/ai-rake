{-# LANGUAGE NoOverloadedLists #-}
module Rake.Provider.MediaSpec where
import Data.Aeson
import Effectful
import Effectful.Error.Static
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Rake.Effect
import Rake.Media
import Rake.Providers.OpenAI.Images
import Rake.Providers.OpenAI.Images qualified as OpenAI
import Rake.Providers.OpenAI.TTS
import Relude
import Test.Hspec

spec :: Spec
spec = describe "direct OpenAI media" $ do
    describe "request encoding" $ do
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

runLlmError :: Eff '[Error RakeError, IOE] a -> IO (Either RakeError a)
runLlmError =
    runEff
        . runErrorNoCallStack

