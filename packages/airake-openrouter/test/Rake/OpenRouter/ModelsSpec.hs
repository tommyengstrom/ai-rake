{-# LANGUAGE NoOverloadedLists #-}

module Rake.OpenRouter.ModelsSpec where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Rake.Providers.OpenRouter.Images
import Rake.Providers.OpenRouter.Models.GptImage25Flare qualified as Flare
import Rake.Providers.OpenRouter.Models.Veo31 qualified as Veo
import Rake.Providers.OpenRouter.Settings
import Rake.Providers.OpenRouter.TTS
import Rake.Providers.OpenRouter.Videos
import Relude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "model JSON codecs" $ do
        enumRoundtrip @GptImageQuality "OpenAI quality"
        enumRoundtrip @GrokImageQuality "Grok quality"
        enumRoundtrip @RasterFormat "raster formats"
        enumRoundtrip @ImageBackground "backgrounds"
        enumRoundtrip @ImageModeration "moderation"
        enumRoundtrip @GrokImageResolution "Grok resolutions"
        enumRoundtrip @GeminiImageResolution "Gemini resolutions"
        enumRoundtrip @GptImageRatio "OpenAI ratios"
        enumRoundtrip @GrokImageRatio "Grok ratios"
        enumRoundtrip @GeminiImageRatio "Gemini ratios"
        enumRoundtrip @GeminiExtendedImageRatio "extended Gemini ratios"
        enumRoundtrip @VideoResolution "video resolutions"
        enumRoundtrip @VideoRatio "video ratios"
        enumRoundtrip @SpeechFormat "speech formats"
        enumRoundtrip @GrokVoice "voices"
        it "roundtrips video status strings, including future statuses" $ property $ \(value :: String) ->
            (eitherDecode (encode (toText value)) :: Either String VideoStatus)
                >>= (Right . toJSON)
                & (=== Right (String (toText value)))
        it "roundtrips video jobs" $ property $ \(identifier :: String) ->
            forAll
                ( elements
                    [Pending, InProgress, Completed, Failed, Cancelled, Expired, UnknownStatus "future"]
                ) $ \status ->
                let job :: VideoJob
                    job =
                        VideoJob
                            (toText identifier)
                            status
                            (Just ["https://example.com/video"])
                            (Just "author/model")
                            (Just (KM.singleton "cost" (Number 2)))
                            Nothing
                 in eitherDecode (encode job) === Right job

    describe "image settings" $ do
        it "uses Flare and low Grok quality defaults" $ do
            imageModelName defaultGptImage25Flare `shouldBe` "openai/gpt-image-2.5-flare"
            KM.lookup "quality" (imageModelConfig defaultGrokImagineImage20)
                `shouldBe` Just (String "low")
        it "merges routing with OpenAI provider options" $ do
            let settings :: OpenRouterSettings '[]
                settings = (defaultOpenRouterSettings ""){provider = KM.singleton "allow_fallbacks" (Bool False)}
            body <-
                either
                    (fail . show)
                    pure
                    (renderImageRequest settings defaultGptImage25Flare (defaultImageRequest "cat"))
            field "provider" body
                `shouldBe` Just
                    ( object
                        [ "allow_fallbacks" .= False
                        , "options" .= object ["openai" .= object ["moderation" .= ("low" :: Text)]]
                        ]
                    )
        it "rejects conflicting routing values" $ do
            let settings :: OpenRouterSettings '[]
                settings = (defaultOpenRouterSettings ""){provider = KM.singleton "allow_fallbacks" (Bool False)}
                model :: CustomImageModel
                model =
                    CustomImageModel
                        "future/model"
                        (KM.singleton "provider" (object ["allow_fallbacks" .= True]))
            renderImageRequest settings model (defaultImageRequest "cat") `shouldSatisfy` isLeft
        forM_ ["model", "prompt", "input_references"] $ \key ->
            it ("rejects owned field " <> show key)
                $ renderImageRequest
                    (defaultOpenRouterSettings "")
                    (CustomImageModel "future/model" (KM.singleton key Null))
                    (defaultImageRequest "cat")
                `shouldSatisfy` isLeft
        it "rejects streaming through the buffered image API"
            $ renderImageRequest
                (defaultOpenRouterSettings "")
                (CustomImageModel "future/model" (KM.singleton "stream" (Bool True)))
                (defaultImageRequest "cat")
            `shouldSatisfy` isLeft
        it "enforces reference image limits"
            $ renderImageRequest
                (defaultOpenRouterSettings "")
                defaultGrokImagineImage20
                (ImageRequest "cat" (replicate 4 "https://example.com/cat.png"))
            `shouldSatisfy` isLeft
        it "rejects transparent JPEG and invalid dimensions" $ do
            validateImageModel
                ( defaultGptImage25Flare
                    { Flare.outputFormat = FormatJpeg
                    , Flare.background = Just BackgroundTransparent
                    }
                    :: GptImage25Flare
                )
                `shouldSatisfy` isLeft
            validateImageModel
                (defaultGptImage25Flare{Flare.dimensions = Just (ImagePixels 0 20)} :: GptImage25Flare)
                `shouldSatisfy` isLeft
        it "passes unknown models and options through" $ do
            let options :: Object
                options = KM.singleton "released_eight_minutes_ago" (object ["mode" .= ("new" :: Text)])
            body <-
                either (fail . show) pure
                    $ renderImageRequest
                        (defaultOpenRouterSettings "")
                        (CustomImageModel "future/new" options)
                        (defaultImageRequest "cat")
            field "released_eight_minutes_ago" body
                `shouldBe` KM.lookup "released_eight_minutes_ago" options

    describe "video settings" $ do
        it "rejects a last frame without a first frame"
            $ renderVideoRequest
                (defaultOpenRouterSettings "")
                defaultVeo31
                (VideoRequest "cat" Nothing (Just "https://example.com/cat.png"))
            `shouldSatisfy` isLeft
        it "rejects unsupported typed Veo ratios"
            $ renderVideoRequest
                (defaultOpenRouterSettings "")
                (defaultVeo31{Veo.ratio = Square} :: Veo31)
                (defaultVideoRequest "cat")
            `shouldSatisfy` isLeft
        it "rejects unsupported Grok last frames"
            $ renderVideoRequest
                (defaultOpenRouterSettings "")
                defaultGrokImagineVideo15
                (VideoRequest "cat" (Just "first") (Just "last"))
            `shouldSatisfy` isLeft

enumRoundtrip
    :: forall a. (Bounded a, Enum a, Show a, Eq a, ToJSON a, FromJSON a) => String -> Spec
enumRoundtrip name = it name
    $ property
    $ forAll (elements [minBound @a .. maxBound])
    $ \value -> eitherDecode (encode value) === Right value

field :: Text -> Value -> Maybe Value
field key = \case
    Object fields -> KM.lookup (fromString (toString key)) fields
    _ -> Nothing
