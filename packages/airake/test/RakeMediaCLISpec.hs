{-# LANGUAGE NoOverloadedLists #-}

module RakeMediaCLISpec where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import RakeImageCLI
import RakeMediaOptions
import RakeTTSCLI
import RakeVideoCLI
import Relude
import Test.Hspec

spec :: Spec
spec = do
    describe "OpenRouter image CLI" $ do
        it "defaults to low-quality Grok" $ do
            options <- either (fail . toString) pure (parseGenImageArgs ["cat"])
            selectedModel options `shouldBe` "x-ai/grok-imagine-image-2.0"
            KM.lookup "quality" (modelSettings options) `shouldBe` Just (String "low")
        it "accepts both quality flag forms" $ do
            a <- either (fail . toString) pure (parseGenImageArgs ["--quality", "medium", "cat"])
            b <- either (fail . toString) pure (parseGenImageArgs ["--quality=medium", "cat"])
            a `shouldBe` b
        it "supports OpenAI maximum quality"
            $ parseGenImageArgs ["-m", "openai/gpt-image-2.5-flare", "--quality=max", "cat"]
            `shouldSatisfy` isRight
        it "rejects a quality unsupported by the chosen model" $ do
            parseGenImageArgs ["--quality=high", "cat"] `shouldSatisfy` isLeft
            parseGenImageArgs ["-m", "google/gemini-nano-banana-2.1", "--quality=medium", "cat"]
                `shouldSatisfy` isLeft
        it "accepts future models and raw settings" $ do
            options <-
                either
                    (fail . toString)
                    pure
                    (parseGenImageArgs ["-m", "future/new", "--model-options", "{\"new_option\":42}", "cat"])
            selectedModel options `shouldBe` "future/new"
            modelSettings options `shouldBe` KM.singleton "new_option" (Number 42)
        it "raw options allow newly supported settings on existing models"
            $ parseGenImageArgs ["--model-options", "{\"quality\":\"future\"}", "cat"]
            `shouldSatisfy` isRight
        it "rejects duplicate options and reserved fields" $ do
            parseGenImageArgs ["--quality=low", "--model-options", "{\"quality\":\"medium\"}", "cat"]
                `shouldSatisfy` isLeft
            parseGenImageArgs ["--model-options", "{\"prompt\":\"different\"}", "cat"]
                `shouldSatisfy` isLeft
        it "never guesses an unknown provider or model prefix"
            $ parseGenImageArgs ["-m", "gpt-image", "cat"]
            `shouldSatisfy` isLeft
        it "rejects native-only options and video options" $ do
            parseGenImageArgs ["--mask", "mask.png", "cat"] `shouldSatisfy` isLeft
            parseGenImageArgs ["--poll-interval-ms", "50", "cat"] `shouldSatisfy` isLeft
    describe "OpenRouter video CLI" $ do
        it "uses the current Grok route" $ do
            options <- either (fail . toString) pure (parseGenVideoArgs ["cat"])
            selectedModel options `shouldBe` "x-ai/grok-imagine-video-1.5"
        it "requires a first frame before a last frame"
            $ parseGenVideoArgs ["-m", "google/veo-3.1", "--last-frame", "last.png", "cat"]
            `shouldSatisfy` isLeft
        it "supports Grok 4:3 video" $
            parseGenVideoArgs ["--aspect-ratio", "4:3", "cat"] `shouldSatisfy` isRight
        it "rejects native edits and unsupported ratios" $ do
            parseGenVideoArgs ["--edit", "video.mp4", "cat"] `shouldSatisfy` isLeft
            parseGenVideoArgs ["--aspect-ratio", "99:1", "cat"] `shouldSatisfy` isLeft
    describe "OpenRouter speech CLI" $ do
        it "selects Grok Voice" $ do
            options <- either (fail . toString) pure (parseGenSpeechArgs ["hello"])
            selectedModel options `shouldBe` "x-ai/grok-voice-tts-1.0"
        it "rejects unsupported settings"
            $ parseGenSpeechArgs ["--speed", "2", "hello"]
            `shouldSatisfy` isLeft
