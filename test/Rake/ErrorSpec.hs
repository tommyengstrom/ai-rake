module Rake.ErrorSpec where

import Data.Aeson (Value (..), object, (.=))
import Rake.Error
import Rake.Types
import Relude
import Test.Hspec

spec :: Spec
spec = describe "Rake.Error" $ do
    describe "extractJsonErrorDetail" $ do
        it "combines top-level code and error text" $ do
            extractJsonErrorDetail
                ( object
                    [ "code" .= ("Some requested entity was not found" :: Text)
                    , "error" .= ("The model grok-2-image-1212 was deprecated" :: Text)
                    ]
                )
                `shouldBe` Just "Some requested entity was not found: The model grok-2-image-1212 was deprecated"

        it "reads nested provider error messages" $ do
            extractJsonErrorDetail
                ( object
                    [ "error"
                        .= object
                            [ "message" .= ("Bad input image" :: Text)
                            ]
                    ]
                )
                `shouldBe` Just "Bad input image"

        it "returns plain string payloads unchanged" $ do
            extractJsonErrorDetail (String "temporary failure")
                `shouldBe` Just "temporary failure"

    describe "renderRakeError" $ do
        it "renders expectation errors directly" $ do
            renderRakeError (LlmExpectationError "boom")
                `shouldBe` "boom"

        it "renders blocked conversations with a reset checkpoint hint" $ do
            renderRakeError (ConversationBlocked (ReplayBlocked "history is blocked") (Just ResetToStart))
                `shouldBe` "history is blocked. Reset before continuing. Latest valid reset checkpoint: start"

        it "renders blocked conversations without a checkpoint hint when history has no stable ids" $ do
            renderRakeError (ConversationBlocked (ReplayBlocked "history is blocked") Nothing)
                `shouldBe`
                    "history is blocked. Reset before continuing. No concrete reset checkpoint can be suggested because the supplied history has no stable item ids."
