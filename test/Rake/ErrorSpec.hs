module Rake.ErrorSpec where

import Data.Aeson (Value (..), encode, object, (.=))
import Network.HTTP.Types.Status (accepted202)
import Network.HTTP.Types.Version (http11)
import Rake.Error
import Rake.Types
import Relude
import Servant.Client (BaseUrl (..), ClientError (..), ResponseF (..), Scheme (..))
import Servant.Client.Core.Request (RequestF (..))
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

        it "renders structured streaming callback errors directly" $ do
            renderRakeError (StreamingInternalError (OnAssistantTextDeltaFailed "stream callback exploded"))
                `shouldBe` "Streaming assistant text callback failed: stream callback exploded"

        it "renders structured streaming logger errors directly" $ do
            renderRakeError (StreamingInternalError (RequestLoggerFailed "responses: closed socket"))
                `shouldBe` "Streaming request logger failed: responses: closed socket"

        it "supports equality on structured streaming issues" $ do
            StreamingInternalError (OnAssistantRefusalDeltaFailed "boom")
                `shouldBe` StreamingInternalError (OnAssistantRefusalDeltaFailed "boom")

        it "renders pending failure responses tersely" $ do
            renderRakeError (LlmClientError pendingVideoFailureResponse)
                `shouldBe` "Provider request failed (HTTP 202 Accepted): status=pending, progress=1"

        it "can show and compare pending failure responses safely" $ do
            show pendingVideoFailureResponse
                `shouldContain` "FailureResponse"
            pendingVideoFailureResponse == pendingVideoFailureResponse
                `shouldBe` True

        it "renders blocked conversations with a reset checkpoint hint" $ do
            renderRakeError (ConversationBlocked (ReplayBlocked "history is blocked") (Just ResetToStart))
                `shouldBe` "history is blocked. Reset before continuing. Latest valid reset checkpoint: start"

        it "renders blocked conversations without a checkpoint hint when history has no stable ids" $ do
            renderRakeError (ConversationBlocked (ReplayBlocked "history is blocked") Nothing)
                `shouldBe`
                    "history is blocked. Reset before continuing. No concrete reset checkpoint can be suggested because the supplied history has no stable item ids."

pendingVideoFailureResponse :: ClientError
pendingVideoFailureResponse =
    FailureResponse pendingVideoRequest pendingVideoResponse

pendingVideoRequest :: RequestF () (BaseUrl, ByteString)
pendingVideoRequest =
    Request
        { requestPath = (BaseUrl Https "example.test" 443 "", "/v1/videos")
        , requestQueryString = mempty
        , requestBody = Nothing
        , requestAccept = mempty
        , requestHeaders = mempty
        , requestHttpVersion = http11
        , requestMethod = "POST"
        }

pendingVideoResponse :: ResponseF LByteString
pendingVideoResponse =
    Response
        { responseStatusCode = accepted202
        , responseHeaders = mempty
        , responseHttpVersion = http11
        , responseBody = encode (object ["status" .= ("pending" :: Text), "progress" .= (1 :: Int)])
        }
