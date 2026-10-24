{-# LANGUAGE NoOverloadedLists #-}

module Rake.OpenRouter.HttpSpec where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, lookup)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Rake.Chat
import Rake.MediaStorage.InMemory
import Rake.Providers.OpenRouter.Chat
import Rake.Providers.OpenRouter.Images
import Rake.Providers.OpenRouter.Settings
import Rake.Providers.OpenRouter.TTS
import Rake.Providers.OpenRouter.Videos
import Rake.TTS.Types
import Relude
import Test.Hspec

type RecordedRequest = (ByteString, [Text], Query, Maybe ByteString, Maybe Value)

spec :: Spec
spec = describe "OpenRouter HTTP contracts" $ do
    it "sends image settings and references to the dedicated endpoint" $ do
        (result, requests) <- capture (\_ -> (status200, "application/json", encode imageResponse)) $ \url ->
            runProvider
                $ generateOpenRouterImage
                    (settings url)
                    defaultGrokImagineImage20
                    (ImageRequest "cat" ["data:image/png;base64,YQ=="])
        result `shouldSatisfy` isRight
        case requests of
            [("POST", ["api", "v1", "images"], [], Just "Bearer test-key", Just (Object body))] -> do
                KM.lookup "model" body `shouldBe` Just (String "x-ai/grok-imagine-image-2.0")
                KM.lookup "quality" body `shouldBe` Just (String "low")
                KM.lookup "input_references" body
                    `shouldBe` Just
                        ( toJSON
                            [ object
                                [ "type" .= ("image_url" :: Text)
                                , "image_url" .= object ["url" .= ("data:image/png;base64,YQ==" :: Text)]
                                ]
                            ]
                        )
            other -> expectationFailure (show other)
        case result of
            Right ImageGenerationResponse{images = [GeneratedImage{mimeType}]} -> mimeType `shouldBe` Just "image/png"
            other -> expectationFailure (show other)
    it "reports provider error objects returned with HTTP 200" $ do
        (result, _) <- capture (\_ -> (status200, "application/json", "{\"error\":{\"message\":\"failed\"}}")) $ \url ->
            runProvider
                $ generateOpenRouterImage
                    (settings url)
                    defaultGrokImagineImage20
                    (defaultImageRequest "cat")
        result `shouldSatisfy` isLeft
    it "does not retry a failed generation POST" $ do
        (result, requests) <- capture (\_ -> (status503, "application/json", "{\"error\":{\"message\":\"busy\"}}")) $ \url ->
            runProvider
                $ generateOpenRouterImage
                    (settings url)
                    defaultGrokImagineImage20
                    (defaultImageRequest "cat")
        result `shouldSatisfy` isLeft
        length requests `shouldBe` 1
    it "polls a video job and downloads through the authenticated content endpoint" $ do
        (result, requests) <- capture videoReply $ \url -> runProvider do
            job <-
                generateOpenRouterVideo
                    (settings url)
                    (VideoPollSettings 0 2)
                    defaultGrokImagineVideo15
                    (defaultVideoRequest "cat")
            downloadOpenRouterVideo (settings url) job 0
        result `shouldBe` Right "video-bytes"
        map (\(_, path, _, _, _) -> path) requests
            `shouldBe` [ ["api", "v1", "videos"]
                       , ["api", "v1", "videos", "job-1"]
                       , ["api", "v1", "videos", "job-1", "content"]
                       ]
        case reverse requests of
            (_, _, query, auth, _) : _ -> do
                query `shouldBe` [("index", Just "0")]
                auth `shouldBe` Just "Bearer test-key"
            [] -> expectationFailure "no download"
    it "validates polling before submitting a billable job" $ do
        forM_ [VideoPollSettings 0 0, VideoPollSettings (-1) 2, VideoPollSettings maxBound 1] $ \polling -> do
            (result, requests) <- capture videoReply $ \url ->
                runProvider
                    $ generateOpenRouterVideo
                        (settings url)
                        polling
                        defaultGrokImagineVideo15
                        (defaultVideoRequest "cat")
            result `shouldSatisfy` isLeft
            requests `shouldBe` []
    it "bounds video polling" $ do
        (result, requests) <- capture (\_ -> (status200, "application/json", encode pendingJob)) $ \url ->
            runProvider
                $ generateOpenRouterVideo
                    (settings url)
                    (VideoPollSettings 0 2)
                    defaultGrokImagineVideo15
                    (defaultVideoRequest "cat")
        result `shouldSatisfy` isLeft
        length requests `shouldBe` 3
    it "rejects terminal video failure and mismatched poll IDs" $ do
        forM_ [failedJob, pendingJob{jobId = "wrong"}] $ \job -> do
            (result, _) <- capture (\_ -> (status200, "application/json", encode job)) $ \url ->
                runProvider $ waitOpenRouterVideo (settings url) (VideoPollSettings 0 1) pendingJob
            result `shouldSatisfy` isLeft
    it "streams binary speech and preserves its MIME type" $ do
        chunks <- newIORef []
        (result, requests) <- capture (\_ -> (status200, "audio/mpeg", "audio-bytes")) $ \url ->
            runProvider
                $ streamOpenRouterSpeech
                    defaultTTSStreamCallbacks
                        { onAudioChunk = \bytes -> liftIO (modifyIORef' chunks (<> [bytes]))
                        }
                    (settings url)
                    defaultGrokVoiceTts10
                    "hello"
        result `shouldBe` Right (Audio "audio-bytes" (Just "audio/mpeg") Nothing)
        mconcat <$> readIORef chunks `shouldReturn` "audio-bytes"
        map (\(_, path, _, _, _) -> path) requests `shouldBe` [["api", "v1", "audio", "speech"]]
    it "runs chat tools through stateless Responses and preserves custom settings" $ do
        (result, requests, connections) <- captureConnections chatReply $ \url -> runChat url do
            let tool :: ToolDef '[Rake, RakeMediaStorage, Error RakeError, Concurrent, IOE]
                tool = defineToolNoArgument "answer" "Return the answer" (pure (Right "42"))
            chatOutcome defaultChatConfig{tools = [tool]} [user "call the tool"]
        result `shouldSatisfy` isRight
        length requests `shouldBe` 2
        length (ordNub connections) `shouldBe` 1
        case reverse requests of
            (_, _, _, _, Just (Object fields)) : _ -> case KM.lookup "input" fields of
                Just (Array inputs) -> toList inputs `shouldContain` [encryptedReasoning]
                other -> expectationFailure (show other)
            other -> expectationFailure (show other)
        forM_ requests $ \(_, path, _, auth, body) -> do
            path `shouldBe` ["api", "v1", "responses"]
            auth `shouldBe` Just "Bearer test-key"
            case body of
                Just (Object fields) -> do
                    KM.lookup "store" fields `shouldBe` Just (Bool False)
                    KM.lookup "include" fields
                        `shouldBe` Just (toJSON (["reasoning.encrypted_content"] :: [Text]))
                    KM.lookup "new_setting" fields `shouldBe` Just (Number 7)
                other -> expectationFailure (show other)
    it "delivers SSE deltas and the final chat response" $ do
        deltas <- newIORef []
        let sse :: LBS.ByteString
            sse =
                "data: {\"type\":\"response.output_text.delta\",\"delta\":\"hello\"}\n\n"
                    <> "data: "
                    <> encode
                        (object ["type" .= ("response.completed" :: Text), "response" .= completedResponse])
                    <> "\n\n"
        (result, _) <- capture (\_ -> (status200, "text/event-stream", sse)) $ \url ->
            runChat url
                $ getLlmResponseStream
                    (\delta -> liftIO (modifyIORef' deltas (<> [delta])))
                    (\_ -> pure ())
                    []
                    Unstructured
                    defaultSamplingOptions
                    [user "hello"]
        result `shouldSatisfy` isRight
        readIORef deltas `shouldReturn` ["hello"]

    it "replays opaque reasoning only into the originating model" $ do
        forM_ ["future/chat", "another/model"] $ \model -> do
            let reasoning :: Value
                reasoning =
                    object
                        [ "type" .= ("reasoning" :: Text)
                        , "id" .= ("reason-1" :: Text)
                        , "encrypted_content" .= ("opaque-token" :: Text)
                        , "summary" .= ([] :: [Value])
                        ]
                response :: Value
                response =
                    object
                        [ "id" .= ("resp-1" :: Text)
                        , "status" .= ("completed" :: Text)
                        , "output"
                            .= [ reasoning
                               , object
                                    [ "type" .= ("message" :: Text)
                                    , "role" .= ("assistant" :: Text)
                                    , "content" .= ("hello" :: Text)
                                    ]
                               ]
                        ]
            ProviderRound{roundItems} <-
                either (fail . show) pure (decodeOpenRouterResponse model response)
            (_, requests) <- capture (\_ -> (status200, "application/json", encode completedResponse)) $ \url ->
                runChat url (getLlmResponse [] Unstructured defaultSamplingOptions roundItems)
            case requests of
                [(_, _, _, _, Just (Object fields))] ->
                    case KM.lookup "input" fields of
                        Just (Array items) -> (reasoning `elem` toList items) `shouldBe` (model == "future/chat")
                        other -> expectationFailure (show other)
                other -> expectationFailure (show other)

settings :: Text -> OpenRouterSettings es
settings url = (defaultOpenRouterSettings "test-key"){baseUrl = url, requestLogger = \_ -> pure ()}

runProvider :: Eff '[Concurrent, Error RakeError, IOE] a -> IO (Either RakeError a)
runProvider = runEff . runErrorNoCallStack @RakeError . runConcurrent

runChat
    :: Text
    -> Eff '[Rake, RakeMediaStorage, Error RakeError, Concurrent, IOE] a
    -> IO (Either RakeError a)
runChat url =
    runEff
        . runConcurrent
        . runErrorNoCallStack @RakeError
        . runRakeMediaStorageInMemory
        . runRakeOpenRouterChat
            (settings url)
            (CustomChatModel "future/chat" (KM.singleton "new_setting" (Number 7)))

capture
    :: (RecordedRequest -> (Status, ByteString, LBS.ByteString))
    -> (Text -> IO a)
    -> IO (a, [RecordedRequest])
capture reply action = do
    (result, requests, _) <- captureConnections reply action
    pure (result, requests)

captureConnections
    :: (RecordedRequest -> (Status, ByteString, LBS.ByteString))
    -> (Text -> IO a)
    -> IO (a, [RecordedRequest], [Text])
captureConnections reply action = do
    requests <- newIORef []
    connections <- newIORef []
    let app :: Wai.Application
        app request respond = do
            bytes <- Wai.strictRequestBody request
            let recorded :: RecordedRequest
                recorded =
                    ( Wai.requestMethod request
                    , Wai.pathInfo request
                    , Wai.queryString request
                    , lookup hAuthorization (Wai.requestHeaders request)
                    , decode bytes
                    )
                (status, contentType, body) = reply recorded
            modifyIORef' requests (<> [recorded])
            modifyIORef' connections (<> [show (Wai.remoteHost request)])
            respond (Wai.responseLBS status [(hContentType, contentType)] body)
    result <- testWithApplication (pure app) $ \port -> action ("http://127.0.0.1:" <> show port <> "/api/v1")
    (result,,) <$> readIORef requests <*> readIORef connections

imageResponse :: Value
imageResponse =
    object
        [ "data" .= [object ["b64_json" .= ("YQ==" :: Text), "media_type" .= ("image/png" :: Text)]]
        , "usage" .= object ["cost" .= (1 :: Int)]
        ]

pendingJob :: VideoJob
pendingJob = VideoJob "job-1" Pending Nothing Nothing Nothing Nothing

failedJob :: VideoJob
failedJob = pendingJob{status = Failed, failure = Just (object ["message" .= ("failed" :: Text)])}

videoReply :: RecordedRequest -> (Status, ByteString, LBS.ByteString)
videoReply (method, path, _, _, _) = case (method, reverse path) of
    ("POST", _) -> (status202, "application/json", encode pendingJob)
    (_, "content" : _) -> (status200, "video/mp4", "video-bytes")
    _ ->
        ( status200
        , "application/json"
        , encode
            pendingJob{status = Completed, contentUrls = Just ["https://untrusted.invalid/video.mp4"]}
        )

completedResponse :: Value
completedResponse =
    object
        [ "id" .= ("resp-2" :: Text)
        , "status" .= ("completed" :: Text)
        , "output"
            .= [ object
                    [ "type" .= ("message" :: Text)
                    , "id" .= ("msg-1" :: Text)
                    , "role" .= ("assistant" :: Text)
                    , "status" .= ("completed" :: Text)
                    , "content"
                        .= [ object
                                [ "type" .= ("output_text" :: Text)
                                , "text" .= ("hello" :: Text)
                                , "annotations" .= ([] :: [Value])
                                ]
                           ]
                    ]
               ]
        ]

chatReply :: RecordedRequest -> (Status, ByteString, LBS.ByteString)
chatReply (_, _, _, _, body) =
    ( status200
    , "application/json"
    , encode (if hasToolOutput then completedResponse else toolResponse)
    )
  where
    hasToolOutput :: Bool
    hasToolOutput = "function_call_output" `isInfixOf` show body
    toolResponse :: Value
    toolResponse =
        object
            [ "id" .= ("resp-1" :: Text)
            , "status" .= ("completed" :: Text)
            , "output"
                .= [ encryptedReasoning
                   , object
                        [ "type" .= ("function_call" :: Text)
                        , "id" .= ("fc-1" :: Text)
                        , "call_id" .= ("call-1" :: Text)
                        , "name" .= ("answer" :: Text)
                        , "arguments" .= ("{}" :: Text)
                        , "status" .= ("completed" :: Text)
                        ]
                   ]
            ]

encryptedReasoning :: Value
encryptedReasoning =
    object
        [ "type" .= ("reasoning" :: Text)
        , "id" .= ("reason-1" :: Text)
        , "encrypted_content" .= ("opaque-token" :: Text)
        , "summary" .= ([] :: [Value])
        ]
