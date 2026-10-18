{-# LANGUAGE OverloadedRecordDot #-}

module Rake.Provider.SpeechSpec where

import Control.Concurrent.MVar qualified as MVar
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Network.HTTP.Types (Status, ResponseHeaders, Method, Query, RequestHeaders, status200, status429, hContentType, hAuthorization)
import Network.HTTP.Types.Header (hContentDisposition)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Rake
import Rake.Providers.ElevenLabs.TTS qualified as EL
import Rake.Providers.OpenRouter.TTS qualified as OR
import Relude
import Servant.Client (ClientError (..))
import System.Timeout qualified as Timeout
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, choose, elements, forAll, oneof)

spec :: Spec
spec = describe "ElevenLabs and OpenRouter speech" $ do
    describe "wire types" $ do
        it "roundtrips every native output format through text and JSON"
            $ forAll (elements [minBound .. maxBound])
            $ \format ->
                EL.parseElevenLabsOutputFormat (EL.renderElevenLabsOutputFormat format)
                    == Right format
                    && fromJSON (toJSON format)
                    == Success format

        it "roundtrips OpenRouter formats and uses the expected wire names" $ do
            toJSON OR.OpenRouterAudioFormatMp3 `shouldBe` String "mp3"
            toJSON OR.OpenRouterAudioFormatPcm `shouldBe` String "pcm"
            forM_ ([minBound .. maxBound] :: [OR.OpenRouterAudioFormat]) $ \format ->
                fromJSON (toJSON format) `shouldBe` Success (format :: OR.OpenRouterAudioFormat)

        it "roundtrips partial voice settings"
            $ forAll voiceSettingsGen
            $ \settings -> fromJSON (toJSON settings) == Success settings

    describe "direct ElevenLabs" $ do
        it "sends the native path, query, authentication, and default body" $ do
            (result, request) <- withAudioServer
                status200
                [ (hContentType, "audio/mpeg; charset=binary")
                , (hContentDisposition, "attachment; filename=voice.mp3")
                ]
                "audio"
                $ \base ->
                    runSpeech
                        (tts (TTSElevenLabs (directSettings base EL.defaultElevenLabsTTSOptions)) "Hello")
            result `shouldBe` Right (Audio "audio" (Just "audio/mpeg") (Just "voice.mp3"))
            request.capturedPath `shouldBe` "/v1/text-to-speech/JBFqnCBsd6RMkjVDRZzb"
            request.capturedQuery `shouldBe` [("output_format", Just "mp3_44100_128")]
            List.lookup "xi-api-key" request.capturedHeaders `shouldBe` Just "test-key"
            List.lookup hAuthorization request.capturedHeaders `shouldBe` Nothing
            request.capturedMethod `shouldBe` "POST"
            request.capturedBody
                `shouldBe` Just
                    (object ["text" .= ("Hello" :: Text), "model_id" .= ("eleven_v4" :: Text)])

        it "escapes voice identifiers and preserves all explicitly supplied controls" $ do
            let options :: EL.ElevenLabsTTSOptions
                options =
                    EL.defaultElevenLabsTTSOptions
                        { EL.voice = "voice/with ?#ü"
                        , EL.model = "eleven_flash_v2_5"
                        , EL.outputFormat = EL.ElevenLabsWav_24000
                        , EL.controls = fullControls
                        }
            (result, request) <- withAudioServer status200 [] "wav-data" $ \base ->
                runSpeech (tts (TTSElevenLabs (directSettings base options)) "Hej")
            result `shouldBe` Right (Audio "wav-data" (Just "audio/wav") Nothing)
            request.capturedPath `shouldBe` "/v1/text-to-speech/voice%2Fwith%20%3F%23%C3%BC"
            request.capturedQuery `shouldBe` [("output_format", Just "wav_24000")]
            request.capturedBody
                `shouldBe` Just
                    ( object
                        [ "text" .= ("Hej" :: Text)
                        , "model_id" .= ("eleven_flash_v2_5" :: Text)
                        , "language_code" .= ("sv" :: Text)
                        , "voice_settings" .= fullVoiceJSON
                        ]
                    )

        it "does not mislabel A-law as mu-law when MIME metadata is absent" $ do
            let options :: EL.ElevenLabsTTSOptions
                options = EL.defaultElevenLabsTTSOptions{EL.outputFormat = EL.ElevenLabsAlaw_8000}
            (result, _) <- withAudioServer status200 [] "alaw-data" $ \base ->
                runSpeech (tts (TTSElevenLabs (directSettings base options)) "Hello")
            result `shouldBe` Right (Audio "alaw-data" (Just "application/octet-stream") Nothing)

        it "streams raw audio through the /stream endpoint and aggregates callbacks" $ do
            chunks <- newIORef []
            (result, request) <- withAudioServer status200 [] "first-second" $ \base ->
                runSpeech
                    ( ttsStreaming
                        (collectChunks chunks)
                        (TTSStreamingElevenLabs (directStreamingSettings base EL.defaultElevenLabsTTSOptions))
                        "Hello"
                    )
            result `shouldBe` Right (Audio "first-second" (Just "audio/mpeg") Nothing)
            request.capturedPath `shouldBe` "/v1/text-to-speech/JBFqnCBsd6RMkjVDRZzb/stream"
            recorded <- readIORef chunks
            recorded `shouldSatisfy` (not . null)
            BS.concat recorded `shouldBe` "first-second"
            recorded `shouldSatisfy` all (not . BS.null)

        it "rejects streaming WAV before attempting HTTP" $ do
            let options :: EL.ElevenLabsTTSOptions
                options = EL.defaultElevenLabsTTSOptions{EL.outputFormat = EL.ElevenLabsWav_24000}
            result <-
                runSpeech
                    ( ttsStreaming
                        defaultTTSStreamCallbacks
                        (TTSStreamingElevenLabs (directStreamingSettings "not a url" options))
                        "Hello"
                    )
            result `shouldSatisfy` expectationContains "does not support WAV"

    describe "OpenRouter" $ do
        it "sends the OpenAI-compatible route with explicit MP3 and no empty provider options" $ do
            (result, request) <- withAudioServer status200 [] "mp3-data" $ \base ->
                runSpeech
                    (tts (TTSOpenRouter (routerSettings base OR.defaultOpenRouterTTSOptions)) "Hello")
            result `shouldBe` Right (Audio "mp3-data" (Just "audio/mpeg") Nothing)
            request.capturedPath `shouldBe` "/api/v1/audio/speech"
            request.capturedQuery `shouldBe` []
            List.lookup hAuthorization request.capturedHeaders `shouldBe` Just "Bearer test-key"
            List.lookup "xi-api-key" request.capturedHeaders `shouldBe` Nothing
            request.capturedBody
                `shouldBe` Just
                    ( object
                        [ "input" .= ("Hello" :: Text)
                        , "model" .= ("elevenlabs/eleven-v4" :: Text)
                        , "voice" .= ("george" :: Text)
                        , "response_format" .= ("mp3" :: Text)
                        ]
                    )

        it "nests ElevenLabs controls under provider.options.elevenlabs" $ do
            let options :: OR.OpenRouterTTSOptions
                options =
                    OR.defaultOpenRouterTTSOptions
                        { OR.model = "elevenlabs/eleven-flash-v2.5"
                        , OR.elevenLabsControls = fullControls
                        , OR.responseFormat = OR.OpenRouterAudioFormatPcm
                        }
            (result, request) <- withAudioServer status200 [] "pcm-data" $ \base ->
                runSpeech (tts (TTSOpenRouter (routerSettings base options)) "Hello")
            result `shouldBe` Right (Audio "pcm-data" (Just "audio/pcm") Nothing)
            request.capturedBody
                `shouldBe` Just
                    ( object
                        [ "input" .= ("Hello" :: Text)
                        , "model" .= ("elevenlabs/eleven-flash-v2.5" :: Text)
                        , "voice" .= ("george" :: Text)
                        , "response_format" .= ("pcm" :: Text)
                        , "provider"
                            .= object
                                [ "options"
                                    .= object
                                        [ "elevenlabs"
                                            .= object
                                                ["language_code" .= ("sv" :: Text), "voice_settings" .= fullVoiceJSON]
                                        ]
                                ]
                        ]
                    )

        it "streams using the same speech route" $ do
            chunks <- newIORef []
            (result, request) <- withAudioServer status200 [] "router-audio" $ \base ->
                runSpeech
                    ( ttsStreaming
                        (collectChunks chunks)
                        (TTSStreamingOpenRouter (routerStreamingSettings base))
                        "Hello"
                    )
            result `shouldBe` Right (Audio "router-audio" (Just "audio/mpeg") Nothing)
            request.capturedPath `shouldBe` "/api/v1/audio/speech"
            BS.concat <$> readIORef chunks `shouldReturn` "router-audio"

        it "accepts other provider models without ElevenLabs controls"
            $ OR.validateOpenRouterTTSOptions
                OR.defaultOpenRouterTTSOptions{OR.model = "other/speech", OR.voice = "custom"}
            `shouldBe` Right ()

        it "rejects ElevenLabs controls on other providers"
            $ OR.validateOpenRouterTTSOptions
                OR.defaultOpenRouterTTSOptions
                    { OR.model = "other/speech"
                    , OR.elevenLabsControls = fullControls
                    }
            `shouldSatisfy` expectationContains "require an elevenlabs/"

    describe "progressive streaming" $ do
        forM_ ([False, True] :: [Bool]) $ \viaRouter ->
            it
                ( if viaRouter
                    then "delivers OpenRouter chunks before EOF"
                    else "delivers ElevenLabs chunks before EOF"
                ) $ do
                delivered <- MVar.newEmptyMVar
                let app :: Wai.Application
                    app request respond = do
                        void (Wai.strictRequestBody request)
                        respond
                            ( Wai.responseStream status200 [(hContentType, "audio/mpeg")] $ \send flush -> do
                                send "first"
                                flush
                                ack <- Timeout.timeout 10_000_000 (MVar.takeMVar delivered)
                                send
                                    ( case ack of
                                        Nothing -> "-callback-was-late"
                                        Just () -> "second"
                                    )
                            )
                    callbacks :: TTSStreamCallbacks '[Error RakeError, IOE]
                    callbacks = TTSStreamCallbacks (\_ -> liftIO (void (MVar.tryPutMVar delivered ())))
                result <- Warp.testWithApplication (pure app) $ \port -> do
                    let base :: Text
                        base = "http://127.0.0.1:" <> show port
                        settings :: TTSStreamingSettings '[Error RakeError, IOE]
                        settings =
                            if viaRouter
                                then TTSStreamingOpenRouter (routerStreamingSettings base)
                                else TTSStreamingElevenLabs (directStreamingSettings base EL.defaultElevenLabsTTSOptions)
                    runSpeech (ttsStreaming callbacks settings "Hello")
                result `shouldBe` Right (Audio "firstsecond" (Just "audio/mpeg") Nothing)

    describe "validation and failures" $ do
        it "rejects blank model, voice, and language identifiers" $ do
            EL.validateElevenLabsTTSOptions EL.defaultElevenLabsTTSOptions{EL.model = " "}
                `shouldSatisfy` isLeft
            OR.validateOpenRouterTTSOptions OR.defaultOpenRouterTTSOptions{OR.voice = ""}
                `shouldSatisfy` isLeft
            EL.validateElevenLabsTTSOptions
                EL.defaultElevenLabsTTSOptions
                    { EL.controls = EL.defaultElevenLabsControls{EL.languageCode = Just " "}
                    }
                `shouldSatisfy` isLeft

        it "rejects nonfinite, nonpositive speed and out-of-range controls" $ do
            forM_ ([0, -1, 0 / 0, 1 / 0] :: [Double]) $ \speed ->
                EL.validateElevenLabsTTSOptions
                    EL.defaultElevenLabsTTSOptions
                        { EL.controls =
                            EL.defaultElevenLabsControls
                                { EL.voiceSettings = EL.defaultElevenLabsVoiceSettings{EL.speed = Just speed}
                                }
                        }
                    `shouldSatisfy` isLeft
            forM_ ([-0.1, 1.1, 0 / 0, 1 / 0] :: [Double]) $ \value ->
                forM_
                    ( [ EL.defaultElevenLabsVoiceSettings{EL.stability = Just value}
                      , EL.defaultElevenLabsVoiceSettings{EL.similarityBoost = Just value}
                      , EL.defaultElevenLabsVoiceSettings{EL.style = Just value}
                      ]
                        :: [EL.ElevenLabsVoiceSettings]
                    )
                    $ \settings ->
                        OR.validateOpenRouterTTSOptions
                            OR.defaultOpenRouterTTSOptions
                                { OR.elevenLabsControls = EL.defaultElevenLabsControls{EL.voiceSettings = settings}
                                }
                            `shouldSatisfy` isLeft

        it "propagates HTTP errors without delivering error JSON as audio" $ do
            chunks <- newIORef []
            (result, _) <- withAudioServer
                status429
                [(hContentType, "application/json")]
                "{\"error\":\"rate limit\"}"
                $ \base ->
                    runSpeech
                        ( ttsStreaming
                            (collectChunks chunks)
                            (TTSStreamingOpenRouter (routerStreamingSettings base))
                            "Hello"
                        )
            result `shouldSatisfy` \case
                Left (LlmClientError FailureResponse{}) -> True
                _ -> False
            readIORef chunks `shouldReturn` []

        it "redacts ElevenLabs API keys from connection errors" $ do
            closedUrl <-
                Warp.testWithApplication
                    (pure (\_ respond -> respond (Wai.responseLBS status200 [] "")))
                    (\port -> pure ("http://127.0.0.1:" <> show port))
            let key :: Text
                key = "sentinel-elevenlabs-secret"
                settings :: EL.ElevenLabsTTSSettings '[Error RakeError, IOE]
                settings = EL.ElevenLabsTTSSettings key closedUrl EL.defaultElevenLabsTTSOptions (const (pure ()))
            result <- runSpeech (tts (TTSElevenLabs settings) "Hello")
            case result of
                Left err@(LlmClientError (ConnectionError _)) -> do
                    renderRakeError err `shouldNotSatisfy` T.isInfixOf key
                    (show err :: Text) `shouldNotSatisfy` T.isInfixOf key
                Left err -> expectationFailure (toString (renderRakeError err))
                Right _ -> expectationFailure "Expected the stopped server to refuse the connection"

        it "preserves callback errors" $ do
            (result, _) <- withAudioServer status200 [] "audio" $ \base ->
                runSpeech
                    ( ttsStreaming
                        (TTSStreamCallbacks (\_ -> throwError (LlmExpectationError "callback failed")))
                        (TTSStreamingElevenLabs (directStreamingSettings base EL.defaultElevenLabsTTSOptions))
                        "Hello"
                    )
            result `shouldBe` Left (LlmExpectationError "callback failed")

    describe "optional live smoke tests" $ do
        it "generates speech directly when explicitly enabled" $ do
            key <- smokeKey "ELEVENLABS_API_KEY"
            result <- runSpeech (tts (TTSElevenLabs (EL.defaultElevenLabsTTSSettings key)) "Hello.")
            result `shouldSatisfy` hasAudio
        it "generates speech through OpenRouter when explicitly enabled" $ do
            key <- smokeKey "OPENROUTER_API_KEY"
            result <- runSpeech (tts (TTSOpenRouter (OR.defaultOpenRouterTTSSettings key)) "Hello.")
            result `shouldSatisfy` hasAudio

voiceSettingsGen :: Gen EL.ElevenLabsVoiceSettings
voiceSettingsGen =
    EL.ElevenLabsVoiceSettings
        <$> optionalUnit
        <*> optionalUnit
        <*> optionalUnit
        <*> oneof [pure Nothing, Just <$> choose (0.7, 1.2)]
        <*> arbitrary
  where
    optionalUnit :: Gen (Maybe Double)
    optionalUnit = oneof [pure Nothing, Just <$> choose (0, 1)]

fullControls :: EL.ElevenLabsControls
fullControls =
    EL.ElevenLabsControls
        (Just "sv")
        (EL.ElevenLabsVoiceSettings (Just 0.4) (Just 0.7) (Just 0.2) (Just 1.1) (Just False))

fullVoiceJSON :: Value
fullVoiceJSON =
    object
        [ "stability" .= (0.4 :: Double)
        , "similarity_boost" .= (0.7 :: Double)
        , "style" .= (0.2 :: Double)
        , "speed" .= (1.1 :: Double)
        , "use_speaker_boost" .= False
        ]

runSpeech :: Eff '[Error RakeError, IOE] a -> IO (Either RakeError a)
runSpeech = runEff . runErrorNoCallStack

directSettings
    :: Text -> EL.ElevenLabsTTSOptions -> EL.ElevenLabsTTSSettings '[Error RakeError, IOE]
directSettings base opts = EL.ElevenLabsTTSSettings "test-key" base opts (const (pure ()))

directStreamingSettings
    :: Text
    -> EL.ElevenLabsTTSOptions
    -> EL.ElevenLabsTTSStreamingSettings '[Error RakeError, IOE]
directStreamingSettings base opts = EL.ElevenLabsTTSStreamingSettings "test-key" base opts (const (pure ()))

routerSettings
    :: Text -> OR.OpenRouterTTSOptions -> OR.OpenRouterTTSSettings '[Error RakeError, IOE]
routerSettings base opts = OR.OpenRouterTTSSettings "test-key" base opts (const (pure ()))

routerStreamingSettings
    :: Text -> OR.OpenRouterTTSStreamingSettings '[Error RakeError, IOE]
routerStreamingSettings base =
    OR.OpenRouterTTSStreamingSettings
        "test-key"
        base
        OR.defaultOpenRouterTTSOptions
        (const (pure ()))

collectChunks :: IORef [ByteString] -> TTSStreamCallbacks '[Error RakeError, IOE]
collectChunks ref = TTSStreamCallbacks (\chunk -> liftIO (modifyIORef' ref (<> [chunk])))

expectationContains :: Text -> Either RakeError a -> Bool
expectationContains needle = \case
    Left (LlmExpectationError message) -> needle `T.isInfixOf` toText message
    _ -> False

hasAudio :: Either RakeError Audio -> Bool
hasAudio = \case
    Right Audio{audioBytes} -> not (BS.null audioBytes)
    Left _ -> False

smokeKey :: String -> IO Text
smokeKey keyName = do
    enabled <- lookupEnv "RAKE_TTS_LIVE_TESTS"
    key <- lookupEnv keyName
    case (enabled, key) of
        (Just "1", Just value) -> pure (toText value)
        _ ->
            pendingWith ("Set RAKE_TTS_LIVE_TESTS=1 and " <> keyName <> " for paid smoke tests")
                >> pure ""

data CapturedRequest = CapturedRequest
    { capturedMethod :: Method
    , capturedPath :: ByteString
    , capturedQuery :: Query
    , capturedHeaders :: RequestHeaders
    , capturedBody :: Maybe Value
    }

withAudioServer
    :: Status -> ResponseHeaders -> LBS.ByteString -> (Text -> IO a) -> IO (a, CapturedRequest)
withAudioServer status headers bytes action = do
    ref <- newIORef Nothing
    let app :: Wai.Application
        app request respond = do
            body <- Wai.strictRequestBody request
            writeIORef
                ref
                ( Just
                    ( CapturedRequest
                        (Wai.requestMethod request)
                        (Wai.rawPathInfo request)
                        (Wai.queryString request)
                        (Wai.requestHeaders request)
                        (decode body)
                    )
                )
            respond (Wai.responseLBS status headers bytes)
    result <-
        Warp.testWithApplication (pure app) (\port -> action ("http://127.0.0.1:" <> show port))
    captured <- readIORef ref >>= maybe (fail "Expected an HTTP request") pure
    pure (result, captured)
