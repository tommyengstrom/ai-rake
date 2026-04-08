module Rake.Provider.XAISpec where

import Data.Aeson (object, (.=))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import ProviderAgnosticTests
import ProviderStructuredSchemaBehaviorTests
import Rake
import Rake.MediaStorage.InMemory
import Rake.Providers.XAI.Chat
import Rake.Providers.XAI.TTS
import Rake.Storage.InMemory
import Relude
import Test.Hspec

imageFixture :: RakeMediaStorage :> es => ProviderImageFixture es
imageFixture =
    ProviderImageFixture
        { providerFamily = ProviderXAIResponses
        , registerImageFixture = \blobId ->
            saveMediaReference
                MediaProviderReference
                    { mediaBlobId = blobId
                    , providerFamily = ProviderXAIResponses
                    , providerRequestPart =
                        object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url"
                                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                                        :: Text
                                   )
                            ]
                    }
        }

runEffectStack
    :: Text
    -> Eff
        '[ Rake
         , RakeStorage
         , Error ChatStorageError
         , RakeMediaStorage
         , Error RakeError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO a
runEffectStack apiKey action = do
    let settings = defaultXAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith @RakeError (error . show)
        . runRakeMediaStorageInMemory
        . runErrorNoCallStackWith @ChatStorageError (error . show)
        . runRakeStorageInMemory
        $ runRakeXAIChat settings action

runEffectStackResult
    :: Text
    -> Eff
        '[ Rake
         , RakeStorage
         , Error ChatStorageError
         , RakeMediaStorage
         , Error RakeError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either RakeError a)
runEffectStackResult apiKey action = do
    let settings = defaultXAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack @RakeError
        . runRakeMediaStorageInMemory
        . runErrorNoCallStackWith @ChatStorageError (error . show)
        . runRakeStorageInMemory
        $ runRakeXAIChat settings action

runStandaloneResult
    :: Eff '[Error RakeError, IOE] a
    -> IO (Either RakeError a)
runStandaloneResult =
    runEff
        . runErrorNoCallStack

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "XAI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "Rake Provider - xAI"
                $ it "requires XAI_API_KEY"
                $ pendingWith "Set XAI_API_KEY to run xAI integration tests."
        Just apiKey ->
            describe "Rake Provider - xAI" $ do
                specWithProvider imageFixture (runEffectStack (T.pack apiKey))
                specWithStructuredSchemaProvider
                    allStructuredSchemasAccepted
                    (runEffectStackResult (T.pack apiKey))
                describe "standalone TTS" $ do
                    it "generates non-empty audio through the shared batch TTS API" $ do
                        let xaiSettings = defaultXAITTSSettings (T.pack apiKey)
                        result <-
                            runStandaloneResult
                                $ tts
                                    (TTSXAI xaiSettings)
                                    "Hello from ai-rake."

                        case result of
                            Left rakeError ->
                                expectationFailure (toString (renderRakeError rakeError))
                            Right Audio{audioBytes} ->
                                BS.length audioBytes `shouldSatisfy` (> 0)

                    it "streams audio chunks and returns the concatenated audio" $ do
                        let xaiSettings = defaultXAITTSStreamingSettings (T.pack apiKey)
                        chunkRef <- newIORef []
                        result <-
                            runStandaloneResult
                                $ ttsStreaming
                                    TTSStreamCallbacks
                                        { onAudioChunk = \chunk ->
                                            liftIO (modifyIORef' chunkRef (chunk :))
                                        }
                                    (TTSStreamingXAI xaiSettings)
                                    "Hello from ai-rake."

                        streamedChunks <- BS.concat . reverse <$> readIORef chunkRef

                        case result of
                            Left rakeError ->
                                expectationFailure (toString (renderRakeError rakeError))
                            Right Audio{audioBytes} -> do
                                BS.length streamedChunks `shouldSatisfy` (> 0)
                                streamedChunks `shouldBe` audioBytes
