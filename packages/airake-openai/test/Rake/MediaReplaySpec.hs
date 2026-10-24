{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rake.MediaReplaySpec where

import Control.Exception (finally)
import Data.Aeson
import Data.IORef qualified as IORef
import Data.UUID.V4 (nextRandom)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static
import Effectful.Time (runTime)
import Rake.Chat
import Rake.Backend (runChatProvider)
import Rake.MediaStorage.Directory (runRakeMediaStorageDirectory)
import Rake.MediaStorage.InMemory (runRakeMediaStorageInMemory)
import Rake.Provider.ResponsesRenderSpec qualified as Render
import Rake.Providers.OpenAI.Chat
    ( decodeOpenAIResponse
    , defaultOpenAIChatSettings
    , runRakeOpenAIChat
    )
import Rake.Providers.OpenAI.Chat qualified as OpenAI
import Rake.Storage.InMemory (runRakeStorageInMemory)
import Relude
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "OpenAI stored media replay" $ do
    describe "restart" $ do
        it "loses same-provider media replay after restart with the default ephemeral media store" $ do
            let payload =
                    Render.responsesAssistantPayloadWithContent
                        "item-openai-image"
                        [ object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url" .= ("https://example.com/cat.png" :: Text)
                            ]
                        ]
            decodedRound <-
                case decodeOpenAIResponse (Render.responsesResponse "response-openai" "completed" [payload]) of
                    Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                    Right roundValue -> pure roundValue

            result <-
                runEff
                    . runConcurrent
                    . runTime
                    . runErrorNoCallStackWith @ChatStorageError (error . show)
                    . runRakeStorageInMemory
                    $ do
                        let settings =
                                (defaultOpenAIChatSettings "test-api-key")
                                    { OpenAI.baseUrl = Render.unreachableBaseUrl
                                    , OpenAI.requestLogger = \_ -> pure ()
                                    }
                        convId <- createConversation
                        appendItems convId [user "show me a cat"]

                        firstResult <-
                            runErrorNoCallStack @RakeError
                                $ runRakeMediaStorageInMemory
                                $ runChatProvider (\_ _ _ _ -> pure decodedRound) (\_ _ _ _ _ -> pure decodedRound)
                                $ withResumableChat defaultChatConfig convId
                        _ <- either (error . show) pure firstResult

                        appendItems convId [user "what next?"]

                        runErrorNoCallStack @RakeError
                            $ runRakeMediaStorageInMemory
                            $ runRakeOpenAIChat settings
                            $ withResumableChat defaultChatConfig convId

            result
                `shouldBe` Left
                    (LlmExpectationError "No stored media reference is available for blob openai.responses-response-openai-item-openai-image-0 when rendering openai.responses")

        it "replays same-provider media after restart when using the directory media store" $
            withTempMediaDirectory \mediaDirectory -> do
                let payload =
                        Render.responsesAssistantPayloadWithContent
                            "item-openai-image"
                            [ object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .= ("https://example.com/cat.png" :: Text)
                                ]
                            ]
                decodedRound <-
                    case decodeOpenAIResponse (Render.responsesResponse "response-openai" "completed" [payload]) of
                        Left err -> expectationFailure ("Expected OpenAI response to decode: " <> show err) >> fail "unreachable"
                        Right roundValue -> pure roundValue

                requestRef <- IORef.newIORef Nothing

                result <-
                    runEff
                        . runConcurrent
                        . runTime
                        . runErrorNoCallStackWith @ChatStorageError (error . show)
                        . runRakeStorageInMemory
                        $ do
                            let settings =
                                    (defaultOpenAIChatSettings "test-api-key")
                                        { OpenAI.baseUrl = Render.unreachableBaseUrl
                                        , OpenAI.requestLogger = recordRequest requestRef
                                        }
                            convId <- createConversation
                            appendItems convId [user "show me a cat"]

                            firstResult <-
                                runErrorNoCallStack @RakeError
                                    $ runRakeMediaStorageDirectory mediaDirectory
                                    $ runChatProvider (\_ _ _ _ -> pure decodedRound) (\_ _ _ _ _ -> pure decodedRound)
                                    $ withResumableChat defaultChatConfig convId
                            _ <- either (error . show) pure firstResult

                            appendItems convId [user "what next?"]

                            runErrorNoCallStack @RakeError
                                $ runRakeMediaStorageDirectory mediaDirectory
                                $ runRakeOpenAIChat settings
                                $ withResumableChat defaultChatConfig convId

                result `shouldSatisfy` isLeft
                requestBody <- Render.readRequest requestRef
                Render.lookupPath ["input"] requestBody
                    `shouldBe` Just
                        ( toJSON
                            ( [ object
                                    [ "role" .= ("user" :: Text)
                                    , "content" .= ("show me a cat" :: Text)
                                    ]
                              , object
                                    [ "role" .= ("assistant" :: Text)
                                    , "content"
                                        .= ( [ object
                                                    [ "type" .= ("input_image" :: Text)
                                                    , "image_url" .= ("https://example.com/cat.png" :: Text)
                                                    ]
                                               ]
                                                :: [Value]
                                           )
                                    ]
                              , object
                                    [ "role" .= ("user" :: Text)
                                    , "content" .= ("what next?" :: Text)
                                    ]
                              ]
                                :: [Value]
                        )
                    )

    describe "stored generic media replay" $ do
        forM_ storedMediaRenderCases $ \StoredMediaRenderCase{caseName, providerFamily, captureStoredRequestBody} -> do
            it ("re-renders stored generic image history for " <> caseName) $ do
                let blobId = "blob-image-1"
                    requestPart = imageRequestPart providerFamily
                requestBody <-
                    captureStoredRequestBody
                        [MediaProviderReference{mediaBlobId = blobId, providerFamily, providerRequestPart = requestPart}]
                        [userParts [imagePart blobId (Just "image/png") (Just "diagram")]]

                Render.lookupPath ["input"] requestBody
                    `shouldBe` Just
                        ( toJSON
                            ( [storedMediaUserInput providerFamily requestPart]
                                :: [Value]
                            )
                        )

            it ("re-renders stored generic audio history for " <> caseName) $ do
                let blobId = "blob-audio-1"
                    requestPart = audioRequestPart providerFamily
                requestBody <-
                    captureStoredRequestBody
                        [MediaProviderReference{mediaBlobId = blobId, providerFamily, providerRequestPart = requestPart}]
                        [userParts [audioPart blobId (Just "audio/wav") (Just "spoken note")]]

                Render.lookupPath ["input"] requestBody
                    `shouldBe` Just
                        ( toJSON
                            ( [storedMediaUserInput providerFamily requestPart]
                                :: [Value]
                            )
                        )

data StoredMediaRenderCase = StoredMediaRenderCase
    { caseName :: String
    , providerFamily :: ProviderApiFamily
    , captureStoredRequestBody :: [MediaProviderReference] -> [HistoryItem] -> IO Value
    }

storedMediaRenderCases :: [StoredMediaRenderCase]
storedMediaRenderCases =
    [ StoredMediaRenderCase
        { caseName = "OpenAI"
        , providerFamily = ProviderOpenAIResponses
        , captureStoredRequestBody = captureStoredOpenAIRequestBody
        }

    ]

storedMediaUserInput :: ProviderApiFamily -> Value -> Value
storedMediaUserInput providerFamily requestPart =
    case providerFamily of
        ProviderGeminiInteractions ->
            object
                [ "type" .= ("user_input" :: Text)
                , "content" .= ([requestPart] :: [Value])
                ]
        _ ->
            object
                [ "role" .= ("user" :: Text)
                , "content" .= ([requestPart] :: [Value])
                ]

captureStoredOpenAIRequestBody :: [MediaProviderReference] -> [HistoryItem] -> IO Value
captureStoredOpenAIRequestBody mediaReferences history = do
    requestRef <- IORef.newIORef Nothing
    let settings =
            (defaultOpenAIChatSettings "test-api-key")
                { OpenAI.baseUrl = Render.unreachableBaseUrl
                , OpenAI.requestLogger = recordRequest requestRef
                }

    result <-
        runEff
            . runConcurrent
            . runTime
            . runErrorNoCallStackWith @ChatStorageError (error . show)
            . runRakeStorageInMemory
            $ do
                convId <- createConversation
                appendItems convId history
                runErrorNoCallStack @RakeError
                    $ runRakeMediaStorageInMemory
                    $ do
                        saveMediaReferences mediaReferences
                        runRakeOpenAIChat settings
                            $ void
                            $ withResumableChat defaultChatConfig convId

    result `shouldSatisfy` isLeft
    Render.readRequest requestRef

imageRequestPart :: ProviderApiFamily -> Value
imageRequestPart = \case
    ProviderOpenAIResponses ->
        responsesImageRequestPart
    ProviderXAIResponses ->
        responsesImageRequestPart
    ProviderGeminiInteractions ->
        object
            [ "type" .= ("image" :: Text)
            , "uri"
                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                        :: Text
                   )
            ]
    ProviderApiFamily{} ->
        error "Unexpected provider family in ReplaySpec imageRequestPart"
  where
    responsesImageRequestPart =
        object
            [ "type" .= ("input_image" :: Text)
            , "image_url"
                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                        :: Text
                   )
            ]

audioRequestPart :: ProviderApiFamily -> Value
audioRequestPart = \case
    ProviderOpenAIResponses ->
        responsesAudioRequestPart
    ProviderXAIResponses ->
        responsesAudioRequestPart
    ProviderGeminiInteractions ->
        geminiInlineMediaRequestPart "audio" "audio/wav" "UklGRg=="
    ProviderApiFamily{} ->
        error "Unexpected provider family in ReplaySpec audioRequestPart"
  where
    responsesAudioRequestPart =
        object
            [ "type" .= ("input_audio" :: Text)
            , "input_audio"
                .= object
                    [ "data" .= ("UklGRg==" :: Text)
                    , "format" .= ("wav" :: Text)
                    ]
            ]

geminiInlineMediaRequestPart :: Text -> Text -> Text -> Value
geminiInlineMediaRequestPart mediaType mimeType base64Data =
    object
        [ "type" .= mediaType
        , "mime_type" .= mimeType
        , "data" .= base64Data
        ]

withTempMediaDirectory :: (FilePath -> IO a) -> IO a
withTempMediaDirectory action = do
    tempRoot <- Directory.getTemporaryDirectory
    uuid <- nextRandom
    let mediaDirectory = tempRoot </> ("ai-rake-media-" <> show uuid)
    Directory.createDirectoryIfMissing True mediaDirectory
    action mediaDirectory
        `finally` Directory.removePathForcibly mediaDirectory

recordRequest :: IOE :> es => IORef.IORef (Maybe Value) -> NativeMsgFormat -> Eff es ()
recordRequest ref = \case
    NativeMsgOut value -> liftIO (IORef.writeIORef ref (Just value))
    NativeMsgIn{} -> pure ()
    NativeConversionNote{} -> pure ()
    NativeRequestFailure{} -> pure ()
