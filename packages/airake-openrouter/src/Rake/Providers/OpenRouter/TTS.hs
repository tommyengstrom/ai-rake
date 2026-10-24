{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.TTS
    ( OpenRouterSpeechModel (..)
    , CustomSpeechModel (..)
    , SpeechFormat (..)
    , GrokVoiceTts10 (..)
    , GrokVoice (..)
    , defaultGrokVoiceTts10
    , generateOpenRouterSpeech
    , streamOpenRouterSpeech
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.Media
import Rake.Providers.OpenRouter.Internal.Audio
import Rake.Providers.OpenRouter.Settings
import Rake.TTS.Types
import Relude

class OpenRouterSpeechModel model where
    speechModelName :: model -> Text
    speechModelConfig :: model -> Object

data CustomSpeechModel = CustomSpeechModel
    { modelName :: Text
    , settings :: Object
    }
    deriving stock (Show, Eq)

instance OpenRouterSpeechModel CustomSpeechModel where
    speechModelName CustomSpeechModel{modelName} = modelName
    speechModelConfig CustomSpeechModel{settings} = settings

data SpeechFormat = Mp3 | Pcm
    deriving stock (Show, Eq, Enum, Bounded, Generic)

instance ToJSON SpeechFormat where
    toJSON = genericToJSON defaultOptions{constructorTagModifier = map toLower}

instance FromJSON SpeechFormat where
    parseJSON = genericParseJSON defaultOptions{constructorTagModifier = map toLower}

data GrokVoice = Eve | Ara | Rex | Sal | Leo
    deriving stock (Show, Eq, Enum, Bounded, Generic)

instance ToJSON GrokVoice where
    toJSON = genericToJSON defaultOptions{constructorTagModifier = map toLower}

instance FromJSON GrokVoice where
    parseJSON = genericParseJSON defaultOptions{constructorTagModifier = map toLower}

data GrokVoiceTts10 = GrokVoiceTts10
    { voice :: GrokVoice
    , responseFormat :: SpeechFormat
    }
    deriving stock (Show, Eq)

defaultGrokVoiceTts10 :: GrokVoiceTts10
defaultGrokVoiceTts10 = GrokVoiceTts10 Eve Mp3

instance OpenRouterSpeechModel GrokVoiceTts10 where
    speechModelName _ = "x-ai/grok-voice-tts-1.0"
    speechModelConfig GrokVoiceTts10{voice, responseFormat} =
        KM.fromList ["voice" .= voice, "response_format" .= responseFormat]

generateOpenRouterSpeech
    :: (OpenRouterSpeechModel model, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> model -> Text -> Eff es Audio
generateOpenRouterSpeech = streamOpenRouterSpeech defaultTTSStreamCallbacks

streamOpenRouterSpeech
    :: (OpenRouterSpeechModel model, IOE :> es, Error RakeError :> es)
    => TTSStreamCallbacks es -> OpenRouterSettings es -> model -> Text -> Eff es Audio
streamOpenRouterSpeech TTSStreamCallbacks{onAudioChunk} settings@OpenRouterSettings
                                                            { apiKey
                                                            , baseUrl
                                                            , requestLogger
                                                            , requestTimeoutMilliseconds
                                                            , httpManager
                                                            } model input = do
    when (T.null (T.strip input))
        $ throwError (LlmExpectationError "Speech input must not be empty")
    body <-
        either throwError pure
            $ assembleRequest
                settings
                (speechModelName model)
                (speechModelConfig model)
                ["input", "stream"]
                (KM.singleton "input" (String input))
    BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} <-
        streamBinaryAudioRequest
            onAudioChunk
            requestLogger
            baseUrl
            "/audio/speech"
            [Just ("Authorization", Text.encodeUtf8 ("Bearer " <> apiKey))]
            httpManager
            requestTimeoutMilliseconds
            body
    pure
        Audio
            { audioBytes = responseBytes
            , mimeType = responseMimeType
            , fileName = responseFileName
            }
