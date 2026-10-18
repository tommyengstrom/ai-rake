module Rake.Providers.OpenRouter.TTS
    ( OpenRouterTTSSettings (..)
    , defaultOpenRouterTTSSettings
    , OpenRouterTTSStreamingSettings (..)
    , defaultOpenRouterTTSStreamingSettings
    , OpenRouterTTSOptions (..)
    , defaultOpenRouterTTSOptions
    , OpenRouterAudioFormat (..)
    , ElevenLabsControls (..)
    , defaultElevenLabsControls
    , ElevenLabsVoiceSettings (..)
    , defaultElevenLabsVoiceSettings
    , validateOpenRouterTTSOptions
    , generateOpenRouterSpeech
    , streamOpenRouterSpeech
    ) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.Media
import Rake.Providers.ElevenLabs.Types
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Providers.TTS.Internal
import Rake.TTS.Types
import Relude

data OpenRouterTTSSettings es = OpenRouterTTSSettings
    { apiKey :: Text
    , baseUrl :: Text
    , options :: OpenRouterTTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenRouterTTSSettings :: Text -> OpenRouterTTSSettings es
defaultOpenRouterTTSSettings apiKey =
    OpenRouterTTSSettings
        { apiKey
        , baseUrl = "https://openrouter.ai"
        , options = defaultOpenRouterTTSOptions
        , requestLogger = defaultWarningLogger "openrouter.tts"
        }

data OpenRouterTTSStreamingSettings es = OpenRouterTTSStreamingSettings
    { apiKey :: Text
    , baseUrl :: Text
    , options :: OpenRouterTTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenRouterTTSStreamingSettings :: Text -> OpenRouterTTSStreamingSettings es
defaultOpenRouterTTSStreamingSettings apiKey =
    OpenRouterTTSStreamingSettings
        { apiKey
        , baseUrl = "https://openrouter.ai"
        , options = defaultOpenRouterTTSOptions
        , requestLogger = defaultWarningLogger "openrouter.tts"
        }

data OpenRouterTTSOptions = OpenRouterTTSOptions
    { model :: Text
    , voice :: Text
    , responseFormat :: OpenRouterAudioFormat
    , elevenLabsControls :: ElevenLabsControls
    }
    deriving stock (Show, Eq)

defaultOpenRouterTTSOptions :: OpenRouterTTSOptions
defaultOpenRouterTTSOptions =
    OpenRouterTTSOptions
        { model = "elevenlabs/eleven-v4"
        , voice = "george"
        , responseFormat = OpenRouterAudioFormatMp3
        , elevenLabsControls = defaultElevenLabsControls
        }

data OpenRouterAudioFormat = OpenRouterAudioFormatMp3 | OpenRouterAudioFormatPcm
    deriving stock (Show, Eq, Enum, Bounded, Generic)

formatJSONOptions :: Options
formatJSONOptions =
    defaultOptions
        { constructorTagModifier =
            fmap Char.toLower . drop (length ("OpenRouterAudioFormat" :: String))
        }

instance ToJSON OpenRouterAudioFormat where
    toJSON = genericToJSON formatJSONOptions

instance FromJSON OpenRouterAudioFormat where
    parseJSON = genericParseJSON formatJSONOptions

validateOpenRouterTTSOptions :: OpenRouterTTSOptions -> Either RakeError ()
validateOpenRouterTTSOptions OpenRouterTTSOptions{model, voice, elevenLabsControls} = do
    validateSpeechIdentifier "model" model
    validateSpeechIdentifier "voice" voice
    unless
        ("elevenlabs/" `T.isPrefixOf` model || elevenLabsControls == defaultElevenLabsControls)
        ( Left
            (LlmExpectationError "ElevenLabs voice controls require an elevenlabs/ OpenRouter model")
        )
    validateElevenLabsControls elevenLabsControls

generateOpenRouterSpeech
    :: (IOE :> es, Error RakeError :> es)
    => OpenRouterTTSSettings es -> Text -> Eff es Audio
generateOpenRouterSpeech OpenRouterTTSSettings{apiKey, baseUrl, options, requestLogger} inputText = do
    either throwError pure (validateOpenRouterTTSOptions options)
    response <-
        postBinaryAudioRequest
            requestLogger
            baseUrl
            "/api/v1/audio/speech"
            [Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))]
            (openRouterSpeechBody inputText options)
    pure (openRouterAudio options response)

streamOpenRouterSpeech
    :: (IOE :> es, Error RakeError :> es)
    => TTSStreamCallbacks es -> OpenRouterTTSStreamingSettings es -> Text -> Eff es Audio
streamOpenRouterSpeech TTSStreamCallbacks{onAudioChunk} OpenRouterTTSStreamingSettings{apiKey, baseUrl, options, requestLogger} inputText = do
    either throwError pure (validateOpenRouterTTSOptions options)
    response <-
        streamBinaryAudioRequest
            onAudioChunk
            requestLogger
            baseUrl
            "/api/v1/audio/speech"
            [Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))]
            (openRouterSpeechBody inputText options)
    pure (openRouterAudio options response)

openRouterSpeechBody :: Text -> OpenRouterTTSOptions -> Value
openRouterSpeechBody inputText OpenRouterTTSOptions{model, voice, responseFormat, elevenLabsControls} =
    object
        ( [ "input" .= inputText
          , "model" .= model
          , "voice" .= voice
          , "response_format" .= responseFormat
          ]
            <> [ "provider" .= object ["options" .= object ["elevenlabs" .= object controlPairs]]
               | not (null controlPairs)
               ]
        )
  where
    controlPairs :: [Pair]
    controlPairs = elevenLabsControlPairs elevenLabsControls

openRouterAudio :: OpenRouterTTSOptions -> BinaryAudioResponse -> Audio
openRouterAudio OpenRouterTTSOptions{responseFormat} BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} =
    Audio
        { audioBytes = responseBytes
        , mimeType =
            responseMimeType
                <|> Just
                    ( case responseFormat of
                        OpenRouterAudioFormatMp3 -> "audio/mpeg"
                        OpenRouterAudioFormatPcm -> "audio/pcm"
                    )
        , fileName = responseFileName
        }
