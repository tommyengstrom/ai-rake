module Rake.Providers.ElevenLabs.TTS
    ( ElevenLabsTTSSettings (..)
    , defaultElevenLabsTTSSettings
    , ElevenLabsTTSStreamingSettings (..)
    , defaultElevenLabsTTSStreamingSettings
    , ElevenLabsTTSOptions (..)
    , defaultElevenLabsTTSOptions
    , ElevenLabsControls (..)
    , defaultElevenLabsControls
    , ElevenLabsVoiceSettings (..)
    , defaultElevenLabsVoiceSettings
    , ElevenLabsOutputFormat (..)
    , renderElevenLabsOutputFormat
    , parseElevenLabsOutputFormat
    , validateElevenLabsTTSOptions
    , generateElevenLabsSpeech
    , streamElevenLabsSpeech
    ) where

import Data.Aeson
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import Network.HTTP.Types.URI (urlEncode)
import Rake.Effect
import Rake.Media
import Rake.Providers.ElevenLabs.Types
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Providers.TTS.Internal
import Rake.TTS.Types
import Relude

data ElevenLabsTTSSettings es = ElevenLabsTTSSettings
    { apiKey :: Text
    , baseUrl :: Text
    , options :: ElevenLabsTTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultElevenLabsTTSSettings :: Text -> ElevenLabsTTSSettings es
defaultElevenLabsTTSSettings apiKey =
    ElevenLabsTTSSettings
        { apiKey
        , baseUrl = "https://api.elevenlabs.io"
        , options = defaultElevenLabsTTSOptions
        , requestLogger = defaultWarningLogger "elevenlabs.tts"
        }

data ElevenLabsTTSStreamingSettings es = ElevenLabsTTSStreamingSettings
    { apiKey :: Text
    , baseUrl :: Text
    , options :: ElevenLabsTTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultElevenLabsTTSStreamingSettings :: Text -> ElevenLabsTTSStreamingSettings es
defaultElevenLabsTTSStreamingSettings apiKey =
    ElevenLabsTTSStreamingSettings
        { apiKey
        , baseUrl = "https://api.elevenlabs.io"
        , options = defaultElevenLabsTTSOptions
        , requestLogger = defaultWarningLogger "elevenlabs.tts"
        }

data ElevenLabsTTSOptions = ElevenLabsTTSOptions
    { model :: Text
    , voice :: Text
    , outputFormat :: ElevenLabsOutputFormat
    , controls :: ElevenLabsControls
    }
    deriving stock (Show, Eq)

defaultElevenLabsTTSOptions :: ElevenLabsTTSOptions
defaultElevenLabsTTSOptions =
    ElevenLabsTTSOptions
        { model = "eleven_v4"
        , voice = "JBFqnCBsd6RMkjVDRZzb"
        , outputFormat = ElevenLabsMp3_44100_128
        , controls = defaultElevenLabsControls
        }

validateElevenLabsTTSOptions :: ElevenLabsTTSOptions -> Either RakeError ()
validateElevenLabsTTSOptions ElevenLabsTTSOptions{model, voice, controls} = do
    validateSpeechIdentifier "model" model
    validateSpeechIdentifier "voice" voice
    validateElevenLabsControls controls

generateElevenLabsSpeech
    :: (IOE :> es, Error RakeError :> es)
    => ElevenLabsTTSSettings es -> Text -> Eff es Audio
generateElevenLabsSpeech ElevenLabsTTSSettings{apiKey, baseUrl, options, requestLogger} inputText = do
    either throwError pure (validateElevenLabsTTSOptions options)
    response <-
        postBinaryAudioRequest
            requestLogger
            baseUrl
            (elevenLabsSpeechPath False options)
            [Just ("xi-api-key", TextEncoding.encodeUtf8 apiKey)]
            (elevenLabsSpeechBody inputText options)
    pure (elevenLabsAudio options response)

streamElevenLabsSpeech
    :: (IOE :> es, Error RakeError :> es)
    => TTSStreamCallbacks es -> ElevenLabsTTSStreamingSettings es -> Text -> Eff es Audio
streamElevenLabsSpeech
    TTSStreamCallbacks{onAudioChunk}
    ElevenLabsTTSStreamingSettings
        { apiKey
        , baseUrl
        , options = options@ElevenLabsTTSOptions{outputFormat}
        , requestLogger
        }
    inputText = do
        either throwError pure (validateElevenLabsTTSOptions options)
        when
            (elevenLabsOutputMimeType outputFormat == "audio/wav")
            ( throwError
                ( LlmExpectationError
                    "ElevenLabs HTTP streaming does not support WAV; use MP3, PCM, Opus, ulaw, or alaw"
                )
            )
        response <-
            streamBinaryAudioRequest
                onAudioChunk
                requestLogger
                baseUrl
                (elevenLabsSpeechPath True options)
                [Just ("xi-api-key", TextEncoding.encodeUtf8 apiKey)]
                (elevenLabsSpeechBody inputText options)
        pure (elevenLabsAudio options response)

elevenLabsSpeechPath :: Bool -> ElevenLabsTTSOptions -> Text
elevenLabsSpeechPath streaming ElevenLabsTTSOptions{voice, outputFormat} =
    "/v1/text-to-speech/"
        <> TextEncoding.decodeUtf8 (urlEncode True (TextEncoding.encodeUtf8 voice))
        <> (if streaming then "/stream" else "")
        <> "?output_format="
        <> renderElevenLabsOutputFormat outputFormat

elevenLabsSpeechBody :: Text -> ElevenLabsTTSOptions -> Value
elevenLabsSpeechBody inputText ElevenLabsTTSOptions{model, controls} =
    object (["text" .= inputText, "model_id" .= model] <> elevenLabsControlPairs controls)

elevenLabsAudio :: ElevenLabsTTSOptions -> BinaryAudioResponse -> Audio
elevenLabsAudio ElevenLabsTTSOptions{outputFormat} BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} =
    Audio
        { audioBytes = responseBytes
        , mimeType = responseMimeType <|> Just (elevenLabsOutputMimeType outputFormat)
        , fileName = responseFileName
        }
