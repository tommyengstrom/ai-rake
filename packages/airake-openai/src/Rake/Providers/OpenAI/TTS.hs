module Rake.Providers.OpenAI.TTS
    ( OpenAITTSSettings (..)
    , defaultOpenAITTSSettings
    , OpenAITTSStreamingSettings (..)
    , defaultOpenAITTSStreamingSettings
    , OpenAITTSOptions (..)
    , defaultOpenAITTSOptions
    , OpenAITTSModel (..)
    , OpenAIVoice (..)
    , OpenAIAudioFormat (..)
    , generateOpenAISpeech
    , streamOpenAISpeech
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.Media
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Providers.TTS.Internal
import Rake.TTS.Types
import Relude

data OpenAITTSSettings es = OpenAITTSSettings
    { apiKey :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , options :: OpenAITTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenAITTSSettings :: Text -> OpenAITTSSettings es
defaultOpenAITTSSettings apiKey =
    OpenAITTSSettings
        { apiKey
        , baseUrl = "https://api.openai.com"
        , organizationId = Nothing
        , projectId = Nothing
        , options = defaultOpenAITTSOptions
        , requestLogger = defaultWarningLogger "openai.tts"
        }

data OpenAITTSStreamingSettings es = OpenAITTSStreamingSettings
    { apiKey :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , options :: OpenAITTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenAITTSStreamingSettings :: Text -> OpenAITTSStreamingSettings es
defaultOpenAITTSStreamingSettings apiKey =
    OpenAITTSStreamingSettings
        { apiKey
        , baseUrl = "https://api.openai.com"
        , organizationId = Nothing
        , projectId = Nothing
        , options = defaultOpenAITTSOptions
        , requestLogger = defaultWarningLogger "openai.tts"
        }

data OpenAITTSOptions = OpenAITTSOptions
    { model :: OpenAITTSModel
    , voice :: OpenAIVoice
    , instructions :: Maybe Text
    , responseFormat :: Maybe OpenAIAudioFormat
    , speed :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)

defaultOpenAITTSOptions :: OpenAITTSOptions
defaultOpenAITTSOptions =
    OpenAITTSOptions
        { model = OpenAITTSModelGPT4OMiniTTS
        , voice = OpenAIVoiceAlloy
        , instructions = Nothing
        , responseFormat = Nothing
        , speed = Nothing
        }

data OpenAITTSModel
    = OpenAITTSModelTTS1
    | OpenAITTSModelTTS1HD
    | OpenAITTSModelGPT4OMiniTTS
    deriving stock (Show, Eq, Generic)

data OpenAIVoice
    = OpenAIVoiceAlloy
    | OpenAIVoiceAsh
    | OpenAIVoiceBallad
    | OpenAIVoiceCoral
    | OpenAIVoiceEcho
    | OpenAIVoiceFable
    | OpenAIVoiceOnyx
    | OpenAIVoiceNova
    | OpenAIVoiceSage
    | OpenAIVoiceShimmer
    | OpenAIVoiceVerse
    | OpenAIVoiceId Text
    deriving stock (Show, Eq, Generic)

data OpenAIAudioFormat
    = OpenAIAudioFormatMp3
    | OpenAIAudioFormatOpus
    | OpenAIAudioFormatAac
    | OpenAIAudioFormatFlac
    | OpenAIAudioFormatWav
    | OpenAIAudioFormatPcm
    deriving stock (Show, Eq, Generic)

instance ToJSON OpenAITTSModel where
    toJSON =
        String . \case
            OpenAITTSModelTTS1 ->
                "tts-1"
            OpenAITTSModelTTS1HD ->
                "tts-1-hd"
            OpenAITTSModelGPT4OMiniTTS ->
                "gpt-4o-mini-tts"

instance ToJSON OpenAIVoice where
    toJSON =
        String . \case
            OpenAIVoiceAlloy ->
                "alloy"
            OpenAIVoiceAsh ->
                "ash"
            OpenAIVoiceBallad ->
                "ballad"
            OpenAIVoiceCoral ->
                "coral"
            OpenAIVoiceEcho ->
                "echo"
            OpenAIVoiceFable ->
                "fable"
            OpenAIVoiceOnyx ->
                "onyx"
            OpenAIVoiceNova ->
                "nova"
            OpenAIVoiceSage ->
                "sage"
            OpenAIVoiceShimmer ->
                "shimmer"
            OpenAIVoiceVerse ->
                "verse"
            OpenAIVoiceId voiceId ->
                voiceId

instance ToJSON OpenAIAudioFormat where
    toJSON =
        String . \case
            OpenAIAudioFormatMp3 ->
                "mp3"
            OpenAIAudioFormatOpus ->
                "opus"
            OpenAIAudioFormatAac ->
                "aac"
            OpenAIAudioFormatFlac ->
                "flac"
            OpenAIAudioFormatWav ->
                "wav"
            OpenAIAudioFormatPcm ->
                "pcm"

instance ToJSON OpenAITTSOptions where
    toJSON OpenAITTSOptions{model, voice, instructions, responseFormat, speed} =
        object
            $ [ "model" .= model
              , "voice" .= voice
              ]
            <> catMaybes
                [ ("instructions" .=) <$> instructions
                , ("response_format" .=) <$> responseFormat
                , ("speed" .=) <$> speed
                ]

generateOpenAISpeech
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => OpenAITTSSettings es
    -> Text
    -> Eff es Audio
generateOpenAISpeech OpenAITTSSettings{apiKey, baseUrl, organizationId, projectId, options, requestLogger} inputText = do
    either throwError pure (validateOpenAITTSOptions options)
    BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} <-
        postBinaryAudioRequest
            requestLogger
            baseUrl
            "/v1/audio/speech"
            [ Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))
            , ("OpenAI-Organization",) . TextEncoding.encodeUtf8 <$> organizationId
            , ("OpenAI-Project",) . TextEncoding.encodeUtf8 <$> projectId
            ]
            (openAITTSRequestBody inputText options)
    pure
        Audio
            { audioBytes = responseBytes
            , mimeType = responseMimeType <|> openAITTSMimeType options
            , fileName = responseFileName
            }

streamOpenAISpeech
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => TTSStreamCallbacks es
    -> OpenAITTSStreamingSettings es
    -> Text
    -> Eff es Audio
streamOpenAISpeech TTSStreamCallbacks{onAudioChunk} OpenAITTSStreamingSettings
                                                        { apiKey
                                                        , baseUrl
                                                        , organizationId
                                                        , projectId
                                                        , options
                                                        , requestLogger
                                                        } inputText = do
    either throwError pure (validateOpenAITTSOptions options)
    BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} <-
        streamBinaryAudioRequest
            onAudioChunk
            requestLogger
            baseUrl
            "/v1/audio/speech"
            [ Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))
            , ("OpenAI-Organization",) . TextEncoding.encodeUtf8 <$> organizationId
            , ("OpenAI-Project",) . TextEncoding.encodeUtf8 <$> projectId
            ]
            (openAITTSRequestBody inputText options)
    pure
        Audio
            { audioBytes = responseBytes
            , mimeType = responseMimeType <|> openAITTSMimeType options
            , fileName = responseFileName
            }

openAITTSRequestBody :: Text -> OpenAITTSOptions -> Value
openAITTSRequestBody inputText options =
    case toJSON options of
        Object optionsObject ->
            Object (KM.insert "input" (String inputText) optionsObject)
        _ ->
            object ["input" .= inputText]

validateOpenAITTSOptions :: OpenAITTSOptions -> Either RakeError ()
validateOpenAITTSOptions OpenAITTSOptions{model, instructions}
    | isJust instructions && model /= OpenAITTSModelGPT4OMiniTTS =
        Left (LlmExpectationError "OpenAI TTS instructions require gpt-4o-mini-tts")
    | otherwise =
        Right ()

openAITTSMimeType :: OpenAITTSOptions -> Maybe Text
openAITTSMimeType OpenAITTSOptions{responseFormat} =
    Just
        ( case fromMaybe OpenAIAudioFormatMp3 responseFormat of
            OpenAIAudioFormatMp3 ->
                "audio/mpeg"
            OpenAIAudioFormatOpus ->
                "audio/opus"
            OpenAIAudioFormatAac ->
                "audio/aac"
            OpenAIAudioFormatFlac ->
                "audio/flac"
            OpenAIAudioFormatWav ->
                "audio/wav"
            OpenAIAudioFormatPcm ->
                "audio/pcm"
        )
