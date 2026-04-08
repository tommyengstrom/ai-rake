module Rake.Providers.XAI.TTS
    ( XAITTSSettings (..)
    , defaultXAITTSSettings
    , XAITTSStreamingSettings (..)
    , defaultXAITTSStreamingSettings
    , XAITTSOptions (..)
    , defaultXAITTSOptions
    , XAITTSVoice (..)
    , XAITTSLanguage (..)
    , XAISampleRate (..)
    , XAIMP3BitRate (..)
    , XAIOutputFormat (..)
    , generateXAISpeech
    , streamXAISpeech
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

data XAITTSSettings es = XAITTSSettings
    { apiKey :: Text
    , baseUrl :: Text
    , options :: XAITTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultXAITTSSettings :: Text -> XAITTSSettings es
defaultXAITTSSettings apiKey =
    XAITTSSettings
        { apiKey
        , baseUrl = "https://api.x.ai"
        , options = defaultXAITTSOptions
        , requestLogger = defaultWarningLogger "xai.tts"
        }

data XAITTSStreamingSettings es = XAITTSStreamingSettings
    { apiKey :: Text
    , baseUrl :: Text
    , options :: XAITTSOptions
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultXAITTSStreamingSettings :: Text -> XAITTSStreamingSettings es
defaultXAITTSStreamingSettings apiKey =
    XAITTSStreamingSettings
        { apiKey
        , baseUrl = "https://api.x.ai"
        , options = defaultXAITTSOptions
        , requestLogger = defaultWarningLogger "xai.tts"
        }

data XAITTSOptions = XAITTSOptions
    { voice :: XAITTSVoice
    , language :: XAITTSLanguage
    , outputFormat :: Maybe XAIOutputFormat
    }
    deriving stock (Show, Eq, Generic)

defaultXAITTSOptions :: XAITTSOptions
defaultXAITTSOptions =
    XAITTSOptions
        { voice = XAITTSVoiceEve
        , language = XAITTSLanguageAuto
        , outputFormat = Nothing
        }

data XAITTSVoice
    = XAITTSVoiceAra
    | XAITTSVoiceEve
    | XAITTSVoiceLeo
    | XAITTSVoiceRex
    | XAITTSVoiceSal
    | XAITTSVoiceId Text
    deriving stock (Show, Eq, Generic)

data XAITTSLanguage
    = XAITTSLanguageAuto
    | XAITTSLanguageEnglish
    | XAITTSLanguageArabicEgypt
    | XAITTSLanguageArabicSaudiArabia
    | XAITTSLanguageArabicUnitedArabEmirates
    | XAITTSLanguageBengali
    | XAITTSLanguageChineseSimplified
    | XAITTSLanguageFrench
    | XAITTSLanguageGerman
    | XAITTSLanguageHindi
    | XAITTSLanguageIndonesian
    | XAITTSLanguageItalian
    | XAITTSLanguageJapanese
    | XAITTSLanguageKorean
    | XAITTSLanguagePortugueseBrazil
    | XAITTSLanguagePortuguesePortugal
    | XAITTSLanguageRussian
    | XAITTSLanguageSpanishMexico
    | XAITTSLanguageSpanishSpain
    | XAITTSLanguageTurkish
    | XAITTSLanguageVietnamese
    | XAITTSLanguageCode Text
    deriving stock (Show, Eq, Generic)

data XAISampleRate
    = XAISampleRate8000
    | XAISampleRate16000
    | XAISampleRate22050
    | XAISampleRate24000
    | XAISampleRate44100
    | XAISampleRate48000
    deriving stock (Show, Eq, Generic)

data XAIMP3BitRate
    = XAIMP3BitRate32000
    | XAIMP3BitRate64000
    | XAIMP3BitRate96000
    | XAIMP3BitRate128000
    | XAIMP3BitRate192000
    deriving stock (Show, Eq, Generic)

data XAIOutputFormat
    = XAIOutputFormatMp3
        { sampleRate :: Maybe XAISampleRate
        , bitRate :: Maybe XAIMP3BitRate
        }
    | XAIOutputFormatWav
        { sampleRate :: Maybe XAISampleRate
        }
    | XAIOutputFormatPcm
        { sampleRate :: Maybe XAISampleRate
        }
    | XAIOutputFormatMuLaw
        { sampleRate :: Maybe XAISampleRate
        }
    | XAIOutputFormatALaw
        { sampleRate :: Maybe XAISampleRate
        }
    deriving stock (Show, Eq, Generic)

instance ToJSON XAITTSVoice where
    toJSON =
        String . \case
            XAITTSVoiceAra ->
                "ara"
            XAITTSVoiceEve ->
                "eve"
            XAITTSVoiceLeo ->
                "leo"
            XAITTSVoiceRex ->
                "rex"
            XAITTSVoiceSal ->
                "sal"
            XAITTSVoiceId voiceId ->
                voiceId

instance ToJSON XAITTSLanguage where
    toJSON =
        String . \case
            XAITTSLanguageAuto ->
                "auto"
            XAITTSLanguageEnglish ->
                "en"
            XAITTSLanguageArabicEgypt ->
                "ar-EG"
            XAITTSLanguageArabicSaudiArabia ->
                "ar-SA"
            XAITTSLanguageArabicUnitedArabEmirates ->
                "ar-AE"
            XAITTSLanguageBengali ->
                "bn"
            XAITTSLanguageChineseSimplified ->
                "zh"
            XAITTSLanguageFrench ->
                "fr"
            XAITTSLanguageGerman ->
                "de"
            XAITTSLanguageHindi ->
                "hi"
            XAITTSLanguageIndonesian ->
                "id"
            XAITTSLanguageItalian ->
                "it"
            XAITTSLanguageJapanese ->
                "ja"
            XAITTSLanguageKorean ->
                "ko"
            XAITTSLanguagePortugueseBrazil ->
                "pt-BR"
            XAITTSLanguagePortuguesePortugal ->
                "pt-PT"
            XAITTSLanguageRussian ->
                "ru"
            XAITTSLanguageSpanishMexico ->
                "es-MX"
            XAITTSLanguageSpanishSpain ->
                "es-ES"
            XAITTSLanguageTurkish ->
                "tr"
            XAITTSLanguageVietnamese ->
                "vi"
            XAITTSLanguageCode languageCode ->
                languageCode

instance ToJSON XAISampleRate where
    toJSON =
        Number . \case
            XAISampleRate8000 ->
                8000
            XAISampleRate16000 ->
                16000
            XAISampleRate22050 ->
                22050
            XAISampleRate24000 ->
                24000
            XAISampleRate44100 ->
                44100
            XAISampleRate48000 ->
                48000

instance ToJSON XAIMP3BitRate where
    toJSON =
        Number . \case
            XAIMP3BitRate32000 ->
                32000
            XAIMP3BitRate64000 ->
                64000
            XAIMP3BitRate96000 ->
                96000
            XAIMP3BitRate128000 ->
                128000
            XAIMP3BitRate192000 ->
                192000

instance ToJSON XAIOutputFormat where
    toJSON = \case
        XAIOutputFormatMp3{sampleRate, bitRate} ->
            object
                $ [ "codec" .= ("mp3" :: Text)
                  ]
                <> catMaybes
                    [ ("sample_rate" .=) <$> sampleRate
                    , ("bit_rate" .=) <$> bitRate
                    ]
        XAIOutputFormatWav{sampleRate} ->
            object
                $ [ "codec" .= ("wav" :: Text)
                  ]
                <> catMaybes
                    [ ("sample_rate" .=) <$> sampleRate
                    ]
        XAIOutputFormatPcm{sampleRate} ->
            object
                $ [ "codec" .= ("pcm" :: Text)
                  ]
                <> catMaybes
                    [ ("sample_rate" .=) <$> sampleRate
                    ]
        XAIOutputFormatMuLaw{sampleRate} ->
            object
                $ [ "codec" .= ("mulaw" :: Text)
                  ]
                <> catMaybes
                    [ ("sample_rate" .=) <$> sampleRate
                    ]
        XAIOutputFormatALaw{sampleRate} ->
            object
                $ [ "codec" .= ("alaw" :: Text)
                  ]
                <> catMaybes
                    [ ("sample_rate" .=) <$> sampleRate
                    ]

instance ToJSON XAITTSOptions where
    toJSON XAITTSOptions{voice, language, outputFormat} =
        object
            $ [ "voice_id" .= voice
              , "language" .= language
              ]
            <> catMaybes
                [ ("output_format" .=) <$> outputFormat
                ]

generateXAISpeech
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAITTSSettings es
    -> Text
    -> Eff es Audio
generateXAISpeech XAITTSSettings{apiKey, baseUrl, options, requestLogger} inputText = do
    BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} <-
        postBinaryAudioRequest
            requestLogger
            baseUrl
            "/v1/tts"
            [ Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))
            ]
            (xaiTTSRequestBody inputText options)
    pure
        Audio
            { audioBytes = responseBytes
            , mimeType = responseMimeType <|> xaiTTSMimeType options
            , fileName = responseFileName
            }

streamXAISpeech
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => TTSStreamCallbacks es
    -> XAITTSStreamingSettings es
    -> Text
    -> Eff es Audio
streamXAISpeech TTSStreamCallbacks{onAudioChunk} XAITTSStreamingSettings{apiKey, baseUrl, options, requestLogger} inputText = do
    BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} <-
        streamBinaryAudioRequest
            onAudioChunk
            requestLogger
            baseUrl
            "/v1/tts"
            [ Just ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))
            ]
            (xaiTTSRequestBody inputText options)
    pure
        Audio
            { audioBytes = responseBytes
            , mimeType = responseMimeType <|> xaiTTSMimeType options
            , fileName = responseFileName
            }

xaiTTSRequestBody :: Text -> XAITTSOptions -> Value
xaiTTSRequestBody inputText options =
    case toJSON options of
        Object optionsObject ->
            Object (KM.insert "text" (String inputText) optionsObject)
        _ ->
            object ["text" .= inputText]

xaiTTSMimeType :: XAITTSOptions -> Maybe Text
xaiTTSMimeType XAITTSOptions{outputFormat} =
    Just
        $ case outputFormat of
            Nothing ->
                "audio/mpeg"
            Just XAIOutputFormatMp3{} ->
                "audio/mpeg"
            Just XAIOutputFormatWav{} ->
                "audio/wav"
            Just XAIOutputFormatPcm{} ->
                "audio/pcm"
            Just XAIOutputFormatMuLaw{} ->
                "audio/basic"
            Just XAIOutputFormatALaw{} ->
                "audio/basic"
