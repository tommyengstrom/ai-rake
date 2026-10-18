module Rake.Providers.ElevenLabs.Types
    ( ElevenLabsVoiceSettings (..)
    , defaultElevenLabsVoiceSettings
    , ElevenLabsControls (..)
    , defaultElevenLabsControls
    , elevenLabsControlPairs
    , validateElevenLabsControls
    , validateSpeechIdentifier
    , ElevenLabsOutputFormat (..)
    , renderElevenLabsOutputFormat
    , parseElevenLabsOutputFormat
    , elevenLabsOutputMimeType
    ) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Char qualified as Char
import Data.Text qualified as T
import Rake.Error
import Relude

data ElevenLabsVoiceSettings = ElevenLabsVoiceSettings
    { stability :: Maybe Double
    , similarityBoost :: Maybe Double
    , style :: Maybe Double
    , speed :: Maybe Double
    , useSpeakerBoost :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

defaultElevenLabsVoiceSettings :: ElevenLabsVoiceSettings
defaultElevenLabsVoiceSettings = ElevenLabsVoiceSettings Nothing Nothing Nothing Nothing Nothing

voiceJSONOptions :: Options
voiceJSONOptions = defaultOptions{fieldLabelModifier = camelTo2 '_', omitNothingFields = True}

instance ToJSON ElevenLabsVoiceSettings where
    toJSON = genericToJSON voiceJSONOptions

instance FromJSON ElevenLabsVoiceSettings where
    parseJSON = genericParseJSON voiceJSONOptions

data ElevenLabsControls = ElevenLabsControls
    { languageCode :: Maybe Text
    , voiceSettings :: ElevenLabsVoiceSettings
    }
    deriving stock (Show, Eq)

defaultElevenLabsControls :: ElevenLabsControls
defaultElevenLabsControls = ElevenLabsControls Nothing defaultElevenLabsVoiceSettings

elevenLabsControlPairs :: ElevenLabsControls -> [Pair]
elevenLabsControlPairs ElevenLabsControls{languageCode, voiceSettings} =
    catMaybes
        [ ("language_code" .=) <$> languageCode
        , if voiceSettings == defaultElevenLabsVoiceSettings
            then Nothing
            else Just ("voice_settings" .= voiceSettings)
        ]

validateElevenLabsControls :: ElevenLabsControls -> Either RakeError ()
validateElevenLabsControls
    ElevenLabsControls
        { languageCode
        , voiceSettings = ElevenLabsVoiceSettings{stability, similarityBoost, style, speed}
        } = do
        traverse_ (validateSpeechIdentifier "language") languageCode
        traverse_ (validateUnitInterval "stability") stability
        traverse_ (validateUnitInterval "similarity") similarityBoost
        traverse_ (validateUnitInterval "style") style
        traverse_ validateSpeed speed
      where
        validateUnitInterval :: Text -> Double -> Either RakeError ()
        validateUnitInterval name value =
            unless
                (isFinite value && value >= 0 && value <= 1)
                (Left (LlmExpectationError (toString name <> " must be a finite number between 0 and 1")))

        validateSpeed :: Double -> Either RakeError ()
        validateSpeed value =
            unless
                (isFinite value && value > 0)
                (Left (LlmExpectationError "speed must be a finite number greater than 0"))

        isFinite :: Double -> Bool
        isFinite value = not (isNaN value || isInfinite value)

validateSpeechIdentifier :: Text -> Text -> Either RakeError ()
validateSpeechIdentifier name value =
    when
        (T.null (T.strip value))
        (Left (LlmExpectationError (toString name <> " must not be empty")))

data ElevenLabsOutputFormat
    = ElevenLabsMp3_22050_32
    | ElevenLabsMp3_24000_48
    | ElevenLabsMp3_44100_32
    | ElevenLabsMp3_44100_64
    | ElevenLabsMp3_44100_96
    | ElevenLabsMp3_44100_128
    | ElevenLabsMp3_44100_192
    | ElevenLabsPcm_8000
    | ElevenLabsPcm_16000
    | ElevenLabsPcm_22050
    | ElevenLabsPcm_24000
    | ElevenLabsPcm_32000
    | ElevenLabsPcm_44100
    | ElevenLabsPcm_48000
    | ElevenLabsWav_8000
    | ElevenLabsWav_16000
    | ElevenLabsWav_22050
    | ElevenLabsWav_24000
    | ElevenLabsWav_32000
    | ElevenLabsWav_44100
    | ElevenLabsWav_48000
    | ElevenLabsUlaw_8000
    | ElevenLabsAlaw_8000
    | ElevenLabsOpus_48000_32
    | ElevenLabsOpus_48000_64
    | ElevenLabsOpus_48000_96
    | ElevenLabsOpus_48000_128
    | ElevenLabsOpus_48000_192
    deriving stock (Show, Eq, Enum, Bounded, Generic)

formatJSONOptions :: Options
formatJSONOptions = defaultOptions{constructorTagModifier = formatConstructorTag}

formatConstructorTag :: String -> String
formatConstructorTag = fmap Char.toLower . drop (length ("ElevenLabs" :: String))

instance ToJSON ElevenLabsOutputFormat where
    toJSON = genericToJSON formatJSONOptions

instance FromJSON ElevenLabsOutputFormat where
    parseJSON = genericParseJSON formatJSONOptions

renderElevenLabsOutputFormat :: ElevenLabsOutputFormat -> Text
renderElevenLabsOutputFormat = toText . formatConstructorTag . show

parseElevenLabsOutputFormat :: Text -> Either Text ElevenLabsOutputFormat
parseElevenLabsOutputFormat rawFormat =
    maybe
        (Left ("Unknown ElevenLabs output format: " <> rawFormat))
        Right
        ( find
            ((== rawFormat) . renderElevenLabsOutputFormat)
            ([minBound .. maxBound] :: [ElevenLabsOutputFormat])
        )

elevenLabsOutputMimeType :: ElevenLabsOutputFormat -> Text
elevenLabsOutputMimeType = \case
    ElevenLabsMp3_22050_32 -> "audio/mpeg"
    ElevenLabsMp3_24000_48 -> "audio/mpeg"
    ElevenLabsMp3_44100_32 -> "audio/mpeg"
    ElevenLabsMp3_44100_64 -> "audio/mpeg"
    ElevenLabsMp3_44100_96 -> "audio/mpeg"
    ElevenLabsMp3_44100_128 -> "audio/mpeg"
    ElevenLabsMp3_44100_192 -> "audio/mpeg"
    ElevenLabsPcm_8000 -> "audio/pcm"
    ElevenLabsPcm_16000 -> "audio/pcm"
    ElevenLabsPcm_22050 -> "audio/pcm"
    ElevenLabsPcm_24000 -> "audio/pcm"
    ElevenLabsPcm_32000 -> "audio/pcm"
    ElevenLabsPcm_44100 -> "audio/pcm"
    ElevenLabsPcm_48000 -> "audio/pcm"
    ElevenLabsWav_8000 -> "audio/wav"
    ElevenLabsWav_16000 -> "audio/wav"
    ElevenLabsWav_22050 -> "audio/wav"
    ElevenLabsWav_24000 -> "audio/wav"
    ElevenLabsWav_32000 -> "audio/wav"
    ElevenLabsWav_44100 -> "audio/wav"
    ElevenLabsWav_48000 -> "audio/wav"
    ElevenLabsUlaw_8000 -> "audio/basic"
    ElevenLabsAlaw_8000 -> "application/octet-stream"
    ElevenLabsOpus_48000_32 -> "audio/ogg"
    ElevenLabsOpus_48000_64 -> "audio/ogg"
    ElevenLabsOpus_48000_96 -> "audio/ogg"
    ElevenLabsOpus_48000_128 -> "audio/ogg"
    ElevenLabsOpus_48000_192 -> "audio/ogg"
