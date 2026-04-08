module GenSpeechCLI
    ( GenSpeechProvider (..)
    , GenSpeechHelpTopic (..)
    , CommonGenSpeechOptions (..)
    , OpenAIGenSpeechOptions (..)
    , XAISpeechCodec (..)
    , XAIGenSpeechOptions (..)
    , GenSpeechOptions (..)
    , ParseGenSpeechArgsResult (..)
    , parseGenSpeechArgs
    , renderGenSpeechHelp
    , slugifySpeechText
    , suggestedSpeechExtension
    , runGenSpeechCli
    ) where

import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import GenCliSupport
import Rake
import Rake.Providers.OpenAI.TTS
import Rake.Providers.XAI.TTS
import Relude hiding (exitFailure, getArgs, lookupEnv)
import System.Directory
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO qualified as IO
import System.Process (readProcessWithExitCode)

data GenSpeechProvider
    = GenSpeechProviderOpenAI
    | GenSpeechProviderXAI
    deriving stock (Show, Eq)

data GenSpeechHelpTopic
    = GenSpeechHelpGeneral
    | GenSpeechHelpOpenAI
    | GenSpeechHelpXAI
    deriving stock (Show, Eq)

data CommonGenSpeechOptions = CommonGenSpeechOptions
    { commonSpeechText :: Text
    , commonSpeechOutputPath :: Maybe FilePath
    }
    deriving stock (Show, Eq)

data OpenAIGenSpeechOptions = OpenAIGenSpeechOptions
    { openAISpeechCommonOptions :: CommonGenSpeechOptions
    , openAISpeechModel :: OpenAITTSModel
    , openAISpeechVoice :: OpenAIVoice
    , openAISpeechInstructions :: Maybe Text
    , openAISpeechFormat :: Maybe OpenAIAudioFormat
    , openAISpeechSpeed :: Maybe Double
    }
    deriving stock (Show, Eq)

data XAISpeechCodec
    = XAISpeechCodecMp3
    | XAISpeechCodecWav
    | XAISpeechCodecPcm
    | XAISpeechCodecMuLaw
    | XAISpeechCodecALaw
    deriving stock (Show, Eq)

data XAIGenSpeechOptions = XAIGenSpeechOptions
    { xaiSpeechCommonOptions :: CommonGenSpeechOptions
    , xaiSpeechVoice :: XAITTSVoice
    , xaiSpeechLanguage :: XAITTSLanguage
    , xaiSpeechCodec :: Maybe XAISpeechCodec
    , xaiSpeechSampleRate :: Maybe XAISampleRate
    , xaiSpeechBitRate :: Maybe XAIMP3BitRate
    }
    deriving stock (Show, Eq)

data GenSpeechOptions
    = GenSpeechOpenAI OpenAIGenSpeechOptions
    | GenSpeechXAI XAIGenSpeechOptions
    deriving stock (Show, Eq)

data ParseGenSpeechArgsResult
    = ParseGenSpeechArgsHelp GenSpeechHelpTopic
    | ParseGenSpeechArgsError Text GenSpeechHelpTopic
    | ParseGenSpeechArgsSuccess GenSpeechOptions
    deriving stock (Show, Eq)

data GenSpeechRunResult
    = GenSpeechPlayed
    | GenSpeechSaved FilePath

data CommonParseState = CommonParseState
    { parseOutputPath :: Maybe FilePath
    , parseTextParts :: [Text]
    }

defaultCommonParseState :: CommonParseState
defaultCommonParseState =
    CommonParseState
        { parseOutputPath = Nothing
        , parseTextParts = []
        }

defaultOpenAIGenSpeechOptions :: OpenAIGenSpeechOptions
defaultOpenAIGenSpeechOptions =
    OpenAIGenSpeechOptions
        { openAISpeechCommonOptions =
            CommonGenSpeechOptions
                { commonSpeechText = ""
                , commonSpeechOutputPath = Nothing
                }
        , openAISpeechModel = OpenAITTSModelGPT4OMiniTTS
        , openAISpeechVoice = OpenAIVoiceAlloy
        , openAISpeechInstructions = Nothing
        , openAISpeechFormat = Nothing
        , openAISpeechSpeed = Nothing
        }

defaultXAIGenSpeechOptions :: XAIGenSpeechOptions
defaultXAIGenSpeechOptions =
    XAIGenSpeechOptions
        { xaiSpeechCommonOptions =
            CommonGenSpeechOptions
                { commonSpeechText = ""
                , commonSpeechOutputPath = Nothing
                }
        , xaiSpeechVoice = XAITTSVoiceEve
        , xaiSpeechLanguage = XAITTSLanguageAuto
        , xaiSpeechCodec = Nothing
        , xaiSpeechSampleRate = Nothing
        , xaiSpeechBitRate = Nothing
        }

runGenSpeechCli :: IO ()
runGenSpeechCli = do
    progName <- getProgName
    args <- getArgs
    case parseGenSpeechArgs (toText <$> args) of
        ParseGenSpeechArgsHelp helpTopic ->
            putTextLn (renderGenSpeechHelp progName helpTopic)
        ParseGenSpeechArgsError err helpTopic -> do
            IO.hPutStrLn IO.stderr (toString err)
            putTextLn (renderGenSpeechHelp progName helpTopic)
            exitFailure
        ParseGenSpeechArgsSuccess options -> do
            result <- runGenSpeech options
            case result of
                Left err -> do
                    IO.hPutStrLn IO.stderr (toString err)
                    exitFailure
                Right GenSpeechPlayed ->
                    putTextLn "Played audio."
                Right (GenSpeechSaved savedPath) ->
                    putTextLn ("Saved audio: " <> toText savedPath)

runGenSpeech :: GenSpeechOptions -> IO (Either Text GenSpeechRunResult)
runGenSpeech options = case options of
    GenSpeechOpenAI openAIOptions@OpenAIGenSpeechOptions
        { openAISpeechCommonOptions = CommonGenSpeechOptions{commonSpeechText, commonSpeechOutputPath}
        } -> do
            maybeApiKey <- lookupEnv "OPENAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "OPENAI_API_KEY is required for openai speech generation")
                Just apiKey -> do
                    let settings = buildOpenAISettings (toText apiKey) openAIOptions
                    responseResult <- runProvider (tts (TTSOpenAI settings) commonSpeechText)
                    handleSpeechResponse commonSpeechOutputPath commonSpeechText (GenSpeechOpenAI openAIOptions) responseResult
    GenSpeechXAI xaiOptions@XAIGenSpeechOptions
        { xaiSpeechCommonOptions = CommonGenSpeechOptions{commonSpeechText, commonSpeechOutputPath}
        } -> do
            maybeApiKey <- lookupEnv "XAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "XAI_API_KEY is required for xai speech generation")
                Just apiKey -> do
                    let settings = buildXAISettings (toText apiKey) xaiOptions
                    responseResult <- runProvider (tts (TTSXAI settings) commonSpeechText)
                    handleSpeechResponse commonSpeechOutputPath commonSpeechText (GenSpeechXAI xaiOptions) responseResult
  where
    runProvider
        :: Eff '[Error RakeError, IOE] Audio
        -> IO (Either Text Audio)
    runProvider action = do
        result <-
            runEff
                . runErrorNoCallStack
                $ action
        pure (first renderRakeError result)

    handleSpeechResponse maybeOutput speechText genSpeechOptions responseResult =
        case responseResult of
            Left err ->
                pure (Left err)
            Right generatedAudio ->
                case maybeOutput of
                    Just outputPath ->
                        saveGeneratedSpeech (Just outputPath) speechText genSpeechOptions generatedAudio
                            <&> fmap GenSpeechSaved
                    Nothing ->
                        playGeneratedSpeech speechText genSpeechOptions generatedAudio
                            <&> fmap (const GenSpeechPlayed)

buildOpenAISettings :: Text -> OpenAIGenSpeechOptions -> OpenAITTSSettings '[Error RakeError, IOE]
buildOpenAISettings resolvedApiKey OpenAIGenSpeechOptions
    { openAISpeechModel
    , openAISpeechVoice
    , openAISpeechInstructions
    , openAISpeechFormat
    , openAISpeechSpeed
    , openAISpeechCommonOptions = _
    } =
        case defaultOpenAITTSSettings resolvedApiKey of
            OpenAITTSSettings
                { apiKey
                , baseUrl
                , organizationId
                , projectId
                , requestLogger
                , options = _
                } ->
                    OpenAITTSSettings
                        { apiKey
                        , baseUrl
                        , organizationId
                        , projectId
                        , options =
                            defaultOpenAITTSOptions
                                { model = openAISpeechModel
                                , voice = openAISpeechVoice
                                , instructions = openAISpeechInstructions
                                , responseFormat = openAISpeechFormat
                                , speed = openAISpeechSpeed
                                }
                        , requestLogger
                        }

buildXAISettings :: Text -> XAIGenSpeechOptions -> XAITTSSettings '[Error RakeError, IOE]
buildXAISettings resolvedApiKey xaiOptions@XAIGenSpeechOptions
    { xaiSpeechVoice
    , xaiSpeechLanguage
    , xaiSpeechCommonOptions = _
    , xaiSpeechCodec = _
    , xaiSpeechSampleRate = _
    , xaiSpeechBitRate = _
    } =
        case defaultXAITTSSettings resolvedApiKey of
            XAITTSSettings
                { apiKey
                , baseUrl
                , requestLogger
                , options = _
                } ->
                    XAITTSSettings
                        { apiKey
                        , baseUrl
                        , options =
                            defaultXAITTSOptions
                                { voice = xaiSpeechVoice
                                , language = xaiSpeechLanguage
                                , outputFormat = xaiOutputFormat xaiOptions
                                }
                        , requestLogger
                        }

parseGenSpeechArgs :: [Text] -> ParseGenSpeechArgsResult
parseGenSpeechArgs = \case
    [] ->
        ParseGenSpeechArgsError "A provider is required. Use `openai` or `xai`." GenSpeechHelpGeneral
    "--help" : _ ->
        ParseGenSpeechArgsHelp GenSpeechHelpGeneral
    providerArg : rest ->
        case parseProvider providerArg of
            Nothing ->
                ParseGenSpeechArgsError
                    ("Unknown provider: " <> providerArg <> ". Use `openai` or `xai`.")
                    GenSpeechHelpGeneral
            Just GenSpeechProviderOpenAI ->
                parseOpenAIArgs rest
            Just GenSpeechProviderXAI ->
                parseXAIArgs rest

parseProvider :: Text -> Maybe GenSpeechProvider
parseProvider = \case
    "openai" ->
        Just GenSpeechProviderOpenAI
    "xai" ->
        Just GenSpeechProviderXAI
    _ ->
        Nothing

parseOpenAIArgs :: [Text] -> ParseGenSpeechArgsResult
parseOpenAIArgs =
    go defaultCommonParseState defaultOpenAIGenSpeechOptions
  where
    go commonState openAIOptions = \case
        [] ->
            finalize commonState openAIOptions
        "--help" : _ ->
            ParseGenSpeechArgsHelp GenSpeechHelpOpenAI
        "--" : rest ->
            finalize (appendTextParts commonState rest) openAIOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} openAIOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} openAIOptions rest
        "--model" : rawModel : rest ->
            case parseOpenAIModel rawModel of
                Left err ->
                    ParseGenSpeechArgsError err GenSpeechHelpOpenAI
                Right modelName ->
                    go commonState openAIOptions{openAISpeechModel = modelName} rest
        "--voice" : rawVoice : rest ->
            go commonState openAIOptions{openAISpeechVoice = parseOpenAIVoice rawVoice} rest
        "--instructions" : instructionsText : rest ->
            go commonState openAIOptions{openAISpeechInstructions = Just instructionsText} rest
        "--format" : rawFormat : rest ->
            case parseOpenAIAudioFormat rawFormat of
                Left err ->
                    ParseGenSpeechArgsError err GenSpeechHelpOpenAI
                Right responseFormat ->
                    go commonState openAIOptions{openAISpeechFormat = Just responseFormat} rest
        "--speed" : rawSpeed : rest ->
            case parsePositiveDoubleOption "--speed" rawSpeed of
                Left err ->
                    ParseGenSpeechArgsError err GenSpeechHelpOpenAI
                Right speed ->
                    go commonState openAIOptions{openAISpeechSpeed = Just speed} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} openAIOptions rest
            | Just rawModel <- T.stripPrefix "--model=" arg ->
                case parseOpenAIModel rawModel of
                    Left err ->
                        ParseGenSpeechArgsError err GenSpeechHelpOpenAI
                    Right modelName ->
                        go commonState openAIOptions{openAISpeechModel = modelName} rest
            | Just rawVoice <- T.stripPrefix "--voice=" arg ->
                go commonState openAIOptions{openAISpeechVoice = parseOpenAIVoice rawVoice} rest
            | Just instructionsText <- T.stripPrefix "--instructions=" arg ->
                go commonState openAIOptions{openAISpeechInstructions = Just instructionsText} rest
            | Just rawFormat <- T.stripPrefix "--format=" arg ->
                case parseOpenAIAudioFormat rawFormat of
                    Left err ->
                        ParseGenSpeechArgsError err GenSpeechHelpOpenAI
                    Right responseFormat ->
                        go commonState openAIOptions{openAISpeechFormat = Just responseFormat} rest
            | Just rawSpeed <- T.stripPrefix "--speed=" arg ->
                case parsePositiveDoubleOption "--speed" rawSpeed of
                    Left err ->
                        ParseGenSpeechArgsError err GenSpeechHelpOpenAI
                    Right speed ->
                        go commonState openAIOptions{openAISpeechSpeed = Just speed} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenSpeechArgsError ("Unknown openai option: " <> arg) GenSpeechHelpOpenAI
            | otherwise ->
                go (appendTextParts commonState [arg]) openAIOptions rest

    finalize CommonParseState{parseOutputPath, parseTextParts} openAIOptions
        | null parseTextParts =
            ParseGenSpeechArgsError "Input text is required." GenSpeechHelpOpenAI
        | otherwise =
            ParseGenSpeechArgsSuccess
                ( GenSpeechOpenAI
                    openAIOptions
                        { openAISpeechCommonOptions =
                            CommonGenSpeechOptions
                                { commonSpeechText = T.unwords parseTextParts
                                , commonSpeechOutputPath = parseOutputPath
                                }
                        }
                )

parseXAIArgs :: [Text] -> ParseGenSpeechArgsResult
parseXAIArgs =
    go defaultCommonParseState defaultXAIGenSpeechOptions
  where
    go commonState xaiOptions = \case
        [] ->
            finalize commonState xaiOptions
        "--help" : _ ->
            ParseGenSpeechArgsHelp GenSpeechHelpXAI
        "--" : rest ->
            finalize (appendTextParts commonState rest) xaiOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
        "--voice" : rawVoice : rest ->
            go commonState xaiOptions{xaiSpeechVoice = parseXAIVoice rawVoice} rest
        "--language" : rawLanguage : rest ->
            go commonState xaiOptions{xaiSpeechLanguage = parseXAILanguage rawLanguage} rest
        "--codec" : rawCodec : rest ->
            case parseXAISpeechCodec rawCodec of
                Left err ->
                    ParseGenSpeechArgsError err GenSpeechHelpXAI
                Right codec ->
                    go commonState xaiOptions{xaiSpeechCodec = Just codec} rest
        "--sample-rate" : rawSampleRate : rest ->
            case parseXAISampleRateOption rawSampleRate of
                Left err ->
                    ParseGenSpeechArgsError err GenSpeechHelpXAI
                Right sampleRate ->
                    go commonState xaiOptions{xaiSpeechSampleRate = Just sampleRate} rest
        "--bit-rate" : rawBitRate : rest ->
            case parseXAIMP3BitRateOption rawBitRate of
                Left err ->
                    ParseGenSpeechArgsError err GenSpeechHelpXAI
                Right bitRate ->
                    go commonState xaiOptions{xaiSpeechBitRate = Just bitRate} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
            | Just rawVoice <- T.stripPrefix "--voice=" arg ->
                go commonState xaiOptions{xaiSpeechVoice = parseXAIVoice rawVoice} rest
            | Just rawLanguage <- T.stripPrefix "--language=" arg ->
                go commonState xaiOptions{xaiSpeechLanguage = parseXAILanguage rawLanguage} rest
            | Just rawCodec <- T.stripPrefix "--codec=" arg ->
                case parseXAISpeechCodec rawCodec of
                    Left err ->
                        ParseGenSpeechArgsError err GenSpeechHelpXAI
                    Right codec ->
                        go commonState xaiOptions{xaiSpeechCodec = Just codec} rest
            | Just rawSampleRate <- T.stripPrefix "--sample-rate=" arg ->
                case parseXAISampleRateOption rawSampleRate of
                    Left err ->
                        ParseGenSpeechArgsError err GenSpeechHelpXAI
                    Right sampleRate ->
                        go commonState xaiOptions{xaiSpeechSampleRate = Just sampleRate} rest
            | Just rawBitRate <- T.stripPrefix "--bit-rate=" arg ->
                case parseXAIMP3BitRateOption rawBitRate of
                    Left err ->
                        ParseGenSpeechArgsError err GenSpeechHelpXAI
                    Right bitRate ->
                        go commonState xaiOptions{xaiSpeechBitRate = Just bitRate} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenSpeechArgsError ("Unknown xai option: " <> arg) GenSpeechHelpXAI
            | otherwise ->
                go (appendTextParts commonState [arg]) xaiOptions rest

    finalize CommonParseState{parseOutputPath, parseTextParts} xaiOptions@XAIGenSpeechOptions{xaiSpeechCodec, xaiSpeechSampleRate, xaiSpeechBitRate}
        | null parseTextParts =
            ParseGenSpeechArgsError "Input text is required." GenSpeechHelpXAI
        | isNothing xaiSpeechCodec && isJust xaiSpeechSampleRate =
            ParseGenSpeechArgsError "Use --codec when setting --sample-rate." GenSpeechHelpXAI
        | isNothing xaiSpeechCodec && isJust xaiSpeechBitRate =
            ParseGenSpeechArgsError "Use --codec when setting --bit-rate." GenSpeechHelpXAI
        | xaiSpeechCodec /= Just XAISpeechCodecMp3 && isJust xaiSpeechBitRate =
            ParseGenSpeechArgsError "--bit-rate requires --codec=mp3." GenSpeechHelpXAI
        | otherwise =
            ParseGenSpeechArgsSuccess
                ( GenSpeechXAI
                    xaiOptions
                        { xaiSpeechCommonOptions =
                            CommonGenSpeechOptions
                                { commonSpeechText = T.unwords parseTextParts
                                , commonSpeechOutputPath = parseOutputPath
                                }
                        }
                )

parseOpenAIModel :: Text -> Either Text OpenAITTSModel
parseOpenAIModel = \case
    "tts-1" ->
        Right OpenAITTSModelTTS1
    "tts-1-hd" ->
        Right OpenAITTSModelTTS1HD
    "gpt-4o-mini-tts" ->
        Right OpenAITTSModelGPT4OMiniTTS
    rawValue ->
        Left
            ( "Unknown value for --model: "
                <> rawValue
                <> ". Supported values: tts-1, tts-1-hd, gpt-4o-mini-tts."
            )

parseOpenAIVoice :: Text -> OpenAIVoice
parseOpenAIVoice = \case
    "alloy" ->
        OpenAIVoiceAlloy
    "ash" ->
        OpenAIVoiceAsh
    "ballad" ->
        OpenAIVoiceBallad
    "coral" ->
        OpenAIVoiceCoral
    "echo" ->
        OpenAIVoiceEcho
    "fable" ->
        OpenAIVoiceFable
    "onyx" ->
        OpenAIVoiceOnyx
    "nova" ->
        OpenAIVoiceNova
    "sage" ->
        OpenAIVoiceSage
    "shimmer" ->
        OpenAIVoiceShimmer
    "verse" ->
        OpenAIVoiceVerse
    voiceId ->
        OpenAIVoiceId voiceId

parseOpenAIAudioFormat :: Text -> Either Text OpenAIAudioFormat
parseOpenAIAudioFormat = \case
    "mp3" ->
        Right OpenAIAudioFormatMp3
    "opus" ->
        Right OpenAIAudioFormatOpus
    "aac" ->
        Right OpenAIAudioFormatAac
    "flac" ->
        Right OpenAIAudioFormatFlac
    "wav" ->
        Right OpenAIAudioFormatWav
    "pcm" ->
        Right OpenAIAudioFormatPcm
    rawValue ->
        Left
            ( "Unknown value for --format: "
                <> rawValue
                <> ". Supported values: mp3, opus, aac, flac, wav, pcm."
            )

parsePositiveDoubleOption :: Text -> Text -> Either Text Double
parsePositiveDoubleOption optionName rawValue =
    case readMaybe @Double (toString rawValue) of
        Nothing ->
            Left ("Invalid value for " <> optionName <> ": " <> rawValue)
        Just value
            | value <= 0 ->
                Left (optionName <> " must be greater than 0")
            | otherwise ->
                Right value

parseXAIVoice :: Text -> XAITTSVoice
parseXAIVoice = \case
    "ara" ->
        XAITTSVoiceAra
    "eve" ->
        XAITTSVoiceEve
    "leo" ->
        XAITTSVoiceLeo
    "rex" ->
        XAITTSVoiceRex
    "sal" ->
        XAITTSVoiceSal
    voiceId ->
        XAITTSVoiceId voiceId

parseXAILanguage :: Text -> XAITTSLanguage
parseXAILanguage = \case
    "auto" ->
        XAITTSLanguageAuto
    "en" ->
        XAITTSLanguageEnglish
    "ar-EG" ->
        XAITTSLanguageArabicEgypt
    "ar-SA" ->
        XAITTSLanguageArabicSaudiArabia
    "ar-AE" ->
        XAITTSLanguageArabicUnitedArabEmirates
    "bn" ->
        XAITTSLanguageBengali
    "zh" ->
        XAITTSLanguageChineseSimplified
    "fr" ->
        XAITTSLanguageFrench
    "de" ->
        XAITTSLanguageGerman
    "hi" ->
        XAITTSLanguageHindi
    "id" ->
        XAITTSLanguageIndonesian
    "it" ->
        XAITTSLanguageItalian
    "ja" ->
        XAITTSLanguageJapanese
    "ko" ->
        XAITTSLanguageKorean
    "pt-BR" ->
        XAITTSLanguagePortugueseBrazil
    "pt-PT" ->
        XAITTSLanguagePortuguesePortugal
    "ru" ->
        XAITTSLanguageRussian
    "es-MX" ->
        XAITTSLanguageSpanishMexico
    "es-ES" ->
        XAITTSLanguageSpanishSpain
    "tr" ->
        XAITTSLanguageTurkish
    "vi" ->
        XAITTSLanguageVietnamese
    languageCode ->
        XAITTSLanguageCode languageCode

parseXAISpeechCodec :: Text -> Either Text XAISpeechCodec
parseXAISpeechCodec = \case
    "mp3" ->
        Right XAISpeechCodecMp3
    "wav" ->
        Right XAISpeechCodecWav
    "pcm" ->
        Right XAISpeechCodecPcm
    "mulaw" ->
        Right XAISpeechCodecMuLaw
    "alaw" ->
        Right XAISpeechCodecALaw
    rawValue ->
        Left
            ( "Unknown value for --codec: "
                <> rawValue
                <> ". Supported values: mp3, wav, pcm, mulaw, alaw."
            )

parseXAISampleRateOption :: Text -> Either Text XAISampleRate
parseXAISampleRateOption rawValue =
    case readMaybe @Int (toString rawValue) of
        Nothing ->
            Left ("Invalid value for --sample-rate: " <> rawValue)
        Just sampleRate ->
            case sampleRate of
                8000 ->
                    Right XAISampleRate8000
                16000 ->
                    Right XAISampleRate16000
                22050 ->
                    Right XAISampleRate22050
                24000 ->
                    Right XAISampleRate24000
                44100 ->
                    Right XAISampleRate44100
                48000 ->
                    Right XAISampleRate48000
                _ ->
                    Left "Unsupported value for --sample-rate. Supported values: 8000, 16000, 22050, 24000, 44100, 48000."

parseXAIMP3BitRateOption :: Text -> Either Text XAIMP3BitRate
parseXAIMP3BitRateOption rawValue =
    case readMaybe @Int (toString rawValue) of
        Nothing ->
            Left ("Invalid value for --bit-rate: " <> rawValue)
        Just bitRate ->
            case bitRate of
                32000 ->
                    Right XAIMP3BitRate32000
                64000 ->
                    Right XAIMP3BitRate64000
                96000 ->
                    Right XAIMP3BitRate96000
                128000 ->
                    Right XAIMP3BitRate128000
                192000 ->
                    Right XAIMP3BitRate192000
                _ ->
                    Left "Unsupported value for --bit-rate. Supported values: 32000, 64000, 96000, 128000, 192000."

appendTextParts :: CommonParseState -> [Text] -> CommonParseState
appendTextParts commonState@CommonParseState{parseTextParts} extraTextParts =
    commonState{parseTextParts = parseTextParts <> extraTextParts}

renderGenSpeechHelp :: String -> GenSpeechHelpTopic -> Text
renderGenSpeechHelp progName = \case
    GenSpeechHelpGeneral ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " openai [OPTIONS] TEXT"
            , "  " <> toText progName <> " xai [OPTIONS] TEXT"
            , ""
            , "Providers:"
            , "  openai  Generate speech with OpenAI. Default model: gpt-4o-mini-tts"
            , "  xai     Generate speech with xAI TTS. Default voice: eve"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "openai options:"
                   ]
                <> openAIOptionLines
                <> [ ""
                   , "xai options:"
                   ]
                <> xaiOptionLines
                <> [ ""
                   , "Notes:"
                   , "  TEXT may be quoted or split across multiple arguments."
                   , "  With no `--output`, the CLI plays the audio locally."
                   , "  Use `" <> toText progName <> " openai --help` or `" <> toText progName <> " xai --help` for focused help."
                   , "  `--sample-rate` and `--bit-rate` require `--codec`."
                   , "  `--bit-rate` is only valid with `--codec=mp3`."
                   , "  Local playback looks for `ffplay`, `mpv`, or `afplay`."
                   , "  OPENAI_API_KEY is required for `openai`."
                   , "  XAI_API_KEY is required for `xai`."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " openai \"Hello from ai-rake.\""
                   , "  " <> toText progName <> " openai --voice=verse --format=wav -o hello.wav \"A clean narration test\""
                   , "  " <> toText progName <> " xai --voice=rex --language=en \"Hello from ai-rake.\""
                   , "  " <> toText progName <> " xai --codec=wav --sample-rate=24000 -o update.wav \"A short status update\""
                   ]
    GenSpeechHelpOpenAI ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " openai [OPTIONS] TEXT"
            , ""
            , "Default model: gpt-4o-mini-tts"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "openai options:"
                   ]
                <> openAIOptionLines
                <> [ ""
                   , "Notes:"
                   , "  TEXT may be quoted or split across multiple arguments."
                   , "  With no `--output`, the CLI plays the audio locally."
                   , "  Local playback looks for `ffplay`, `mpv`, or `afplay`."
                   , "  OPENAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " openai \"Hello from ai-rake.\""
                   , "  " <> toText progName <> " openai --model=tts-1-hd --voice=shimmer -o announcement.mp3 \"A product announcement\""
                   , "  " <> toText progName <> " openai --instructions=\"Speak slowly\" --format=flac -o report.flac \"Status report follows\""
                   ]
    GenSpeechHelpXAI ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " xai [OPTIONS] TEXT"
            , ""
            , "Default voice: eve"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "xai options:"
                   ]
                <> xaiOptionLines
                <> [ ""
                   , "Notes:"
                   , "  TEXT may be quoted or split across multiple arguments."
                   , "  With no `--output`, the CLI plays the audio locally."
                   , "  `--sample-rate` and `--bit-rate` require `--codec`."
                   , "  `--bit-rate` is only valid with `--codec=mp3`."
                   , "  Local playback looks for `ffplay`, `mpv`, or `afplay`."
                   , "  XAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " xai --voice=eve --language=auto \"Hello from ai-rake.\""
                   , "  " <> toText progName <> " xai --codec=mp3 --bit-rate=128000 -o narration.mp3 \"A polished narration\""
                   , "  " <> toText progName <> " xai --codec=pcm --sample-rate=16000 -o prompt.pcm \"A phone-friendly prompt\""
                   ]
  where
    commonOptionLines =
        [ "  -o, --output PATH            Save audio to PATH instead of playing it."
        , "  --help                       Show help."
        ]

    openAIOptionLines =
        [ "  --model MODEL                One of: tts-1, tts-1-hd, gpt-4o-mini-tts."
        , "  --voice VOICE                Built-in voice name or custom OpenAI voice id."
        , "  --instructions TEXT          Optional speaking instructions."
        , "  --format FORMAT              One of: mp3, opus, aac, flac, wav, pcm."
        , "  --speed N                    Positive playback speed multiplier."
        ]

    xaiOptionLines =
        [ "  --voice VOICE                Built-in voice name or custom xAI voice id."
        , "  --language LANGUAGE          Known code like en or pt-BR, or a custom language code."
        , "  --codec CODEC                One of: mp3, wav, pcm, mulaw, alaw."
        , "  --sample-rate HZ             One of: 8000, 16000, 22050, 24000, 44100, 48000."
        , "  --bit-rate BPS               MP3 only: 32000, 64000, 96000, 128000, 192000."
        ]

slugifySpeechText :: Text -> FilePath
slugifySpeechText =
    slugifyPromptWithFallback "speech"

suggestedSpeechExtension :: GenSpeechOptions -> Audio -> String
suggestedSpeechExtension genSpeechOptions Audio{mimeType, fileName} =
    fromMaybe ".mp3" $
        (fileName >>= urlExtension)
            <|> (mimeType >>= audioExtensionFromMimeType)
            <|> explicitSpeechExtension genSpeechOptions

explicitSpeechExtension :: GenSpeechOptions -> Maybe String
explicitSpeechExtension = \case
    GenSpeechOpenAI OpenAIGenSpeechOptions{openAISpeechFormat} ->
        case openAISpeechFormat of
            Nothing ->
                Just ".mp3"
            Just responseFormat ->
                Just (openAIAudioFormatExtension responseFormat)
    GenSpeechXAI XAIGenSpeechOptions{xaiSpeechCodec} ->
        case xaiSpeechCodec of
            Nothing ->
                Just ".mp3"
            Just codec ->
                Just (xaiSpeechCodecExtension codec)

openAIAudioFormatExtension :: OpenAIAudioFormat -> String
openAIAudioFormatExtension = \case
    OpenAIAudioFormatMp3 ->
        ".mp3"
    OpenAIAudioFormatOpus ->
        ".opus"
    OpenAIAudioFormatAac ->
        ".aac"
    OpenAIAudioFormatFlac ->
        ".flac"
    OpenAIAudioFormatWav ->
        ".wav"
    OpenAIAudioFormatPcm ->
        ".pcm"

xaiSpeechCodecExtension :: XAISpeechCodec -> String
xaiSpeechCodecExtension = \case
    XAISpeechCodecMp3 ->
        ".mp3"
    XAISpeechCodecWav ->
        ".wav"
    XAISpeechCodecPcm ->
        ".pcm"
    XAISpeechCodecMuLaw ->
        ".mulaw"
    XAISpeechCodecALaw ->
        ".alaw"

xaiOutputFormat :: XAIGenSpeechOptions -> Maybe XAIOutputFormat
xaiOutputFormat XAIGenSpeechOptions{xaiSpeechCodec, xaiSpeechSampleRate, xaiSpeechBitRate} =
    case xaiSpeechCodec of
        Nothing ->
            Nothing
        Just XAISpeechCodecMp3 ->
            Just
                XAIOutputFormatMp3
                    { sampleRate = xaiSpeechSampleRate
                    , bitRate = xaiSpeechBitRate
                    }
        Just XAISpeechCodecWav ->
            Just
                XAIOutputFormatWav
                    { sampleRate = xaiSpeechSampleRate
                    }
        Just XAISpeechCodecPcm ->
            Just
                XAIOutputFormatPcm
                    { sampleRate = xaiSpeechSampleRate
                    }
        Just XAISpeechCodecMuLaw ->
            Just
                XAIOutputFormatMuLaw
                    { sampleRate = xaiSpeechSampleRate
                    }
        Just XAISpeechCodecALaw ->
            Just
                XAIOutputFormatALaw
                    { sampleRate = xaiSpeechSampleRate
                    }

saveGeneratedSpeech :: Maybe FilePath -> Text -> GenSpeechOptions -> Audio -> IO (Either Text FilePath)
saveGeneratedSpeech maybeOutput speechText genSpeechOptions generatedAudio@Audio{audioBytes} = do
    outputPaths <-
        buildOutputPaths
            "speech"
            maybeOutput
            speechText
            [suggestedSpeechExtension genSpeechOptions generatedAudio]
    case outputPaths of
        [outputPath] -> do
            writeBinaryFile outputPath audioBytes
            pure (Right outputPath)
        _ ->
            pure (Left "Expected exactly one output path for audio extension.")

playGeneratedSpeech :: Text -> GenSpeechOptions -> Audio -> IO (Either Text ())
playGeneratedSpeech speechText genSpeechOptions generatedAudio = do
    tempDir <- getTemporaryDirectory
    let tempPathTemplate = tempDir </> "rake-tts-play"
    saveResult <- saveGeneratedSpeech (Just tempPathTemplate) speechText genSpeechOptions generatedAudio
    case saveResult of
        Left err ->
            pure (Left err)
        Right tempPath -> do
            playResult <- playSavedSpeech tempPath
            ignoreMissingFile tempPath
            pure playResult

playSavedSpeech :: FilePath -> IO (Either Text ())
playSavedSpeech savedPath = do
    maybePlayer <- findAvailablePlayer playbackCandidates
    case maybePlayer of
        Nothing ->
            pure
                ( Left
                    "No supported audio player was found. Install `ffplay`, `mpv`, or `afplay`, or pass `--output PATH` to save the audio instead."
                )
        Just (playerPath, buildArgs) ->
            runExternalCommand playerPath (buildArgs savedPath)

findAvailablePlayer :: [(FilePath, FilePath -> [String])] -> IO (Maybe (FilePath, FilePath -> [String]))
findAvailablePlayer candidates =
    go candidates
  where
    go = \case
        [] ->
            pure Nothing
        (playerName, buildArgs) : rest -> do
            maybeExecutable <- findExecutable playerName
            case maybeExecutable of
                Nothing ->
                    go rest
                Just executablePath ->
                    pure (Just (executablePath, buildArgs))

playbackCandidates :: [(FilePath, FilePath -> [String])]
playbackCandidates =
    [ ("ffplay", \audioPath -> ["-nodisp", "-autoexit", "-loglevel", "error", audioPath])
    , ("mpv", \audioPath -> ["--no-video", "--really-quiet", audioPath])
    , ("afplay", \audioPath -> [audioPath])
    ]

runExternalCommand :: FilePath -> [String] -> IO (Either Text ())
runExternalCommand executablePath args =
    runExternalCommandWithOutput executablePath args <&> void

runExternalCommandWithOutput :: FilePath -> [String] -> IO (Either Text Text)
runExternalCommandWithOutput executablePath args = do
    (exitCode, stdoutText, stderrText) <- readProcessWithExitCode executablePath args ""
    pure $
        case exitCode of
            ExitSuccess ->
                Right (T.strip (toText stdoutText))
            ExitFailure exitCodeValue ->
                Left
                    ( toText executablePath
                        <> " failed with exit code "
                        <> show exitCodeValue
                        <> renderProcessErrorDetails stdoutText stderrText
                    )

renderProcessErrorDetails :: String -> String -> Text
renderProcessErrorDetails stdoutText stderrText =
    case T.strip preferredDetail of
        "" ->
            ""
        detail ->
            ": " <> detail
  where
    preferredDetail
        | null stderrText =
            toText stdoutText
        | otherwise =
            toText stderrText

ignoreMissingFile :: FilePath -> IO ()
ignoreMissingFile path = do
    exists <- doesFileExist path
    when exists (removeFile path)
