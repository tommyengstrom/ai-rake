{-# LANGUAGE NoOverloadedLists #-}

module RakeTTSCLI
    ( parseGenSpeechArgs
    , renderGenSpeechHelp
    , runGenSpeechCli
    , speechModels
    ) where

import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Rake.Media
import Rake.Providers.OpenRouter.Settings
import Rake.Providers.OpenRouter.TTS
import RakeCliSupport
import RakeMediaOptions
import Relude
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process (readProcessWithExitCode)

speechModels :: [(Text, Object)]
speechModels = [(speechModelName defaultGrokVoiceTts10, speechModelConfig defaultGrokVoiceTts10)]

parseGenSpeechArgs :: [Text] -> Either Text MediaOptions
parseGenSpeechArgs args = do
    options@MediaOptions
        { selectedModel
        , modelSettings
        , rawSettings
        , imageSources
        , lastFrameSource
        , extendSource
        , outputPath
        } <-
        parseMediaOptions "x-ai/grok-voice-tts-1.0" flags args >>= finishOptions speechModels
    when (pollingSelected options)
        $ Left "Polling options are only supported for video generation"
    unless (null imageSources && isNothing lastFrameSource && isNothing extendSource)
        $ Left "Image/video options are not supported for speech"
    when (selectedModel == "x-ai/grok-voice-tts-1.0" && not rawSettings) do
        rejectSettings ["voice", "response_format"] modelSettings
        enumSetting "voice" (map toJSON [minBound @GrokVoice .. maxBound]) modelSettings
        enumSetting
            "response_format"
            (map toJSON [minBound @SpeechFormat .. maxBound])
            modelSettings
    when
        (KM.lookup "response_format" modelSettings == Just (String "pcm") && isNothing outputPath)
        $ Left "Raw PCM requires --output; playback needs model-specific audio parameters"
    pure options
  where
    flags :: [(Text, Text, Text -> Either Text Value)]
    flags =
        [ ("--voice", "voice", Right . String)
        , ("--instructions", "instructions", Right . String)
        , ("--format", "response_format", Right . String)
        , ("--speed", "speed", number)
        ]
    number :: Text -> Either Text Value
    number value = case readMaybe @Double (toString value) of
        Just speed | speed > 0 && not (isNaN speed || isInfinite speed) -> Right (toJSON speed)
        _ -> Left "--speed requires a positive finite number"

renderGenSpeechHelp :: Text
renderGenSpeechHelp =
    T.unlines
        [ "Usage: rake-tts [-m author/model] [OPTIONS] TEXT"
        , "Uses OPENROUTER_API_KEY. Default: x-ai/grok-voice-tts-1.0, voice eve, MP3."
        , "--voice VOICE  --format mp3|pcm  --instructions TEXT  --speed N"
        , "Support for instructions and speed depends on the selected model."
        , "--model-options JSON  Raw settings for any OpenRouter model."
        , "-o/--output PATH saves audio; otherwise ffplay, mpv or afplay plays it."
        , "Direct OpenAI speech remains available through airake-openai."
        ]

runGenSpeechCli :: IO ()
runGenSpeechCli = runMediaCli renderGenSpeechHelp parseGenSpeechArgs run
  where
    run :: Text -> MediaOptions -> IO (Either Text [FilePath])
    run apiKey MediaOptions{selectedModel, modelSettings, promptText, outputPath} = do
        result <-
            runProvider
                $ generateOpenRouterSpeech
                    (defaultOpenRouterSettings apiKey)
                    (CustomSpeechModel selectedModel modelSettings)
                    promptText
        case result of
            Left err -> pure (Left err)
            Right Audio{audioBytes, mimeType} -> do
                let extension :: String
                    extension =
                        fromMaybe
                            ( if KM.lookup "response_format" modelSettings == Just (String "pcm")
                                then ".pcm"
                                else ".mp3"
                            )
                            (mimeType >>= audioExtensionFromMimeType)
                case outputPath of
                    Just path -> Right <$> saveBytes "speech" (Just path) promptText [(extension, audioBytes)]
                    Nothing -> do
                        temp <- getTemporaryDirectory
                        bracket
                            (IO.openBinaryTempFile temp ("rake-speech" <> extension))
                            (\(path, handle) -> IO.hClose handle >> removeFile path)
                            ( \(path, handle) -> do
                                IO.hClose handle
                                writeBinaryFile path audioBytes
                                fmap (fmap (const [])) (playSavedSpeech path)
                            )

playSavedSpeech :: FilePath -> IO (Either Text ())
playSavedSpeech path =
    go
        [ ("ffplay", ["-nodisp", "-autoexit", "-loglevel", "error", path])
        , ("mpv", ["--no-video", "--really-quiet", path])
        , ("afplay", [path])
        ]
  where
    go :: [(FilePath, [String])] -> IO (Either Text ())
    go = \case
        [] -> pure (Left "Install ffplay, mpv or afplay, or use --output to save audio")
        (name, args) : rest ->
            findExecutable name >>= \case
                Nothing -> go rest
                Just executable -> do
                    (status, _, err) <- readProcessWithExitCode executable args ""
                    pure $ case status of
                        ExitSuccess -> Right ()
                        ExitFailure code -> Left ("Audio player failed (" <> show code <> "): " <> toText err)
