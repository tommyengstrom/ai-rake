{-# LANGUAGE NoOverloadedLists #-}

module RakeVideoCLI
    ( parseGenVideoArgs
    , renderGenVideoHelp
    , runGenVideoCli
    , videoModels
    ) where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Effectful (IOE)
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error, throwError)
import Rake.Error (RakeError (..), renderRakeError)
import Rake.Providers.OpenRouter.Settings
import Rake.Providers.OpenRouter.Videos
import RakeCliSupport
import RakeMediaOptions
import RakeVideoFiles
import Relude

videoModels :: [(Text, Object)]
videoModels =
    [ entry defaultGrokImagineVideo15
    , entry (GrokImagineVideo15Lite defaultGrokImagineVideo15)
    , entry defaultVeo31
    ]
  where
    entry :: OpenRouterVideoModel model => model -> (Text, Object)
    entry model = (videoModelName model, videoModelConfig model)

parseGenVideoArgs :: [Text] -> Either Text MediaOptions
parseGenVideoArgs args = do
    options@MediaOptions
        { selectedModel
        , modelSettings
        , rawSettings
        , imageSources
        , lastFrameSource
        , extendSource
        } <-
        parseMediaOptions "x-ai/grok-imagine-video-1.5" flags args >>= finishOptions videoModels
    when (length imageSources > 1) $ Left "Video generation accepts one --image first frame"
    when (isJust lastFrameSource && null imageSources) $ Left "--last-frame requires --image"
    when (isJust extendSource && (not (null imageSources) || isJust lastFrameSource))
        $ Left "--extend cannot be combined with --image or --last-frame"
    unless rawSettings $ validateKnown selectedModel modelSettings lastFrameSource
    void
        $ first renderRakeError
        $ renderVideoRequest
            (defaultOpenRouterSettings "")
            (CustomVideoModel selectedModel modelSettings)
            (VideoRequest (promptText options) (viaNonEmpty head imageSources) lastFrameSource)
    pure options
  where
    flags :: [(Text, Text, Text -> Either Text Value)]
    flags =
        [ ("--aspect-ratio", "aspect_ratio", Right . String)
        , ("--resolution", "resolution", Right . String)
        ]
            <> [(flag, key, integer) | (flag, key) <- [("--duration", "duration"), ("--seed", "seed")]]
            <> [("--generate-audio", "generate_audio", boolean)]
    integer :: Text -> Either Text Value
    integer value = maybe (Left "Expected an integer") (Right . toJSON) (readMaybe @Int (toString value))
    boolean :: Text -> Either Text Value
    boolean = \case
        "true" -> Right (Bool True)
        "false" -> Right (Bool False)
        _ -> Left "--generate-audio expects true or false"

validateKnown :: Text -> Object -> Maybe Text -> Either Text ()
validateKnown model settings lastFrame
    | model `elem` ["x-ai/grok-imagine-video-1.5", "x-ai/grok-imagine-video-1.5-lite"] = do
        rejectSettings ["duration", "resolution", "aspect_ratio"] settings
        enumSetting
            "aspect_ratio"
            (map String ["16:9", "9:16", "1:1", "4:3", "3:4", "3:2", "2:3"])
            settings
        numericSetting "duration" 1 15 settings
        enumSetting "resolution" (map String ["480p", "720p", "1080p"]) settings
        when (isJust lastFrame) $ Left "OpenRouter's Grok route does not support --last-frame"
    | model == "google/veo-3.1" = do
        rejectSettings
            ["duration", "resolution", "aspect_ratio", "generate_audio", "seed"]
            settings
        enumSetting "duration" (map toJSON ([4, 6, 8] :: [Int])) settings
        enumSetting "resolution" (map String ["720p", "1080p", "4K"]) settings
        enumSetting "aspect_ratio" (map String ["16:9", "9:16"]) settings
    | otherwise = Right ()

renderGenVideoHelp :: Text
renderGenVideoHelp =
    T.unlines
        $ [ "Usage: rake-video [-m author/model] [OPTIONS] PROMPT"
          , "Uses OPENROUTER_API_KEY. Default: x-ai/grok-imagine-video-1.5."
          , "--image SOURCE  --last-frame SOURCE  --duration SECONDS"
          , "--aspect-ratio RATIO  --resolution 480p|720p|1080p|4K"
          , "--generate-audio true|false  --seed N (model-dependent)"
          , "--extend SOURCE  Generate from the last frame and append locally using ffmpeg."
          , "--poll-interval-ms N  --max-poll-attempts N  -o/--output PATH"
          , "--model-options JSON  Raw settings for any OpenRouter model."
          , "Direct-provider --edit is unavailable through this client."
          , "Known models:"
          ]
        <> map (("  " <>) . fst) videoModels

runGenVideoCli :: IO ()
runGenVideoCli = runMediaCli renderGenVideoHelp parseGenVideoArgs run
  where
    run :: Text -> MediaOptions -> IO (Either Text [FilePath])
    run apiKey options@MediaOptions{extendSource} = case extendSource of
        Nothing -> runExceptT do
            values <- ExceptT (generateBytes apiKey options)
            liftIO
                (saveBytes "video" (outputPath options) (promptText options) (map (".mp4",) values))
        Just source -> do
            tools <-
                runExceptT
                    $ (,)
                    <$> ExceptT (requireExecutable "ffmpeg")
                    <*> ExceptT (requireExecutable "ffprobe")
            case tools of
                Left err -> pure (Left err)
                Right (ffmpeg, ffprobe) -> withPreparedVideoSource source $ \sourcePath ->
                    withTempFilePath "rake-frame.png" $ \framePath -> runExceptT do
                        ExceptT (extractLastFrame ffmpeg sourcePath framePath)
                        values <-
                            ExceptT
                                (generateBytes apiKey options{imageSources = [toText framePath], extendSource = Nothing})
                        continuation <- case values of
                            [bytes] -> pure bytes
                            _ -> throwE "Video extension requires exactly one generated continuation"
                        paths <-
                            liftIO (buildOutputPaths "video" (outputPath options) (promptText options) [".mp4"])
                        output <- maybe (throwE "Missing video output path") pure (viaNonEmpty head paths)
                        ExceptT $ withTempBinaryFile "rake-continuation.mp4" continuation $ \path ->
                            fmap (fmap (const [output])) (appendContinuation ffmpeg ffprobe sourcePath path output)

generateBytes :: Text -> MediaOptions -> IO (Either Text [BS.ByteString])
generateBytes
    apiKey
    MediaOptions
        { selectedModel
        , modelSettings
        , promptText
        , imageSources
        , lastFrameSource
        , pollInterval
        , pollAttempts
        } = runExceptT do
        images <- ExceptT (sequence <$> traverse resolveMediaSource imageSources)
        lastImage <- ExceptT (sequence <$> traverse resolveMediaSource lastFrameSource)
        ExceptT $ runProvider do
            let settings :: OpenRouterSettings '[Concurrent, Error RakeError, IOE]
                settings = defaultOpenRouterSettings apiKey
                polling :: VideoPollSettings
                polling = VideoPollSettings pollInterval pollAttempts
            withOpenRouterManager settings $ \client -> do
                job@VideoJob{contentUrls} <-
                    generateOpenRouterVideo
                        client
                        polling
                        (CustomVideoModel selectedModel modelSettings)
                        (VideoRequest promptText (viaNonEmpty head images) lastImage)
                let count :: Int
                    count = maybe 1 length contentUrls
                when (count == 0)
                    $ throwError (LlmInvalidResponseError "Completed video job has no outputs")
                traverse (downloadOpenRouterVideo client job) [0 .. count - 1]
