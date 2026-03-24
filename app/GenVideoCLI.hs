module GenVideoCLI
    ( GenVideoProvider (..)
    , GenVideoHelpTopic (..)
    , CommonGenVideoOptions (..)
    , GrokGenVideoOptions (..)
    , GenVideoOptions (..)
    , ParseGenVideoArgsResult (..)
    , parseGenVideoArgs
    , renderGenVideoHelp
    , runGenVideoCli
    ) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static
import GenCliSupport
import Data.Text qualified as T
import Rake
import Rake.Providers.XAI.Imagine
import Relude hiding (exitFailure, getArgs, lookupEnv)
import System.Directory
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO qualified as IO
import System.Process (readProcessWithExitCode)

data GenVideoProvider
    = GenVideoProviderGrok
    deriving stock (Show, Eq)

data GenVideoHelpTopic
    = GenVideoHelpGeneral
    | GenVideoHelpGrok
    deriving stock (Show, Eq)

data CommonGenVideoOptions = CommonGenVideoOptions
    { commonVideoPromptText :: Text
    , commonVideoOutputPath :: Maybe FilePath
    }
    deriving stock (Show, Eq)

data GrokGenVideoOptions = GrokGenVideoOptions
    { grokVideoCommonOptions :: CommonGenVideoOptions
    , grokVideoModel :: Text
    , grokVideoImageSource :: Maybe Text
    , grokVideoEditSource :: Maybe Text
    , grokVideoExtendSource :: Maybe Text
    , grokVideoDuration :: Maybe Int
    , grokVideoAspectRatio :: Maybe Text
    , grokVideoResolution :: Maybe Text
    , grokVideoPollIntervalMilliseconds :: Int
    , grokVideoMaxPollAttempts :: Int
    }
    deriving stock (Show, Eq)

data GenVideoOptions
    = GenVideoGrok GrokGenVideoOptions
    deriving stock (Show, Eq)

data ParseGenVideoArgsResult
    = ParseGenVideoArgsHelp GenVideoHelpTopic
    | ParseGenVideoArgsError Text GenVideoHelpTopic
    | ParseGenVideoArgsSuccess GenVideoOptions
    deriving stock (Show, Eq)

data CommonParseState = CommonParseState
    { parseOutputPath :: Maybe FilePath
    , parsePromptParts :: [Text]
    }

defaultCommonParseState :: CommonParseState
defaultCommonParseState =
    CommonParseState
        { parseOutputPath = Nothing
        , parsePromptParts = []
        }

defaultGrokGenVideoOptions :: GrokGenVideoOptions
defaultGrokGenVideoOptions =
    GrokGenVideoOptions
        { grokVideoCommonOptions =
            CommonGenVideoOptions
                { commonVideoPromptText = ""
                , commonVideoOutputPath = Nothing
                }
        , grokVideoModel = "grok-imagine-video"
        , grokVideoImageSource = Nothing
        , grokVideoEditSource = Nothing
        , grokVideoExtendSource = Nothing
        , grokVideoDuration = Nothing
        , grokVideoAspectRatio = Nothing
        , grokVideoResolution = Nothing
        , grokVideoPollIntervalMilliseconds = 5000
        , grokVideoMaxPollAttempts = 120
        }

runGenVideoCli :: IO ()
runGenVideoCli = do
    progName <- getProgName
    args <- getArgs
    case parseGenVideoArgs (toText <$> args) of
        ParseGenVideoArgsHelp helpTopic ->
            putTextLn (renderGenVideoHelp progName helpTopic)
        ParseGenVideoArgsError err helpTopic -> do
            IO.hPutStrLn IO.stderr (toString err)
            putTextLn (renderGenVideoHelp progName helpTopic)
            exitFailure
        ParseGenVideoArgsSuccess options -> do
            result <- runGenVideo options
            case result of
                Left err -> do
                    IO.hPutStrLn IO.stderr (toString err)
                    exitFailure
                Right savedPaths ->
                    traverse_ (putTextLn . ("Saved video: " <>) . toText) savedPaths

runGenVideo :: GenVideoOptions -> IO (Either Text [FilePath])
runGenVideo = \case
    GenVideoGrok GrokGenVideoOptions
        { grokVideoCommonOptions = CommonGenVideoOptions{commonVideoPromptText, commonVideoOutputPath}
        , grokVideoModel
        , grokVideoImageSource
        , grokVideoEditSource
        , grokVideoExtendSource
        , grokVideoDuration
        , grokVideoAspectRatio
        , grokVideoResolution
        , grokVideoPollIntervalMilliseconds
        , grokVideoMaxPollAttempts
        } -> do
            maybeApiKey <- lookupEnv "XAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "XAI_API_KEY is required for Grok video generation")
                Just apiKey -> do
                    resolvedImageSource <- traverse resolveMediaSource grokVideoImageSource
                    resolvedEditSource <- traverse resolveMediaSource grokVideoEditSource
                    case (sequence resolvedImageSource, sequence resolvedEditSource) of
                        (Left err, _) ->
                            pure (Left err)
                        (_, Left err) ->
                            pure (Left err)
                        (Right maybeImageSource, Right maybeEditSource) ->
                            case (maybeImageSource, maybeEditSource, grokVideoExtendSource) of
                                (Just imageSource, Nothing, Nothing) -> do
                                    responseResult <-
                                        runProvider
                                            ( generateXAIVideo
                                                (videoSettings (toText apiKey) grokVideoPollIntervalMilliseconds grokVideoMaxPollAttempts)
                                                ( XAIImagineVideoRequest
                                                    { model = grokVideoModel
                                                    , prompt = commonVideoPromptText
                                                    , imageUrl = Just imageSource
                                                    , videoUrl = Nothing
                                                    , duration = grokVideoDuration
                                                    , aspectRatio = grokVideoAspectRatio
                                                    , resolution = grokVideoResolution
                                                    }
                                                )
                                            )
                                    handleVideoResponse commonVideoOutputPath commonVideoPromptText responseResult
                                (Nothing, Just editSource, Nothing) -> do
                                    responseResult <-
                                        runProvider
                                            ( generateXAIVideo
                                                (videoSettings (toText apiKey) grokVideoPollIntervalMilliseconds grokVideoMaxPollAttempts)
                                                ( XAIImagineVideoRequest
                                                    { model = grokVideoModel
                                                    , prompt = commonVideoPromptText
                                                    , imageUrl = Nothing
                                                    , videoUrl = Just editSource
                                                    , duration = grokVideoDuration
                                                    , aspectRatio = grokVideoAspectRatio
                                                    , resolution = grokVideoResolution
                                                    }
                                                )
                                            )
                                    handleVideoResponse commonVideoOutputPath commonVideoPromptText responseResult
                                (Nothing, Nothing, Just extendSource) ->
                                    extendVideoFromEnd
                                        (toText apiKey)
                                        grokVideoPollIntervalMilliseconds
                                        grokVideoMaxPollAttempts
                                        grokVideoModel
                                        commonVideoPromptText
                                        commonVideoOutputPath
                                        extendSource
                                        grokVideoDuration
                                        grokVideoAspectRatio
                                        grokVideoResolution
                                _ ->
                                    pure (Left "Exactly one of --image, --edit/--video, or --extend must be provided.")
  where
    videoSettings apiKey pollIntervalMilliseconds' maxPollAttempts' =
        (defaultXAIImagineSettings apiKey)
            { pollIntervalMilliseconds = pollIntervalMilliseconds'
            , maxPollAttempts = maxPollAttempts'
            }

    runProvider
        :: Eff '[Error RakeError, IOE] XAIVideoResponse
        -> IO (Either Text XAIVideoResponse)
    runProvider action = do
        result <-
            runEff
                . runErrorNoCallStack
                $ action
        pure (first renderRakeError result)

    handleVideoResponse maybeOutput promptText responseResult =
        case responseResult of
            Left err ->
                pure (Left err)
            Right XAIVideoResponse{status = XAIVideoDone, video = Just generatedVideo} ->
                saveGeneratedVideo maybeOutput promptText generatedVideo
            Right XAIVideoResponse{status} ->
                pure (Left ("The provider did not return a completed video. Final status: " <> renderVideoStatus status))

    extendVideoFromEnd apiKey pollIntervalMilliseconds' maxPollAttempts' modelName promptText maybeOutput sourceVideo duration aspectRatio resolution = do
        ffmpegPathResult <- requireExecutable "ffmpeg"
        ffprobePathResult <- requireExecutable "ffprobe"
        case (ffmpegPathResult, ffprobePathResult) of
            (Left err, _) ->
                pure (Left err)
            (_, Left err) ->
                pure (Left err)
            (Right ffmpegPath, Right ffprobePath) ->
                withPreparedVideoSource sourceVideo $ \sourceVideoPath ->
                    withTempFilePath "gen-video-last-frame" $ \framePath ->
                        withTempFilePath "gen-video-continuation" $ \continuationPath -> do
                            extractFrameResult <- extractLastFrame ffmpegPath sourceVideoPath framePath
                            case extractFrameResult of
                                Left err ->
                                    pure (Left err)
                                Right () -> do
                                    resolvedFrame <- resolveMediaSource (toText framePath)
                                    case resolvedFrame of
                                        Left err ->
                                            pure (Left err)
                                        Right frameSource -> do
                                            responseResult <-
                                                runProvider
                                                    ( generateXAIVideo
                                                        (videoSettings apiKey pollIntervalMilliseconds' maxPollAttempts')
                                                        ( XAIImagineVideoRequest
                                                            { model = modelName
                                                            , prompt = promptText
                                                            , imageUrl = Just frameSource
                                                            , videoUrl = Nothing
                                                            , duration = duration
                                                            , aspectRatio = aspectRatio
                                                            , resolution = resolution
                                                            }
                                                        )
                                                    )
                                            case responseResult of
                                                Left err ->
                                                    pure (Left err)
                                                Right XAIVideoResponse{status = XAIVideoDone, video = Just generatedVideo} -> do
                                                    downloadedVideo <- downloadGeneratedVideo generatedVideo
                                                    case downloadedVideo of
                                                        Left err ->
                                                            pure (Left err)
                                                        Right continuationBytes -> do
                                                            writeBinaryFile continuationPath continuationBytes
                                                            outputPaths <- buildOutputPaths "video" maybeOutput promptText [".mp4"]
                                                            case outputPaths of
                                                                [outputPath] -> do
                                                                    concatResult <-
                                                                        appendContinuation
                                                                            ffmpegPath
                                                                            ffprobePath
                                                                            sourceVideoPath
                                                                            continuationPath
                                                                            outputPath
                                                                    case concatResult of
                                                                        Left err ->
                                                                            pure (Left err)
                                                                        Right () ->
                                                                            pure (Right [outputPath])
                                                                _ ->
                                                                    pure (Left "Expected exactly one output path for video extension.")
                                                Right XAIVideoResponse{status} ->
                                                    pure (Left ("The provider did not return a completed continuation video. Final status: " <> renderVideoStatus status))

parseGenVideoArgs :: [Text] -> ParseGenVideoArgsResult
parseGenVideoArgs = \case
    [] ->
        ParseGenVideoArgsError "A provider is required. Use `grok`." GenVideoHelpGeneral
    "--help" : _ ->
        ParseGenVideoArgsHelp GenVideoHelpGeneral
    providerArg : rest ->
        case parseProvider providerArg of
            Nothing ->
                ParseGenVideoArgsError
                    ("Unknown provider: " <> providerArg <> ". Use `grok`.")
                    GenVideoHelpGeneral
            Just GenVideoProviderGrok ->
                parseGrokArgs rest

parseProvider :: Text -> Maybe GenVideoProvider
parseProvider = \case
    "grok" ->
        Just GenVideoProviderGrok
    _ ->
        Nothing

parseGrokArgs :: [Text] -> ParseGenVideoArgsResult
parseGrokArgs =
    go defaultCommonParseState defaultGrokGenVideoOptions
  where
    go commonState grokOptions = \case
        [] ->
            finalize commonState grokOptions
        "--help" : _ ->
            ParseGenVideoArgsHelp GenVideoHelpGrok
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                grokOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} grokOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} grokOptions rest
        "--model" : modelName : rest ->
            go commonState grokOptions{grokVideoModel = modelName} rest
        "--image" : imageSource : rest ->
            go commonState grokOptions{grokVideoImageSource = Just imageSource} rest
        "--edit" : videoSource : rest ->
            go commonState grokOptions{grokVideoEditSource = Just videoSource} rest
        "--extend" : videoSource : rest ->
            go commonState grokOptions{grokVideoExtendSource = Just videoSource} rest
        "--video" : videoSource : rest ->
            go commonState grokOptions{grokVideoEditSource = Just videoSource} rest
        "--duration" : rawDuration : rest ->
            case parsePositiveIntOption "--duration" rawDuration of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpGrok
                Right durationSeconds ->
                    go commonState grokOptions{grokVideoDuration = Just durationSeconds} rest
        "--aspect-ratio" : aspectRatioValue : rest ->
            go commonState grokOptions{grokVideoAspectRatio = Just aspectRatioValue} rest
        "--resolution" : resolutionValue : rest ->
            go commonState grokOptions{grokVideoResolution = Just resolutionValue} rest
        "--poll-interval-ms" : rawPollInterval : rest ->
            case parsePositiveIntOption "--poll-interval-ms" rawPollInterval of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpGrok
                Right pollIntervalMilliseconds ->
                    go commonState grokOptions{grokVideoPollIntervalMilliseconds = pollIntervalMilliseconds} rest
        "--max-poll-attempts" : rawMaxPollAttempts : rest ->
            case parsePositiveIntOption "--max-poll-attempts" rawMaxPollAttempts of
                Left err ->
                    ParseGenVideoArgsError err GenVideoHelpGrok
                Right maxPollAttempts ->
                    go commonState grokOptions{grokVideoMaxPollAttempts = maxPollAttempts} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} grokOptions rest
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState grokOptions{grokVideoModel = modelName} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState grokOptions{grokVideoImageSource = Just imageSource} rest
            | Just videoSource <- T.stripPrefix "--edit=" arg ->
                go commonState grokOptions{grokVideoEditSource = Just videoSource} rest
            | Just videoSource <- T.stripPrefix "--extend=" arg ->
                go commonState grokOptions{grokVideoExtendSource = Just videoSource} rest
            | Just videoSource <- T.stripPrefix "--video=" arg ->
                go commonState grokOptions{grokVideoEditSource = Just videoSource} rest
            | Just rawDuration <- T.stripPrefix "--duration=" arg ->
                case parsePositiveIntOption "--duration" rawDuration of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpGrok
                    Right durationSeconds ->
                        go commonState grokOptions{grokVideoDuration = Just durationSeconds} rest
            | Just aspectRatioValue <- T.stripPrefix "--aspect-ratio=" arg ->
                go commonState grokOptions{grokVideoAspectRatio = Just aspectRatioValue} rest
            | Just resolutionValue <- T.stripPrefix "--resolution=" arg ->
                go commonState grokOptions{grokVideoResolution = Just resolutionValue} rest
            | Just rawPollInterval <- T.stripPrefix "--poll-interval-ms=" arg ->
                case parsePositiveIntOption "--poll-interval-ms" rawPollInterval of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpGrok
                    Right pollIntervalMilliseconds ->
                        go commonState grokOptions{grokVideoPollIntervalMilliseconds = pollIntervalMilliseconds} rest
            | Just rawMaxPollAttempts <- T.stripPrefix "--max-poll-attempts=" arg ->
                case parsePositiveIntOption "--max-poll-attempts" rawMaxPollAttempts of
                    Left err ->
                        ParseGenVideoArgsError err GenVideoHelpGrok
                    Right maxPollAttempts ->
                        go commonState grokOptions{grokVideoMaxPollAttempts = maxPollAttempts} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenVideoArgsError ("Unknown Grok option: " <> arg) GenVideoHelpGrok
            | otherwise ->
                go (appendPromptParts commonState [arg]) grokOptions rest

    finalize CommonParseState{parseOutputPath, parsePromptParts} grokOptions
        | null parsePromptParts =
            ParseGenVideoArgsError "A prompt is required." GenVideoHelpGrok
        | hasGrokVideoSourceConflict grokOptions =
            ParseGenVideoArgsError "Use exactly one of --image, --edit/--video, or --extend." GenVideoHelpGrok
        | missingGrokVideoSource grokOptions =
            ParseGenVideoArgsError "Use --image SOURCE for image-to-video generation, --edit SOURCE to update an existing video, or --extend SOURCE to append a continuation." GenVideoHelpGrok
        | otherwise =
            ParseGenVideoArgsSuccess $
                GenVideoGrok
                    grokOptions
                        { grokVideoCommonOptions =
                            CommonGenVideoOptions
                                { commonVideoPromptText = T.unwords parsePromptParts
                                , commonVideoOutputPath = parseOutputPath
                                }
                        }

parsePositiveIntOption :: Text -> Text -> Either Text Int
parsePositiveIntOption optionName rawValue =
    case readMaybe @Int (toString rawValue) of
        Nothing ->
            Left ("Invalid value for " <> optionName <> ": " <> rawValue)
        Just value
            | value < 1 ->
                Left (optionName <> " must be at least 1")
            | otherwise ->
                Right value

appendPromptParts :: CommonParseState -> [Text] -> CommonParseState
appendPromptParts commonState@CommonParseState{parsePromptParts} extraPromptParts =
    commonState{parsePromptParts = parsePromptParts <> extraPromptParts}

hasGrokVideoSourceConflict :: GrokGenVideoOptions -> Bool
hasGrokVideoSourceConflict GrokGenVideoOptions{grokVideoImageSource, grokVideoEditSource, grokVideoExtendSource} =
    length (catMaybes [grokVideoImageSource, grokVideoEditSource, grokVideoExtendSource]) > 1

missingGrokVideoSource :: GrokGenVideoOptions -> Bool
missingGrokVideoSource GrokGenVideoOptions{grokVideoImageSource, grokVideoEditSource, grokVideoExtendSource} =
    isNothing grokVideoImageSource
        && isNothing grokVideoEditSource
        && isNothing grokVideoExtendSource

renderGenVideoHelp :: String -> GenVideoHelpTopic -> Text
renderGenVideoHelp progName = \case
    GenVideoHelpGeneral ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " grok [OPTIONS] PROMPT"
            , ""
            , "Providers:"
            , "  grok    Generate videos with xAI Grok Imagine. Default model: grok-imagine-video"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "Grok options:"
                   ]
                <> grokOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local file path."
                   , "  Use `" <> toText progName <> " grok --help` for focused help."
                   , "  Use `--image SOURCE` for image-to-video generation."
                   , "  Use `--edit SOURCE` to update an existing video."
                   , "  Use `--extend SOURCE` to append a continuation to the end of a video."
                   , "  `--video SOURCE` is kept as a compatible alias for `--edit`."
                   , "  `--duration`, `--aspect-ratio`, and `--resolution` apply to image-to-video and extend."
                   , "  `--extend` requires local `ffmpeg` and `ffprobe`."
                   , "  `--extend` currently supports local files and URLs, not data URLs."
                   , "  XAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " grok \"She walk away\" --image girl.jpg"
                   , "  " <> toText progName <> " grok --image=girl.jpg --duration=8 \"She walk away\""
                   , "  " <> toText progName <> " grok --edit=clip.mp4 \"make the lighting moodier\""
                   , "  " <> toText progName <> " grok --extend=clip.mp4 \"continue the scene for 5 more seconds\""
                   ]
    GenVideoHelpGrok ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " grok [OPTIONS] PROMPT"
            , ""
            , "Default model: grok-imagine-video"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "Grok options:"
                   ]
                <> grokOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local file path."
                   , "  Use `--image SOURCE` for image-to-video generation."
                   , "  Use `--edit SOURCE` to update an existing video."
                   , "  Use `--extend SOURCE` to append a continuation to the end of a video."
                   , "  `--video SOURCE` is kept as a compatible alias for `--edit`."
                   , "  `--duration`, `--aspect-ratio`, and `--resolution` apply to image-to-video and extend."
                   , "  `--extend` requires local `ffmpeg` and `ffprobe`."
                   , "  `--extend` currently supports local files and URLs, not data URLs."
                   , "  XAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " grok \"She walk away\" --image girl.jpg"
                   , "  " <> toText progName <> " grok --image=girl.jpg --duration=8 \"She walk away\""
                   , "  " <> toText progName <> " grok --edit=clip.mp4 \"make the lighting moodier\""
                   , "  " <> toText progName <> " grok --extend=clip.mp4 \"continue the scene for 5 more seconds\""
                   ]
  where
    commonOptionLines =
        [ "  -o, --output PATH            Output file path or filename prefix."
        , "  --help                       Show help."
        ]

    grokOptionLines =
        [ "  --model MODEL                Override the Grok video model."
        , "  --image SOURCE               Input still image URL or local file."
        , "  --edit SOURCE                Update an existing video URL or local file."
        , "  --extend SOURCE              Append a continuation to the end of a video."
        , "  --video SOURCE               Alias for --edit."
        , "  --duration SECONDS           Duration hint for image-to-video or extend."
        , "  --aspect-ratio RATIO         Aspect ratio hint, for example 16:9."
        , "  --resolution RESOLUTION      Resolution hint, for example 720p."
        , "  --poll-interval-ms N         Poll interval in milliseconds. Default: 5000"
        , "  --max-poll-attempts N        Maximum poll attempts. Default: 120"
        ]

saveGeneratedVideo :: Maybe FilePath -> Text -> GeneratedVideo -> IO (Either Text [FilePath])
saveGeneratedVideo maybeOutput promptText GeneratedVideo{url = maybeUrl} =
    case maybeUrl of
        Nothing ->
            pure (Left "Video payload has no url")
        Just videoUrl -> do
            downloadResult <- downloadBinary videoUrl
            case downloadResult of
                Left err ->
                    pure (Left err)
                Right videoBytes -> do
                    outputPaths <-
                        buildOutputPaths
                            "video"
                            maybeOutput
                            promptText
                            [fromMaybe ".mp4" (urlExtension videoUrl)]
                    traverse_
                        (\path -> writeBinaryFile path videoBytes)
                        outputPaths
                    pure (Right outputPaths)

downloadGeneratedVideo :: GeneratedVideo -> IO (Either Text BS.ByteString)
downloadGeneratedVideo GeneratedVideo{url = maybeUrl} =
    case maybeUrl of
        Nothing ->
            pure (Left "Video payload has no url")
        Just videoUrl ->
            downloadBinary videoUrl

requireExecutable :: FilePath -> IO (Either Text FilePath)
requireExecutable executableName =
    findExecutable executableName <&> \case
        Nothing ->
            Left
                ( toText executableName
                    <> " is required for true video extension. xAI's public API supports video edits, but not append-style continuation, so `--extend` uses local "
                    <> toText executableName
                    <> " tooling to stitch the original clip and the generated continuation together."
                )
        Just executablePath ->
            Right executablePath

withPreparedVideoSource :: Text -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withPreparedVideoSource source useSource
    | "http://" `T.isPrefixOf` source || "https://" `T.isPrefixOf` source = do
        downloadResult <- downloadBinary source
        case downloadResult of
            Left err ->
                pure (Left err)
            Right videoBytes ->
                withTempBinaryFile "gen-video-source" videoBytes useSource
    | "data:" `T.isPrefixOf` source =
        pure (Left "True video extension does not support data URLs yet. Write the source video to a file first.")
    | otherwise = do
        let sourcePath = toString source
        exists <- doesFileExist sourcePath
        if exists
            then useSource sourcePath
            else pure (Left ("Video source is not a URL or existing file: " <> source))

withTempBinaryFile :: String -> BS.ByteString -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withTempBinaryFile template bytes useFile =
    withTempFilePath template $ \path -> do
        writeBinaryFile path bytes
        useFile path

withTempFilePath :: String -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withTempFilePath template useFile = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- IO.openBinaryTempFile tempDir template
    IO.hClose handle
    result <- useFile path
    ignoreMissingFile path
    pure result

appendContinuation
    :: FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO (Either Text ())
appendContinuation ffmpegPath ffprobePath sourceVideoPath continuationPath outputPath = do
    originalVideoSize <- probeVideoSize ffprobePath sourceVideoPath
    originalHasAudio <- probeHasAudio ffprobePath sourceVideoPath
    continuationHasAudio <- probeHasAudio ffprobePath continuationPath
    case (originalVideoSize, originalHasAudio, continuationHasAudio) of
        (Left err, _, _) ->
            pure (Left err)
        (_, Left err, _) ->
            pure (Left err)
        (_, _, Left err) ->
            pure (Left err)
        (Right (videoWidth, videoHeight), Right hasOriginalAudio, Right hasContinuationAudio) ->
            runExternalCommand ffmpegPath (concatCommandArgs videoWidth videoHeight hasOriginalAudio hasContinuationAudio)
  where
    concatCommandArgs videoWidth videoHeight hasOriginalAudio hasContinuationAudio
        | hasOriginalAudio && hasContinuationAudio =
            [ "-y"
            , "-i"
            , sourceVideoPath
            , "-i"
            , continuationPath
            , "-filter_complex"
            , concatFilter videoWidth videoHeight True
            , "-map"
            , "[outv]"
            , "-map"
            , "[outa]"
            , "-c:v"
            , "libx264"
            , "-pix_fmt"
            , "yuv420p"
            , "-c:a"
            , "aac"
            , "-movflags"
            , "+faststart"
            , outputPath
            ]
        | otherwise =
            [ "-y"
            , "-i"
            , sourceVideoPath
            , "-i"
            , continuationPath
            , "-filter_complex"
            , concatFilter videoWidth videoHeight False
            , "-map"
            , "[outv]"
            , "-c:v"
            , "libx264"
            , "-pix_fmt"
            , "yuv420p"
            , "-movflags"
            , "+faststart"
            , outputPath
            ]

concatFilter :: Int -> Int -> Bool -> String
concatFilter videoWidth videoHeight includeAudio =
    if includeAudio
        then
            videoPrelude
                <> "[0:a]aformat=sample_rates=48000:channel_layouts=stereo[a0];"
                <> "[1:a]aformat=sample_rates=48000:channel_layouts=stereo[a1];"
                <> "[v0][a0][v1][a1]concat=n=2:v=1:a=1[outv][outa]"
        else
            videoPrelude
                <> "[v0][v1]concat=n=2:v=1:a=0[outv]"
  where
    scaleAndPad :: Int -> String -> String
    scaleAndPad inputIndex outputLabel =
        "["
            <> show inputIndex
            <> ":v]fps=30,scale="
            <> show videoWidth
            <> ":"
            <> show videoHeight
            <> ":force_original_aspect_ratio=decrease,pad="
            <> show videoWidth
            <> ":"
            <> show videoHeight
            <> ":(ow-iw)/2:(oh-ih)/2,setsar=1["
            <> outputLabel
            <> "];"

    videoPrelude :: String
    videoPrelude =
        scaleAndPad (0 :: Int) "v0"
            <> scaleAndPad (1 :: Int) "v1"

probeVideoSize :: FilePath -> FilePath -> IO (Either Text (Int, Int))
probeVideoSize ffprobePath videoPath = do
    probeResult <-
        runExternalCommandWithOutput
            ffprobePath
            [ "-v"
            , "error"
            , "-select_streams"
            , "v:0"
            , "-show_entries"
            , "stream=width,height"
            , "-of"
            , "csv=s=x:p=0"
            , videoPath
            ]
    pure $
        probeResult >>= \rawDimensions ->
            case break (== 'x') (toString (T.strip rawDimensions)) of
                (rawWidth, 'x' : rawHeight) ->
                    case (readMaybe @Int rawWidth, readMaybe @Int rawHeight) of
                        (Just videoWidth, Just videoHeight) ->
                            Right (videoWidth, videoHeight)
                        _ ->
                            Left ("Could not parse video dimensions from ffprobe output: " <> rawDimensions)
                _ ->
                    Left ("Could not parse video dimensions from ffprobe output: " <> rawDimensions)

probeHasAudio :: FilePath -> FilePath -> IO (Either Text Bool)
probeHasAudio ffprobePath videoPath =
    runExternalCommandWithOutput
        ffprobePath
        [ "-v"
        , "error"
        , "-select_streams"
        , "a:0"
        , "-show_entries"
        , "stream=index"
        , "-of"
        , "csv=p=0"
        , videoPath
        ]
        <&> fmap (not . T.null . T.strip)

extractLastFrame :: FilePath -> FilePath -> FilePath -> IO (Either Text ())
extractLastFrame ffmpegPath sourceVideoPath framePath =
    runExternalCommand
        ffmpegPath
        [ "-y"
        , "-sseof"
        , "-0.1"
        , "-i"
        , sourceVideoPath
        , "-frames:v"
        , "1"
        , "-f"
        , "image2"
        , "-vcodec"
        , "png"
        , framePath
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
ignoreMissingFile path =
    do
        exists <- doesFileExist path
        when exists (removeFile path)

renderVideoStatus :: XAIVideoStatus -> Text
renderVideoStatus = \case
    XAIVideoPending ->
        "pending"
    XAIVideoDone ->
        "done"
    XAIVideoExpired ->
        "expired"
    XAIVideoUnknown other ->
        other
