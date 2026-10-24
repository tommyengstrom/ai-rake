module RakeVideoFiles
    ( requireExecutable
    , withPreparedVideoSource
    , withTempBinaryFile
    , withTempFilePath
    , appendContinuation
    , extractLastFrame
    ) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import RakeCliSupport
import Relude
import System.Directory
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process (readProcessWithExitCode)

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
        pure
            ( Left
                "True video extension does not support data URLs yet. Write the source video to a file first."
            )
    | otherwise = do
        let sourcePath = toString source
        exists <- doesFileExist sourcePath
        if exists
            then useSource sourcePath
            else pure (Left ("Video source is not a URL or existing file: " <> source))

withTempBinaryFile
    :: String -> BS.ByteString -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withTempBinaryFile template bytes useFile =
    withTempFilePath template $ \path -> do
        writeBinaryFile path bytes
        useFile path

withTempFilePath :: String -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withTempFilePath template useFile = do
    tempDir <- getTemporaryDirectory
    bracket
        (IO.openBinaryTempFile tempDir template)
        (\(path, handle) -> IO.hClose handle >> ignoreMissingFile path)
        (\(path, handle) -> IO.hClose handle >> useFile path)

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
            runExternalCommand
                ffmpegPath
                (concatCommandArgs videoWidth videoHeight hasOriginalAudio hasContinuationAudio)
  where
    concatCommandArgs :: Int -> Int -> Bool -> Bool -> [String]
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
    pure
        $ probeResult
        >>= \rawDimensions ->
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
    pure
        $ case exitCode of
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
    preferredDetail :: Text
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
