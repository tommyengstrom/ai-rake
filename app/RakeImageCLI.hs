module RakeImageCLI
    ( GenImageProvider (..)
    , GenImageHelpTopic (..)
    , CommonGenImageOptions (..)
    , OpenAIGenImageOptions (..)
    , XAIGenImageOptions (..)
    , Banana2GenImageOptions (..)
    , GenImageOptions (..)
    , ParseGenImageArgsResult (..)
    , parseGenImageArgs
    , renderGenImageHelp
    , slugifyPrompt
    , runGenImageCli
    ) where

import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import RakeCliSupport
import Rake
import Rake.Providers.Gemini.Images
import Rake.Providers.OpenAI.Images
import Rake.Providers.XAI.Imagine
import Relude hiding (exitFailure, getArgs, lookupEnv)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.IO qualified as IO

data GenImageProvider
    = GenImageProviderOpenAI
    | GenImageProviderXAI
    | GenImageProviderBanana2
    deriving stock (Show, Eq)

data GenImageHelpTopic
    = GenImageHelpGeneral
    | GenImageHelpOpenAI
    | GenImageHelpXAI
    | GenImageHelpBanana2
    deriving stock (Show, Eq)

data CommonGenImageOptions = CommonGenImageOptions
    { commonPromptText :: Text
    , commonOutputPath :: Maybe FilePath
    , commonImageCount :: Int
    }
    deriving stock (Show, Eq)

data OpenAIGenImageOptions = OpenAIGenImageOptions
    { openAICommonOptions :: CommonGenImageOptions
    , openAIModel :: Text
    , openAISize :: Maybe Text
    , openAIQuality :: Maybe Text
    , openAIOutputFormat :: Text
    , openAIOutputCompression :: Maybe Int
    , openAIBackground :: Maybe Text
    , openAIModeration :: Maybe Text
    , openAIUser :: Maybe Text
    , openAIInputImageSources :: [Text]
    , openAIInputFileIds :: [Text]
    , openAIMaskSource :: Maybe Text
    , openAIMaskFileId :: Maybe Text
    , openAIInputFidelity :: Maybe Text
    }
    deriving stock (Show, Eq)

data XAIGenImageOptions = XAIGenImageOptions
    { xaiCommonOptions :: CommonGenImageOptions
    , xaiModel :: Text
    , xaiInputImageSources :: [Text]
    , xaiAspectRatio :: Maybe Text
    , xaiResolution :: Maybe Text
    , xaiResponseFormat :: Text
    }
    deriving stock (Show, Eq)

data Banana2GenImageOptions = Banana2GenImageOptions
    { banana2CommonOptions :: CommonGenImageOptions
    , banana2Model :: Text
    , banana2InputImageSources :: [Text]
    , banana2AspectRatio :: Maybe Text
    , banana2ImageSize :: Maybe Text
    }
    deriving stock (Show, Eq)

data GenImageOptions
    = GenImageOpenAI OpenAIGenImageOptions
    | GenImageXAI XAIGenImageOptions
    | GenImageBanana2 Banana2GenImageOptions
    deriving stock (Show, Eq)

data ParseGenImageArgsResult
    = ParseGenImageArgsHelp GenImageHelpTopic
    | ParseGenImageArgsError Text GenImageHelpTopic
    | ParseGenImageArgsSuccess GenImageOptions
    deriving stock (Show, Eq)

data CommonParseState = CommonParseState
    { parseOutputPath :: Maybe FilePath
    , parseImageCount :: Int
    , parsePromptParts :: [Text]
    }

defaultCommonParseState :: CommonParseState
defaultCommonParseState =
    CommonParseState
        { parseOutputPath = Nothing
        , parseImageCount = 1
        , parsePromptParts = []
        }

defaultOpenAIGenImageOptions :: OpenAIGenImageOptions
defaultOpenAIGenImageOptions =
    OpenAIGenImageOptions
        { openAICommonOptions =
            CommonGenImageOptions
                { commonPromptText = ""
                , commonOutputPath = Nothing
                , commonImageCount = 1
                }
        , openAIModel = "gpt-image-1.5"
        , openAISize = Nothing
        , openAIQuality = Nothing
        , openAIOutputFormat = "png"
        , openAIOutputCompression = Nothing
        , openAIBackground = Nothing
        , openAIModeration = Nothing
        , openAIUser = Nothing
        , openAIInputImageSources = []
        , openAIInputFileIds = []
        , openAIMaskSource = Nothing
        , openAIMaskFileId = Nothing
        , openAIInputFidelity = Nothing
        }

defaultXAIGenImageOptions :: XAIGenImageOptions
defaultXAIGenImageOptions =
    XAIGenImageOptions
        { xaiCommonOptions =
            CommonGenImageOptions
                { commonPromptText = ""
                , commonOutputPath = Nothing
                , commonImageCount = 1
                }
        , xaiModel = "grok-imagine-image"
        , xaiInputImageSources = []
        , xaiAspectRatio = Nothing
        , xaiResolution = Nothing
        , xaiResponseFormat = "b64_json"
        }

defaultBanana2GenImageOptions :: Banana2GenImageOptions
defaultBanana2GenImageOptions =
    Banana2GenImageOptions
        { banana2CommonOptions =
            CommonGenImageOptions
                { commonPromptText = ""
                , commonOutputPath = Nothing
                , commonImageCount = 1
                }
        , banana2Model = "gemini-2.5-flash-image"
        , banana2InputImageSources = []
        , banana2AspectRatio = Nothing
        , banana2ImageSize = Nothing
        }

runGenImageCli :: IO ()
runGenImageCli = do
    progName <- getProgName
    args <- getArgs
    case parseGenImageArgs (toText <$> args) of
        ParseGenImageArgsHelp helpTopic ->
            putTextLn (renderGenImageHelp progName helpTopic)
        ParseGenImageArgsError err helpTopic -> do
            IO.hPutStrLn IO.stderr (toString err)
            putTextLn (renderGenImageHelp progName helpTopic)
            exitFailure
        ParseGenImageArgsSuccess options -> do
            result <- runGenImage options
            case result of
                Left err -> do
                    IO.hPutStrLn IO.stderr (toString err)
                    exitFailure
                Right savedPaths ->
                    traverse_ (putTextLn . ("Saved image: " <>) . toText) savedPaths

runGenImage :: GenImageOptions -> IO (Either Text [FilePath])
runGenImage = \case
    GenImageOpenAI OpenAIGenImageOptions
        { openAICommonOptions = CommonGenImageOptions{commonPromptText, commonOutputPath, commonImageCount}
        , openAIModel
        , openAISize
        , openAIQuality
        , openAIOutputFormat
        , openAIOutputCompression
        , openAIBackground
        , openAIModeration
        , openAIUser
        , openAIInputImageSources
        , openAIInputFileIds
        , openAIMaskSource
        , openAIMaskFileId
        , openAIInputFidelity
        } -> do
            maybeApiKey <- lookupEnv "OPENAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "OPENAI_API_KEY is required for gptimage image generation")
                Just apiKey -> do
                    resolvedInputImages <- traverse resolveMediaSource openAIInputImageSources
                    resolvedMaskSource <- traverse resolveMediaSource openAIMaskSource
                    case (sequence resolvedInputImages, sequence resolvedMaskSource) of
                        (Left err, _) ->
                            pure (Left err)
                        (_, Left err) ->
                            pure (Left err)
                        (Right imageUrls, Right maybeMaskUrl) ->
                            if isJust maybeMaskUrl && isJust openAIMaskFileId
                                then pure (Left "Use either --mask or --mask-file-id, not both")
                                else do
                                    let settings = defaultOpenAIImagesSettings (toText apiKey)
                                        request :: OpenAIImageRequest
                                        request =
                                            OpenAIImageRequest
                                                { model = openAIModel
                                                , prompt = commonPromptText
                                                , inputImages =
                                                    map OpenAIImageUrl imageUrls
                                                        <> map OpenAIImageFileId openAIInputFileIds
                                                , mask =
                                                    (OpenAIImageUrl <$> maybeMaskUrl)
                                                        <|> (OpenAIImageFileId <$> openAIMaskFileId)
                                                , background = openAIBackground
                                                , inputFidelity = openAIInputFidelity
                                                , moderation = openAIModeration
                                                , n = Just commonImageCount
                                                , outputCompression = openAIOutputCompression
                                                , outputFormat = Just openAIOutputFormat
                                                , quality = openAIQuality
                                                , size = openAISize
                                                , user = openAIUser
                                                }
                                    responseResult <- runProvider (generateOpenAIImage settings request)
                                    handleImageResponse commonOutputPath commonPromptText responseResult
    GenImageXAI XAIGenImageOptions
        { xaiCommonOptions = CommonGenImageOptions{commonPromptText, commonOutputPath, commonImageCount}
        , xaiModel
        , xaiInputImageSources
        , xaiAspectRatio
        , xaiResolution
        , xaiResponseFormat
        } -> do
            maybeApiKey <- lookupEnv "XAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "XAI_API_KEY is required for xai image generation")
                Just apiKey -> do
                    resolvedInputImages <- traverse resolveMediaSource xaiInputImageSources
                    case sequence resolvedInputImages of
                        Left err ->
                            pure (Left err)
                        Right imageUrls -> do
                            let settings = defaultXAIImagineSettings (toText apiKey)
                                request :: XAIImagineImageRequest
                                request =
                                    XAIImagineImageRequest
                                        { model = xaiModel
                                        , prompt = commonPromptText
                                        , inputImages = imageUrls
                                        , n = Just commonImageCount
                                        , aspectRatio = xaiAspectRatio
                                        , resolution = xaiResolution
                                        , responseFormat = Just xaiResponseFormat
                                        }
                            responseResult <- runProvider (generateXAIImage settings request)
                            handleImageResponse commonOutputPath commonPromptText responseResult
    GenImageBanana2 Banana2GenImageOptions
        { banana2CommonOptions = CommonGenImageOptions{commonPromptText, commonOutputPath, commonImageCount = _}
        , banana2Model
        , banana2InputImageSources
        , banana2AspectRatio
        , banana2ImageSize
        } -> do
            maybeApiKey <- lookupEnv "GEMINI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "GEMINI_API_KEY is required for Banana2 image generation")
                Just apiKey -> do
                        resolvedInputImages <- traverse resolveInlineImageSource banana2InputImageSources
                        case sequence resolvedInputImages of
                            Left err ->
                                pure (Left err)
                            Right inputImages -> do
                                let settings = defaultGeminiImagesSettings (toText apiKey)
                                    request :: GeminiImageRequest
                                    request =
                                        GeminiImageRequest
                                            { model = banana2Model
                                            , prompt = commonPromptText
                                            , inputImages
                                            , aspectRatio = banana2AspectRatio
                                            , imageSize = banana2ImageSize
                                            }
                                responseResult <- runProvider (generateGeminiImage settings request)
                                handleImageResponse commonOutputPath commonPromptText responseResult
  where
    runProvider
        :: Eff '[Error RakeError, IOE] ImageGenerationResponse
        -> IO (Either Text ImageGenerationResponse)
    runProvider action = do
        result <-
            runEff
                . runErrorNoCallStack
                $ action
        pure (first renderRakeError result)

    handleImageResponse maybeOutput promptText responseResult =
        case responseResult of
            Left err ->
                pure (Left err)
            Right ImageGenerationResponse{images}
                | null images ->
                    pure (Left "The provider returned no images")
                | otherwise ->
                    saveGeneratedImages maybeOutput promptText images

parseGenImageArgs :: [Text] -> ParseGenImageArgsResult
parseGenImageArgs = \case
    [] ->
        ParseGenImageArgsError "A model is required. Use `gptimage`, `xai`, or `banana2`." GenImageHelpGeneral
    "--help" : _ ->
        ParseGenImageArgsHelp GenImageHelpGeneral
    providerArg : rest ->
        case parseProvider providerArg of
            Nothing ->
                ParseGenImageArgsError
                    ("Unknown model: " <> providerArg <> ". Use `gptimage`, `xai`, or `banana2`.")
                    GenImageHelpGeneral
            Just GenImageProviderOpenAI ->
                parseOpenAIArgs rest
            Just GenImageProviderXAI ->
                parseXAIArgs rest
            Just GenImageProviderBanana2 ->
                parseBanana2Args rest

parseProvider :: Text -> Maybe GenImageProvider
parseProvider = \case
    "gptimage" ->
        Just GenImageProviderOpenAI
    "xai" ->
        Just GenImageProviderXAI
    "banana2" ->
        Just GenImageProviderBanana2
    _ ->
        Nothing

parseOpenAIArgs :: [Text] -> ParseGenImageArgsResult
parseOpenAIArgs =
    go defaultCommonParseState defaultOpenAIGenImageOptions
  where
    go commonState openAIOptions = \case
        [] ->
            finalize commonState openAIOptions
        "--help" : _ ->
            ParseGenImageArgsHelp GenImageHelpOpenAI
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                openAIOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} openAIOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} openAIOptions rest
        "--count" : rawCount : rest ->
            case parsePositiveIntOption "--count" rawCount of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpOpenAI
                Right imageCount ->
                    go commonState{parseImageCount = imageCount} openAIOptions rest
        "-n" : rawCount : rest ->
            case parsePositiveIntOption "--count" rawCount of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpOpenAI
                Right imageCount ->
                    go commonState{parseImageCount = imageCount} openAIOptions rest
        "--model" : modelName : rest ->
            go commonState openAIOptions{openAIModel = modelName} rest
        "--size" : sizeValue : rest ->
            go commonState openAIOptions{openAISize = Just sizeValue} rest
        "--quality" : qualityValue : rest ->
            go commonState openAIOptions{openAIQuality = Just qualityValue} rest
        "--output-format" : formatValue : rest ->
            go commonState openAIOptions{openAIOutputFormat = formatValue} rest
        "--output-compression" : rawCompression : rest ->
            case parseBoundedIntOption "--output-compression" 0 100 rawCompression of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpOpenAI
                Right outputCompression ->
                    go commonState openAIOptions{openAIOutputCompression = Just outputCompression} rest
        "--background" : backgroundValue : rest ->
            go commonState openAIOptions{openAIBackground = Just backgroundValue} rest
        "--moderation" : moderationValue : rest ->
            go commonState openAIOptions{openAIModeration = Just moderationValue} rest
        "--user" : userValue : rest ->
            go commonState openAIOptions{openAIUser = Just userValue} rest
        "--image" : imageSource : rest ->
            go commonState (appendOpenAIInputImageSource openAIOptions imageSource) rest
        "--image-file-id" : fileId : rest ->
            go commonState (appendOpenAIInputFileId openAIOptions fileId) rest
        "--mask" : maskSource : rest ->
            go commonState openAIOptions{openAIMaskSource = Just maskSource} rest
        "--mask-file-id" : maskFileId : rest ->
            go commonState openAIOptions{openAIMaskFileId = Just maskFileId} rest
        "--input-fidelity" : fidelityValue : rest ->
            go commonState openAIOptions{openAIInputFidelity = Just fidelityValue} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} openAIOptions rest
            | Just rawCount <- T.stripPrefix "--count=" arg ->
                case parsePositiveIntOption "--count" rawCount of
                    Left err ->
                        ParseGenImageArgsError err GenImageHelpOpenAI
                    Right imageCount ->
                        go commonState{parseImageCount = imageCount} openAIOptions rest
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState openAIOptions{openAIModel = modelName} rest
            | Just sizeValue <- T.stripPrefix "--size=" arg ->
                go commonState openAIOptions{openAISize = Just sizeValue} rest
            | Just qualityValue <- T.stripPrefix "--quality=" arg ->
                go commonState openAIOptions{openAIQuality = Just qualityValue} rest
            | Just formatValue <- T.stripPrefix "--output-format=" arg ->
                go commonState openAIOptions{openAIOutputFormat = formatValue} rest
            | Just rawCompression <- T.stripPrefix "--output-compression=" arg ->
                case parseBoundedIntOption "--output-compression" 0 100 rawCompression of
                    Left err ->
                        ParseGenImageArgsError err GenImageHelpOpenAI
                    Right outputCompression ->
                        go commonState openAIOptions{openAIOutputCompression = Just outputCompression} rest
            | Just backgroundValue <- T.stripPrefix "--background=" arg ->
                go commonState openAIOptions{openAIBackground = Just backgroundValue} rest
            | Just moderationValue <- T.stripPrefix "--moderation=" arg ->
                go commonState openAIOptions{openAIModeration = Just moderationValue} rest
            | Just userValue <- T.stripPrefix "--user=" arg ->
                go commonState openAIOptions{openAIUser = Just userValue} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState (appendOpenAIInputImageSource openAIOptions imageSource) rest
            | Just fileId <- T.stripPrefix "--image-file-id=" arg ->
                go commonState (appendOpenAIInputFileId openAIOptions fileId) rest
            | Just maskSource <- T.stripPrefix "--mask=" arg ->
                go commonState openAIOptions{openAIMaskSource = Just maskSource} rest
            | Just maskFileId <- T.stripPrefix "--mask-file-id=" arg ->
                go commonState openAIOptions{openAIMaskFileId = Just maskFileId} rest
            | Just fidelityValue <- T.stripPrefix "--input-fidelity=" arg ->
                go commonState openAIOptions{openAIInputFidelity = Just fidelityValue} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenImageArgsError ("Unknown gptimage option: " <> arg) GenImageHelpOpenAI
            | otherwise ->
                go (appendPromptParts commonState [arg]) openAIOptions rest

    finalize CommonParseState{parseOutputPath, parseImageCount, parsePromptParts} openAIOptions
        | null parsePromptParts =
            ParseGenImageArgsError "A prompt is required." GenImageHelpOpenAI
        | hasOpenAIMaskConflict openAIOptions =
            ParseGenImageArgsError "Use either --mask or --mask-file-id, not both." GenImageHelpOpenAI
        | otherwise =
            ParseGenImageArgsSuccess $
                GenImageOpenAI
                    openAIOptions
                        { openAICommonOptions =
                            CommonGenImageOptions
                                { commonPromptText = T.unwords parsePromptParts
                                , commonOutputPath = parseOutputPath
                                , commonImageCount = parseImageCount
                                }
                        }

parseXAIArgs :: [Text] -> ParseGenImageArgsResult
parseXAIArgs =
    go defaultCommonParseState defaultXAIGenImageOptions
  where
    go commonState xaiOptions = \case
        [] ->
            finalize commonState xaiOptions
        "--help" : _ ->
            ParseGenImageArgsHelp GenImageHelpXAI
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                xaiOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
        "--count" : rawCount : rest ->
            case parsePositiveIntOption "--count" rawCount of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpXAI
                Right imageCount ->
                    go commonState{parseImageCount = imageCount} xaiOptions rest
        "-n" : rawCount : rest ->
            case parsePositiveIntOption "--count" rawCount of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpXAI
                Right imageCount ->
                    go commonState{parseImageCount = imageCount} xaiOptions rest
        "--model" : modelName : rest ->
            go commonState xaiOptions{xaiModel = modelName} rest
        "--image" : imageSource : rest ->
            go commonState (appendXAIInputImageSource xaiOptions imageSource) rest
        "--aspect-ratio" : aspectRatioValue : rest ->
            go commonState xaiOptions{xaiAspectRatio = Just aspectRatioValue} rest
        "--resolution" : resolutionValue : rest ->
            go commonState xaiOptions{xaiResolution = Just resolutionValue} rest
        "--response-format" : responseFormatValue : rest ->
            go commonState xaiOptions{xaiResponseFormat = responseFormatValue} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} xaiOptions rest
            | Just rawCount <- T.stripPrefix "--count=" arg ->
                case parsePositiveIntOption "--count" rawCount of
                    Left err ->
                        ParseGenImageArgsError err GenImageHelpXAI
                    Right imageCount ->
                        go commonState{parseImageCount = imageCount} xaiOptions rest
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState xaiOptions{xaiModel = modelName} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState (appendXAIInputImageSource xaiOptions imageSource) rest
            | Just aspectRatioValue <- T.stripPrefix "--aspect-ratio=" arg ->
                go commonState xaiOptions{xaiAspectRatio = Just aspectRatioValue} rest
            | Just resolutionValue <- T.stripPrefix "--resolution=" arg ->
                go commonState xaiOptions{xaiResolution = Just resolutionValue} rest
            | Just responseFormatValue <- T.stripPrefix "--response-format=" arg ->
                go commonState xaiOptions{xaiResponseFormat = responseFormatValue} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenImageArgsError ("Unknown xai option: " <> arg) GenImageHelpXAI
            | otherwise ->
                go (appendPromptParts commonState [arg]) xaiOptions rest

    finalize CommonParseState{parseOutputPath, parseImageCount, parsePromptParts} xaiOptions
        | null parsePromptParts =
            ParseGenImageArgsError "A prompt is required." GenImageHelpXAI
        | otherwise =
            ParseGenImageArgsSuccess $
                GenImageXAI
                    xaiOptions
                        { xaiCommonOptions =
                            CommonGenImageOptions
                                { commonPromptText = T.unwords parsePromptParts
                                , commonOutputPath = parseOutputPath
                                , commonImageCount = parseImageCount
                                }
                        }

parseBanana2Args :: [Text] -> ParseGenImageArgsResult
parseBanana2Args =
    go defaultCommonParseState defaultBanana2GenImageOptions
  where
    go commonState banana2Options = \case
        [] ->
            finalize commonState banana2Options
        "--help" : _ ->
            ParseGenImageArgsHelp GenImageHelpBanana2
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                banana2Options
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} banana2Options rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} banana2Options rest
        "--count" : _ : _ ->
            ParseGenImageArgsError "banana2 does not support --count." GenImageHelpBanana2
        "-n" : _ : _ ->
            ParseGenImageArgsError "banana2 does not support --count." GenImageHelpBanana2
        "--model" : modelName : rest ->
            go commonState banana2Options{banana2Model = modelName} rest
        "--image" : imageSource : rest ->
            go commonState (appendBanana2InputImageSource banana2Options imageSource) rest
        "--aspect-ratio" : aspectRatioValue : rest ->
            go commonState banana2Options{banana2AspectRatio = Just aspectRatioValue} rest
        "--image-size" : imageSizeValue : rest ->
            go commonState banana2Options{banana2ImageSize = Just imageSizeValue} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} banana2Options rest
            | "--count=" `T.isPrefixOf` arg ->
                ParseGenImageArgsError "banana2 does not support --count." GenImageHelpBanana2
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState banana2Options{banana2Model = modelName} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState (appendBanana2InputImageSource banana2Options imageSource) rest
            | Just aspectRatioValue <- T.stripPrefix "--aspect-ratio=" arg ->
                go commonState banana2Options{banana2AspectRatio = Just aspectRatioValue} rest
            | Just imageSizeValue <- T.stripPrefix "--image-size=" arg ->
                go commonState banana2Options{banana2ImageSize = Just imageSizeValue} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenImageArgsError ("Unknown banana2 option: " <> arg) GenImageHelpBanana2
            | otherwise ->
                go (appendPromptParts commonState [arg]) banana2Options rest

    finalize CommonParseState{parseOutputPath, parseImageCount, parsePromptParts} banana2Options
        | null parsePromptParts =
            ParseGenImageArgsError "A prompt is required." GenImageHelpBanana2
        | otherwise =
            ParseGenImageArgsSuccess $
                GenImageBanana2
                    banana2Options
                        { banana2CommonOptions =
                            CommonGenImageOptions
                                { commonPromptText = T.unwords parsePromptParts
                                , commonOutputPath = parseOutputPath
                                , commonImageCount = parseImageCount
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

parseBoundedIntOption :: Text -> Int -> Int -> Text -> Either Text Int
parseBoundedIntOption optionName minValue maxValue rawValue =
    case readMaybe @Int (toString rawValue) of
        Nothing ->
            Left ("Invalid value for " <> optionName <> ": " <> rawValue)
        Just value
            | value < minValue || value > maxValue ->
                Left (optionName <> " must be between " <> toText (show minValue :: String) <> " and " <> toText (show maxValue :: String))
            | otherwise ->
                Right value

appendPromptParts :: CommonParseState -> [Text] -> CommonParseState
appendPromptParts commonState@CommonParseState{parsePromptParts} extraPromptParts =
    commonState{parsePromptParts = parsePromptParts <> extraPromptParts}

appendOpenAIInputImageSource :: OpenAIGenImageOptions -> Text -> OpenAIGenImageOptions
appendOpenAIInputImageSource options@OpenAIGenImageOptions{openAIInputImageSources} imageSource =
    options{openAIInputImageSources = openAIInputImageSources <> [imageSource]}

appendOpenAIInputFileId :: OpenAIGenImageOptions -> Text -> OpenAIGenImageOptions
appendOpenAIInputFileId options@OpenAIGenImageOptions{openAIInputFileIds} fileId =
    options{openAIInputFileIds = openAIInputFileIds <> [fileId]}

appendXAIInputImageSource :: XAIGenImageOptions -> Text -> XAIGenImageOptions
appendXAIInputImageSource options@XAIGenImageOptions{xaiInputImageSources} imageSource =
    options{xaiInputImageSources = xaiInputImageSources <> [imageSource]}

appendBanana2InputImageSource :: Banana2GenImageOptions -> Text -> Banana2GenImageOptions
appendBanana2InputImageSource options@Banana2GenImageOptions{banana2InputImageSources} imageSource =
    options{banana2InputImageSources = banana2InputImageSources <> [imageSource]}

hasOpenAIMaskConflict :: OpenAIGenImageOptions -> Bool
hasOpenAIMaskConflict OpenAIGenImageOptions{openAIMaskSource, openAIMaskFileId} =
    isJust openAIMaskSource && isJust openAIMaskFileId

renderGenImageHelp :: String -> GenImageHelpTopic -> Text
renderGenImageHelp progName = \case
    GenImageHelpGeneral ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " gptimage [OPTIONS] PROMPT"
            , "  " <> toText progName <> " xai [OPTIONS] PROMPT"
            , "  " <> toText progName <> " banana2 [OPTIONS] PROMPT"
            , ""
            , "Commands:"
            , "  gptimage  Generate images with OpenAI. Default model: gpt-image-1.5"
            , "  xai       Generate images with xAI Grok Imagine. Default model: grok-imagine-image"
            , "  banana2   Generate images with Gemini Nano Banana 2. Default model: gemini-2.5-flash-image"
            , ""
            , "Common options:"
            ]
                <> baseCommonOptionLines
                <> [ ""
                   , "gptimage options:"
                   ]
                <> countOptionLines
                <> openAIOptionLines
                <> [ ""
                   , "xai options:"
                   ]
                <> countOptionLines
                <> xaiOptionLines
                <> [ ""
                   , "banana2 options:"
                   ]
                <> banana2OptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  Use `" <> toText progName <> " gptimage --help`, `" <> toText progName <> " xai --help`, or `" <> toText progName <> " banana2 --help` for focused help."
                   , "  OPENAI_API_KEY is required for `gptimage`."
                   , "  XAI_API_KEY is required for `xai`."
                   , "  GEMINI_API_KEY is required for `banana2`."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " xai \"a man riding a horse on the moon\""
                   , "  " <> toText progName <> " gptimage --size=1536x1024 --quality=high \"a glass terrarium city\""
                   , "  " <> toText progName <> " banana2 --aspect-ratio=1:1 \"a tiny blue square on white background\""
                   ]
    GenImageHelpOpenAI ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " gptimage [OPTIONS] PROMPT"
            , ""
            , "Default model: gpt-image-1.5"
            , ""
            , "Common options:"
            ]
                <> baseCommonOptionLines
                <> [ ""
                   , "gptimage options:"
                   ]
                <> countOptionLines
                <> openAIOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  `--image-file-id` and `--mask-file-id` are for OpenAI uploaded file ids."
                   , "  OPENAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " gptimage \"a ceramic mug on linen\""
                   , "  " <> toText progName <> " gptimage --size=1536x1024 --quality=high \"a brutalist library in fog\""
                   , "  " <> toText progName <> " gptimage --image=base.png --mask=mask.png \"add a moon\""
                   ]
    GenImageHelpXAI ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " xai [OPTIONS] PROMPT"
            , ""
            , "Default model: grok-imagine-image"
            , ""
            , "Common options:"
            ]
                <> baseCommonOptionLines
                <> [ ""
                   , "xai options:"
                   ]
                <> countOptionLines
                <> xaiOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  XAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " xai \"a man riding a horse on the moon\""
                   , "  " <> toText progName <> " xai --aspect-ratio=16:9 --resolution=2k \"a floating city at sunrise\""
                   , "  " <> toText progName <> " xai --image=base.png \"turn this into a watercolor postcard\""
                   ]
    GenImageHelpBanana2 ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " banana2 [OPTIONS] PROMPT"
            , ""
            , "Default model: gemini-2.5-flash-image"
            , ""
            , "Common options:"
            ]
                <> baseCommonOptionLines
                <> [ ""
                   , "banana2 options:"
                   ]
                <> banana2OptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  GEMINI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " banana2 \"a cinematic portrait of a robot gardener\""
                   , "  " <> toText progName <> " banana2 --aspect-ratio=16:9 --image-size=2K \"a floating city at sunrise\""
                   , "  " <> toText progName <> " banana2 --image=base.png \"turn this into a watercolor postcard\""
                   ]
  where
    baseCommonOptionLines =
        [ "  -o, --output PATH            Output file path or filename prefix."
        , "  --help                       Show help."
        ]

    countOptionLines =
        [ "  -n, --count N                Number of images to request. Default: 1"
        ]

    openAIOptionLines =
        [ "  --model MODEL                Override the OpenAI image model."
        , "  --size SIZE                  Image size, for example 1024x1024."
        , "  --quality QUALITY            Image quality, for example low, medium, or high."
        , "  --output-format FORMAT       Output format. Default: png"
        , "  --output-compression N       Compression level from 0 to 100."
        , "  --background MODE            Background mode, for example transparent."
        , "  --moderation MODE            Moderation level."
        , "  --user USER_ID               User identifier for provider-side tracking."
        , "  --image SOURCE               Repeatable. Add input image URLs or local files."
        , "  --image-file-id FILE_ID      Repeatable. Add OpenAI uploaded file ids."
        , "  --mask SOURCE                Optional mask URL or local file."
        , "  --mask-file-id FILE_ID       Optional mask file id."
        , "  --input-fidelity LEVEL       Fidelity mode for edit inputs."
        ]

    xaiOptionLines =
        [ "  --model MODEL                Override the xAI image model."
        , "  --image SOURCE               Repeatable. Add input image URLs or local files."
        , "  --aspect-ratio RATIO         Aspect ratio, for example 16:9."
        , "  --resolution RESOLUTION      Resolution hint, for example 2k."
        , "  --response-format FORMAT     Response format. Default: b64_json"
        ]

    banana2OptionLines =
        [ "  --model MODEL                Override the Banana2 image model."
        , "  --image SOURCE               Repeatable. Add input image URLs or local files."
        , "  --aspect-ratio RATIO         Aspect ratio hint, for example 1:1 or 16:9."
        , "  --image-size SIZE            Image size hint, for example 1K or 2K."
        ]

slugifyPrompt :: Text -> FilePath
slugifyPrompt =
    slugifyPromptWithFallback "image"

saveGeneratedImages :: Maybe FilePath -> Text -> [GeneratedImage] -> IO (Either Text [FilePath])
saveGeneratedImages maybeOutput promptText images = do
    fetchedImages <- traverse fetchGeneratedImage images
    case lefts fetchedImages of
        firstErr : _ ->
            pure (Left firstErr)
        [] -> do
            let imageFiles = rights fetchedImages
            outputPaths <-
                buildOutputPaths
                    "image"
                    maybeOutput
                    promptText
                    [suggestedExtension | GeneratedFile{suggestedExtension} <- imageFiles]
            traverse_
                (\(path, GeneratedFile{fileBytes}) -> writeBinaryFile path fileBytes)
                (zip outputPaths imageFiles)
            pure (Right outputPaths)

data GeneratedFile = GeneratedFile
    { fileBytes :: BS.ByteString
    , suggestedExtension :: String
    }

fetchGeneratedImage :: GeneratedImage -> IO (Either Text GeneratedFile)
fetchGeneratedImage GeneratedImage{url = maybeUrl, b64Json = maybeB64Json} =
    case maybeB64Json of
        Just encodedBytes ->
            pure $
                case Base64.decode (TextEncoding.encodeUtf8 encodedBytes) of
                    Left err ->
                        Left ("Failed to decode base64 image data: " <> toText err)
                    Right decodedBytes ->
                        Right
                            GeneratedFile
                                { fileBytes = decodedBytes
                                , suggestedExtension = fromMaybe ".png" (detectImageExtension decodedBytes)
                                }
        Nothing ->
            case maybeUrl of
                Nothing ->
                    pure (Left "Image payload has neither b64_json nor url")
                Just imageUrl -> do
                    downloadResult <- downloadBinary imageUrl
                    pure $
                        second
                            ( \downloadedBytes ->
                                GeneratedFile
                                    { fileBytes = downloadedBytes
                                    , suggestedExtension =
                                        fromMaybe
                                            (fromMaybe ".png" (urlExtension imageUrl))
                                            (detectImageExtension downloadedBytes)
                                    }
                            )
                            downloadResult

detectImageExtension :: BS.ByteString -> Maybe String
detectImageExtension bytes =
    mimeTypeToExtension =<< detectImageMimeType bytes

detectImageMimeType :: BS.ByteString -> Maybe Text
detectImageMimeType bytes
    | pngSignature `BS.isPrefixOf` bytes =
        Just "image/png"
    | jpegSignature `BS.isPrefixOf` bytes =
        Just "image/jpeg"
    | gif87aSignature `BS.isPrefixOf` bytes || gif89aSignature `BS.isPrefixOf` bytes =
        Just "image/gif"
    | riffSignature `BS.isPrefixOf` bytes && webpSignature `BS.isInfixOf` BS.take 16 bytes =
        Just "image/webp"
    | otherwise =
        Nothing
  where
    pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]
    jpegSignature = BS.pack [255, 216, 255]
    gif87aSignature = TextEncoding.encodeUtf8 "GIF87a"
    gif89aSignature = TextEncoding.encodeUtf8 "GIF89a"
    riffSignature = TextEncoding.encodeUtf8 "RIFF"
    webpSignature = TextEncoding.encodeUtf8 "WEBP"

mimeTypeToExtension :: Text -> Maybe String
mimeTypeToExtension = \case
    "image/png" ->
        Just ".png"
    "image/jpeg" ->
        Just ".jpg"
    "image/gif" ->
        Just ".gif"
    "image/webp" ->
        Just ".webp"
    _ ->
        Nothing
