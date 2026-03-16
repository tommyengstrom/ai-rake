module GenImageCLI
    ( GenImageProvider (..)
    , GenImageHelpTopic (..)
    , CommonGenImageOptions (..)
    , OpenAIGenImageOptions (..)
    , GrokGenImageOptions (..)
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
import GenCliSupport
import LlmChat
import LlmChat.Providers.OpenAI.Images
import LlmChat.Providers.XAI.Imagine
import Relude hiding (exitFailure, getArgs, lookupEnv)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.IO qualified as IO

data GenImageProvider
    = GenImageProviderOpenAI
    | GenImageProviderGrok
    deriving stock (Show, Eq)

data GenImageHelpTopic
    = GenImageHelpGeneral
    | GenImageHelpOpenAI
    | GenImageHelpGrok
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

data GrokGenImageOptions = GrokGenImageOptions
    { grokCommonOptions :: CommonGenImageOptions
    , grokModel :: Text
    , grokInputImageSources :: [Text]
    , grokAspectRatio :: Maybe Text
    , grokResolution :: Maybe Text
    , grokResponseFormat :: Text
    }
    deriving stock (Show, Eq)

data GenImageOptions
    = GenImageOpenAI OpenAIGenImageOptions
    | GenImageGrok GrokGenImageOptions
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

defaultGrokGenImageOptions :: GrokGenImageOptions
defaultGrokGenImageOptions =
    GrokGenImageOptions
        { grokCommonOptions =
            CommonGenImageOptions
                { commonPromptText = ""
                , commonOutputPath = Nothing
                , commonImageCount = 1
                }
        , grokModel = "grok-imagine-image"
        , grokInputImageSources = []
        , grokAspectRatio = Nothing
        , grokResolution = Nothing
        , grokResponseFormat = "b64_json"
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
                    pure (Left "OPENAI_API_KEY is required for OpenAI image generation")
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
    GenImageGrok GrokGenImageOptions
        { grokCommonOptions = CommonGenImageOptions{commonPromptText, commonOutputPath, commonImageCount}
        , grokModel
        , grokInputImageSources
        , grokAspectRatio
        , grokResolution
        , grokResponseFormat
        } -> do
            maybeApiKey <- lookupEnv "XAI_API_KEY"
            case maybeApiKey of
                Nothing ->
                    pure (Left "XAI_API_KEY is required for Grok image generation")
                Just apiKey -> do
                    resolvedInputImages <- traverse resolveMediaSource grokInputImageSources
                    case sequence resolvedInputImages of
                        Left err ->
                            pure (Left err)
                        Right imageUrls -> do
                            let settings = defaultXAIImagineSettings (toText apiKey)
                                request :: XAIImagineImageRequest
                                request =
                                    XAIImagineImageRequest
                                        { model = grokModel
                                        , prompt = commonPromptText
                                        , inputImages = imageUrls
                                        , n = Just commonImageCount
                                        , aspectRatio = grokAspectRatio
                                        , resolution = grokResolution
                                        , responseFormat = Just grokResponseFormat
                                        }
                            responseResult <- runProvider (generateXAIImage settings request)
                            handleImageResponse commonOutputPath commonPromptText responseResult
  where
    runProvider
        :: Eff '[Error LlmChatError, IOE] ImageGenerationResponse
        -> IO (Either Text ImageGenerationResponse)
    runProvider action = do
        result <-
            runEff
                . runErrorNoCallStack
                $ action
        pure (first renderLlmChatError result)

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
        ParseGenImageArgsError "A provider is required. Use `openai` or `grok`." GenImageHelpGeneral
    "--help" : _ ->
        ParseGenImageArgsHelp GenImageHelpGeneral
    providerArg : rest ->
        case parseProvider providerArg of
            Nothing ->
                ParseGenImageArgsError
                    ("Unknown provider: " <> providerArg <> ". Use `openai` or `grok`.")
                    GenImageHelpGeneral
            Just GenImageProviderOpenAI ->
                parseOpenAIArgs rest
            Just GenImageProviderGrok ->
                parseGrokArgs rest

parseProvider :: Text -> Maybe GenImageProvider
parseProvider = \case
    "openai" ->
        Just GenImageProviderOpenAI
    "grok" ->
        Just GenImageProviderGrok
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
                ParseGenImageArgsError ("Unknown OpenAI option: " <> arg) GenImageHelpOpenAI
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

parseGrokArgs :: [Text] -> ParseGenImageArgsResult
parseGrokArgs =
    go defaultCommonParseState defaultGrokGenImageOptions
  where
    go commonState grokOptions = \case
        [] ->
            finalize commonState grokOptions
        "--help" : _ ->
            ParseGenImageArgsHelp GenImageHelpGrok
        "--" : rest ->
            finalize
                (appendPromptParts commonState rest)
                grokOptions
        "--output" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} grokOptions rest
        "-o" : path : rest ->
            go commonState{parseOutputPath = Just (toString path)} grokOptions rest
        "--count" : rawCount : rest ->
            case parsePositiveIntOption "--count" rawCount of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpGrok
                Right imageCount ->
                    go commonState{parseImageCount = imageCount} grokOptions rest
        "-n" : rawCount : rest ->
            case parsePositiveIntOption "--count" rawCount of
                Left err ->
                    ParseGenImageArgsError err GenImageHelpGrok
                Right imageCount ->
                    go commonState{parseImageCount = imageCount} grokOptions rest
        "--model" : modelName : rest ->
            go commonState grokOptions{grokModel = modelName} rest
        "--image" : imageSource : rest ->
            go commonState (appendGrokInputImageSource grokOptions imageSource) rest
        "--aspect-ratio" : aspectRatioValue : rest ->
            go commonState grokOptions{grokAspectRatio = Just aspectRatioValue} rest
        "--resolution" : resolutionValue : rest ->
            go commonState grokOptions{grokResolution = Just resolutionValue} rest
        "--response-format" : responseFormatValue : rest ->
            go commonState grokOptions{grokResponseFormat = responseFormatValue} rest
        arg : rest
            | Just path <- T.stripPrefix "--output=" arg ->
                go commonState{parseOutputPath = Just (toString path)} grokOptions rest
            | Just rawCount <- T.stripPrefix "--count=" arg ->
                case parsePositiveIntOption "--count" rawCount of
                    Left err ->
                        ParseGenImageArgsError err GenImageHelpGrok
                    Right imageCount ->
                        go commonState{parseImageCount = imageCount} grokOptions rest
            | Just modelName <- T.stripPrefix "--model=" arg ->
                go commonState grokOptions{grokModel = modelName} rest
            | Just imageSource <- T.stripPrefix "--image=" arg ->
                go commonState (appendGrokInputImageSource grokOptions imageSource) rest
            | Just aspectRatioValue <- T.stripPrefix "--aspect-ratio=" arg ->
                go commonState grokOptions{grokAspectRatio = Just aspectRatioValue} rest
            | Just resolutionValue <- T.stripPrefix "--resolution=" arg ->
                go commonState grokOptions{grokResolution = Just resolutionValue} rest
            | Just responseFormatValue <- T.stripPrefix "--response-format=" arg ->
                go commonState grokOptions{grokResponseFormat = responseFormatValue} rest
            | "-" `T.isPrefixOf` arg ->
                ParseGenImageArgsError ("Unknown Grok option: " <> arg) GenImageHelpGrok
            | otherwise ->
                go (appendPromptParts commonState [arg]) grokOptions rest

    finalize CommonParseState{parseOutputPath, parseImageCount, parsePromptParts} grokOptions
        | null parsePromptParts =
            ParseGenImageArgsError "A prompt is required." GenImageHelpGrok
        | otherwise =
            ParseGenImageArgsSuccess $
                GenImageGrok
                    grokOptions
                        { grokCommonOptions =
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

appendGrokInputImageSource :: GrokGenImageOptions -> Text -> GrokGenImageOptions
appendGrokInputImageSource options@GrokGenImageOptions{grokInputImageSources} imageSource =
    options{grokInputImageSources = grokInputImageSources <> [imageSource]}

hasOpenAIMaskConflict :: OpenAIGenImageOptions -> Bool
hasOpenAIMaskConflict OpenAIGenImageOptions{openAIMaskSource, openAIMaskFileId} =
    isJust openAIMaskSource && isJust openAIMaskFileId

renderGenImageHelp :: String -> GenImageHelpTopic -> Text
renderGenImageHelp progName = \case
    GenImageHelpGeneral ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " openai [OPTIONS] PROMPT"
            , "  " <> toText progName <> " grok [OPTIONS] PROMPT"
            , ""
            , "Providers:"
            , "  openai  Generate images with OpenAI. Default model: gpt-image-1.5"
            , "  grok    Generate images with xAI Grok Imagine. Default model: grok-imagine-image"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "OpenAI-specific options:"
                   ]
                <> openAIOptionLines
                <> [ ""
                   , "Grok-specific options:"
                   ]
                <> grokOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  Use `" <> toText progName <> " openai --help` or `" <> toText progName <> " grok --help` for focused help."
                   , "  OPENAI_API_KEY is required for `openai`."
                   , "  XAI_API_KEY is required for `grok`."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " grok \"a man riding a horse on the moon\""
                   , "  " <> toText progName <> " openai --size=1536x1024 --quality=high \"a glass terrarium city\""
                   , "  " <> toText progName <> " grok --image=base.png \"turn this into watercolor\""
                   ]
    GenImageHelpOpenAI ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " openai [OPTIONS] PROMPT"
            , ""
            , "Default model: gpt-image-1.5"
            , ""
            , "Common options:"
            ]
                <> commonOptionLines
                <> [ ""
                   , "OpenAI options:"
                   ]
                <> openAIOptionLines
                <> [ ""
                   , "Notes:"
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  `--image-file-id` and `--mask-file-id` are for OpenAI uploaded file ids."
                   , "  OPENAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " openai \"a ceramic mug on linen\""
                   , "  " <> toText progName <> " openai --size=1536x1024 --quality=high \"a brutalist library in fog\""
                   , "  " <> toText progName <> " openai --image=base.png --mask=mask.png \"add a moon\""
                   ]
    GenImageHelpGrok ->
        T.unlines $
            [ "Usage:"
            , "  " <> toText progName <> " grok [OPTIONS] PROMPT"
            , ""
            , "Default model: grok-imagine-image"
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
                   , "  SOURCE can be a URL, a data URL, or a local image path."
                   , "  XAI_API_KEY is required."
                   , ""
                   , "Examples:"
                   , "  " <> toText progName <> " grok \"a man riding a horse on the moon\""
                   , "  " <> toText progName <> " grok --aspect-ratio=16:9 --resolution=2k \"a floating city at sunrise\""
                   , "  " <> toText progName <> " grok --image=base.png \"turn this into a watercolor postcard\""
                   ]
  where
    commonOptionLines =
        [ "  -o, --output PATH            Output file path or filename prefix."
        , "  -n, --count N                Number of images to request. Default: 1"
        , "  --help                       Show help."
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

    grokOptionLines =
        [ "  --model MODEL                Override the Grok image model."
        , "  --image SOURCE               Repeatable. Add input image URLs or local files."
        , "  --aspect-ratio RATIO         Aspect ratio, for example 16:9."
        , "  --resolution RESOLUTION      Resolution hint, for example 2k."
        , "  --response-format FORMAT     Response format. Default: b64_json"
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
