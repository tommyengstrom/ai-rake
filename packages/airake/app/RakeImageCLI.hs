{-# LANGUAGE NoOverloadedLists #-}

module RakeImageCLI
    ( parseGenImageArgs
    , renderGenImageHelp
    , runGenImageCli
    , imageModels
    , slugifyPrompt
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Rake.Error (renderRakeError)
import Rake.Media
import Rake.Providers.OpenRouter.Images
import Rake.Providers.OpenRouter.Settings
import RakeCliSupport
import RakeMediaOptions
import Relude

imageModels :: [(Text, Object)]
imageModels =
    [ entry defaultGrokImagineImage20
    , entry defaultGptImage25Flare
    , entry defaultGptImage25Sunburst
    , entry defaultGeminiNanoBanana21
    , entry defaultGeminiFlashLiteImage31
    , entry defaultGeminiProImage3
    ]
  where
    entry :: OpenRouterImageModel model => model -> (Text, Object)
    entry model = (imageModelName model, imageModelConfig model)

parseGenImageArgs :: [Text] -> Either Text MediaOptions
parseGenImageArgs args = do
    options@MediaOptions
        { selectedModel
        , modelSettings
        , rawSettings
        , imageSources
        , lastFrameSource
        , extendSource
        } <-
        parseMediaOptions "x-ai/grok-imagine-image-2.0" flags args >>= finishOptions imageModels
    when (isJust lastFrameSource || isJust extendSource)
        $ Left "Video frame/extension options are not supported for image generation"
    when (pollingSelected options)
        $ Left "Polling options are only supported for video generation"
    unless rawSettings $ validateKnown selectedModel modelSettings (length imageSources)
    void
        $ first renderRakeError
        $ renderImageRequest
            (defaultOpenRouterSettings "")
            (CustomImageModel selectedModel modelSettings)
            (ImageRequest (promptText options) imageSources)
    pure options
  where
    flags :: [(Text, Text, Text -> Either Text Value)]
    flags =
        [ (flag, key, Right . String)
        | (flag, key) <-
            [ ("--quality", "quality")
            , ("--aspect-ratio", "aspect_ratio")
            , ("--resolution", "resolution")
            , ("--image-size", "resolution")
            , ("--size", "size")
            , ("--output-format", "output_format")
            , ("--background", "background")
            ]
        ]
            <> [ (flag, key, integer)
               | (flag, key) <- [("--n", "n"), ("-n", "n"), ("--output-compression", "output_compression")]
               ]
    integer :: Text -> Either Text Value
    integer value = maybe (Left "Expected an integer") (Right . toJSON) (readMaybe @Int (toString value))

validateKnown :: Text -> Object -> Int -> Either Text ()
validateKnown model settings references
    | model `elem` ["openai/gpt-image-2.5-flare", "openai/gpt-image-2.5-sunburst"] = do
        rejectSettings
            [ "quality"
            , "aspect_ratio"
            , "size"
            , "n"
            , "output_format"
            , "background"
            , "output_compression"
            , "provider"
            ]
            settings
        enumSetting "quality" (map toJSON [minBound @GptImageQuality .. maxBound]) settings
        enumSetting "aspect_ratio" (map toJSON [minBound @GptImageRatio .. maxBound]) settings
        enumSetting "output_format" (map toJSON [minBound @RasterFormat .. maxBound]) settings
        enumSetting "background" (map toJSON [minBound @ImageBackground .. maxBound]) settings
        numericSetting "n" 1 10 settings
        numericSetting "output_compression" 0 100 settings
        forM_ (KM.lookup "size" settings) $ \case
            String value -> case traverse (readMaybe @Int . toString) (T.splitOn "x" value) of
                Just [width, height] | width > 0 && height > 0 -> Right ()
                _ -> Left "--size requires positive WIDTHxHEIGHT"
            _ -> Left "--size requires positive WIDTHxHEIGHT"
        limit 16
        when
            ( KM.lookup "background" settings
                == Just (String "transparent")
                && KM.lookup "output_format" settings
                == Just (String "jpeg")
            )
            $ Left "Transparency requires PNG or WebP"
        when (KM.member "size" settings && KM.member "aspect_ratio" settings)
            $ Left "Choose --size or --aspect-ratio"
    | model == "x-ai/grok-imagine-image-2.0" = do
        rejectSettings ["quality", "aspect_ratio", "resolution"] settings
        enumSetting "quality" (map toJSON [minBound @GrokImageQuality .. maxBound]) settings
        enumSetting "aspect_ratio" (map toJSON [minBound @GrokImageRatio .. maxBound]) settings
        enumSetting "resolution" (map toJSON [minBound @GrokImageResolution .. maxBound]) settings
        limit 3
    | model
        `elem` [ "google/gemini-nano-banana-2.1"
               , "google/gemini-3.1-flash-lite-image"
               , "google/gemini-3-pro-image"
               ] = do
        rejectSettings ["aspect_ratio", "resolution"] settings
        if model == "google/gemini-3-pro-image"
            then
                enumSetting "aspect_ratio" (map toJSON [minBound @GeminiImageRatio .. maxBound]) settings
            else
                enumSetting
                    "aspect_ratio"
                    (map toJSON [minBound @GeminiExtendedImageRatio .. maxBound])
                    settings
        enumSetting
            "resolution"
            ( if model == "google/gemini-3.1-flash-lite-image"
                then [String "1K"]
                else map toJSON [minBound @GeminiImageResolution .. maxBound]
            )
            settings
        limit 14
    | otherwise = Right ()
  where
    limit :: Int -> Either Text ()
    limit maximumReferences =
        when (references > maximumReferences)
            $ Left ("This model accepts at most " <> show maximumReferences <> " reference images")

renderGenImageHelp :: Text
renderGenImageHelp =
    T.unlines
        $ [ "Usage: rake-image [-m author/model] [OPTIONS] PROMPT"
          , "Uses OPENROUTER_API_KEY. Default: x-ai/grok-imagine-image-2.0, low quality."
          , "--quality VALUE   OpenAI: auto|low|medium|high|xhigh|max; Grok: low|medium"
          , "--image SOURCE    Repeatable file, URL or data URL for editing/reference images"
          , "--aspect-ratio RATIO  --resolution SIZE  --size WIDTHxHEIGHT"
          , "--n COUNT  --output-format png|jpeg|webp  --background auto|opaque|transparent"
          , "--output-compression 0..100  -o/--output PATH"
          , "--model-options JSON  Raw settings object for any model, including newly released models."
          , "Options accept both --flag VALUE and --flag=VALUE. Model-specific support varies."
          , "Known models:"
          ]
        <> map (("  " <>) . fst) imageModels

runGenImageCli :: IO ()
runGenImageCli = runMediaCli renderGenImageHelp parseGenImageArgs run
  where
    run :: Text -> MediaOptions -> IO (Either Text [FilePath])
    run apiKey MediaOptions{selectedModel, modelSettings, promptText, outputPath, imageSources} = do
        resolved <- traverse resolveMediaSource imageSources
        case sequence resolved of
            Left err -> pure (Left err)
            Right images -> do
                result <-
                    runProvider
                        $ generateOpenRouterImage
                            (defaultOpenRouterSettings apiKey)
                            (CustomImageModel selectedModel modelSettings)
                            (ImageRequest promptText images)
                case result of
                    Left err -> pure (Left err)
                    Right ImageGenerationResponse{images = generated} -> do
                        values <- traverse imageBytes generated
                        case sequence values of
                            Left err -> pure (Left err)
                            Right bytes -> Right <$> saveBytes "image" outputPath promptText bytes

imageBytes :: GeneratedImage -> IO (Either Text (String, BS.ByteString))
imageBytes GeneratedImage{url = Just source, b64Json = Nothing, revisedPrompt}
    | "data:" `T.isPrefixOf` source =
        resolveInlineImageSource source >>= \case
            Left err -> pure (Left err)
            Right InlineImage{base64Data, mimeType} -> imageBytes (GeneratedImage Nothing (Just base64Data) revisedPrompt (Just mimeType))
imageBytes GeneratedImage{url, b64Json, mimeType} = do
    bytes <- case b64Json of
        Just value -> pure (first toText (Base64.decode (Text.encodeUtf8 value)))
        Nothing -> maybe (pure (Left "Image result has no data")) downloadBinary url
    pure ((extension,) <$> bytes)
  where
    extension :: String
    extension = case mimeType of
        Just "image/jpeg" -> ".jpg"
        Just "image/webp" -> ".webp"
        Just "image/svg+xml" -> ".svg"
        Just "image/png" -> ".png"
        _ -> fromMaybe ".png" (url >>= urlExtension)

slugifyPrompt :: Text -> FilePath
slugifyPrompt = slugifyPromptWithFallback "image"
