{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.ImageModels
    ( OpenRouterImageModel (..)
    , CustomImageModel (..)
    , GptImageQuality (..)
    , GrokImageQuality (..)
    , RasterFormat (..)
    , ImageBackground (..)
    , ImageModeration (..)
    , GrokImageResolution (..)
    , GeminiImageResolution (..)
    , GptImageRatio (..)
    , GrokImageRatio (..)
    , GeminiImageRatio (..)
    , GeminiExtendedImageRatio (..)
    , ImageDimensions (..)
    , GptImage25Flare (..)
    , defaultGptImage25Flare
    , GptImage25Sunburst (..)
    , defaultGptImage25Sunburst
    , GrokImagineImage20 (..)
    , defaultGrokImagineImage20
    , GeminiNanoBanana21 (..)
    , defaultGeminiNanoBanana21
    , GeminiFlashLiteImage31 (..)
    , defaultGeminiFlashLiteImage31
    , GeminiProImage3 (..)
    , defaultGeminiProImage3
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Rake.Error
import Relude

class OpenRouterImageModel model where
    imageModelName :: model -> Text
    imageModelConfig :: model -> Object
    validateImageModel :: model -> Either RakeError ()
    validateImageModel _ = Right ()
    imageReferenceLimit :: model -> Maybe Int
    imageReferenceLimit _ = Nothing

data CustomImageModel = CustomImageModel
    { modelName :: Text
    , settings :: Object
    }
    deriving stock (Show, Eq)

instance OpenRouterImageModel CustomImageModel where
    imageModelName CustomImageModel{modelName} = modelName
    imageModelConfig CustomImageModel{settings} = settings

data GptImageQuality
    = GptQualityAuto
    | GptQualityLow
    | GptQualityMedium
    | GptQualityHigh
    | GptQualityXHigh
    | GptQualityMax
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GptImageQuality where
    toJSON = genericToJSON (gptImageQualityOptions)

instance FromJSON GptImageQuality where
    parseJSON = genericParseJSON (gptImageQualityOptions)

gptImageQualityOptions :: Options
gptImageQualityOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GptQualityAuto" -> "auto"
        "GptQualityLow" -> "low"
        "GptQualityMedium" -> "medium"
        "GptQualityHigh" -> "high"
        "GptQualityXHigh" -> "xhigh"
        "GptQualityMax" -> "max"
        other -> other

data GrokImageQuality
    = GrokQualityLow
    | GrokQualityMedium
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GrokImageQuality where
    toJSON = genericToJSON (grokImageQualityOptions)

instance FromJSON GrokImageQuality where
    parseJSON = genericParseJSON (grokImageQualityOptions)

grokImageQualityOptions :: Options
grokImageQualityOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GrokQualityLow" -> "low"
        "GrokQualityMedium" -> "medium"
        other -> other

data RasterFormat
    = FormatPng
    | FormatJpeg
    | FormatWebp
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON RasterFormat where
    toJSON = genericToJSON (rasterFormatOptions)

instance FromJSON RasterFormat where
    parseJSON = genericParseJSON (rasterFormatOptions)

rasterFormatOptions :: Options
rasterFormatOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "FormatPng" -> "png"
        "FormatJpeg" -> "jpeg"
        "FormatWebp" -> "webp"
        other -> other

data ImageBackground
    = BackgroundAuto
    | BackgroundTransparent
    | BackgroundOpaque
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ImageBackground where
    toJSON = genericToJSON (imageBackgroundOptions)

instance FromJSON ImageBackground where
    parseJSON = genericParseJSON (imageBackgroundOptions)

imageBackgroundOptions :: Options
imageBackgroundOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "BackgroundAuto" -> "auto"
        "BackgroundTransparent" -> "transparent"
        "BackgroundOpaque" -> "opaque"
        other -> other

data ImageModeration
    = ModerationAuto
    | ModerationLow
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ImageModeration where
    toJSON = genericToJSON (imageModerationOptions)

instance FromJSON ImageModeration where
    parseJSON = genericParseJSON (imageModerationOptions)

imageModerationOptions :: Options
imageModerationOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "ModerationAuto" -> "auto"
        "ModerationLow" -> "low"
        other -> other

data GrokImageResolution
    = GrokResolution1K
    | GrokResolution2K
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GrokImageResolution where
    toJSON = genericToJSON (grokImageResolutionOptions)

instance FromJSON GrokImageResolution where
    parseJSON = genericParseJSON (grokImageResolutionOptions)

grokImageResolutionOptions :: Options
grokImageResolutionOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GrokResolution1K" -> "1K"
        "GrokResolution2K" -> "2K"
        other -> other

data GeminiImageResolution
    = GeminiResolution1K
    | GeminiResolution2K
    | GeminiResolution4K
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GeminiImageResolution where
    toJSON = genericToJSON (geminiImageResolutionOptions)

instance FromJSON GeminiImageResolution where
    parseJSON = genericParseJSON (geminiImageResolutionOptions)

geminiImageResolutionOptions :: Options
geminiImageResolutionOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GeminiResolution1K" -> "1K"
        "GeminiResolution2K" -> "2K"
        "GeminiResolution4K" -> "4K"
        other -> other

data GptImageRatio
    = GptRatioAuto
    | GptRatio1x1
    | GptRatio3x2
    | GptRatio2x3
    | GptRatio4x3
    | GptRatio3x4
    | GptRatio16x9
    | GptRatio9x16
    | GptRatio21x9
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GptImageRatio where
    toJSON = genericToJSON (gptImageRatioOptions)

instance FromJSON GptImageRatio where
    parseJSON = genericParseJSON (gptImageRatioOptions)

gptImageRatioOptions :: Options
gptImageRatioOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GptRatioAuto" -> "auto"
        "GptRatio1x1" -> "1:1"
        "GptRatio3x2" -> "3:2"
        "GptRatio2x3" -> "2:3"
        "GptRatio4x3" -> "4:3"
        "GptRatio3x4" -> "3:4"
        "GptRatio16x9" -> "16:9"
        "GptRatio9x16" -> "9:16"
        "GptRatio21x9" -> "21:9"
        other -> other

data GrokImageRatio
    = GrokRatioAuto
    | GrokRatio1x1
    | GrokRatio3x4
    | GrokRatio4x3
    | GrokRatio9x16
    | GrokRatio16x9
    | GrokRatio2x3
    | GrokRatio3x2
    | GrokRatio9x19_5
    | GrokRatio19_5x9
    | GrokRatio9x20
    | GrokRatio20x9
    | GrokRatio1x2
    | GrokRatio2x1
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GrokImageRatio where
    toJSON = genericToJSON (grokImageRatioOptions)

instance FromJSON GrokImageRatio where
    parseJSON = genericParseJSON (grokImageRatioOptions)

grokImageRatioOptions :: Options
grokImageRatioOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GrokRatioAuto" -> "auto"
        "GrokRatio1x1" -> "1:1"
        "GrokRatio3x4" -> "3:4"
        "GrokRatio4x3" -> "4:3"
        "GrokRatio9x16" -> "9:16"
        "GrokRatio16x9" -> "16:9"
        "GrokRatio2x3" -> "2:3"
        "GrokRatio3x2" -> "3:2"
        "GrokRatio9x19_5" -> "9:19.5"
        "GrokRatio19_5x9" -> "19.5:9"
        "GrokRatio9x20" -> "9:20"
        "GrokRatio20x9" -> "20:9"
        "GrokRatio1x2" -> "1:2"
        "GrokRatio2x1" -> "2:1"
        other -> other

data GeminiImageRatio
    = GeminiRatio1x1
    | GeminiRatio2x3
    | GeminiRatio3x2
    | GeminiRatio3x4
    | GeminiRatio4x3
    | GeminiRatio4x5
    | GeminiRatio5x4
    | GeminiRatio9x16
    | GeminiRatio16x9
    | GeminiRatio21x9
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GeminiImageRatio where
    toJSON = genericToJSON (geminiImageRatioOptions)

instance FromJSON GeminiImageRatio where
    parseJSON = genericParseJSON (geminiImageRatioOptions)

geminiImageRatioOptions :: Options
geminiImageRatioOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GeminiRatio1x1" -> "1:1"
        "GeminiRatio2x3" -> "2:3"
        "GeminiRatio3x2" -> "3:2"
        "GeminiRatio3x4" -> "3:4"
        "GeminiRatio4x3" -> "4:3"
        "GeminiRatio4x5" -> "4:5"
        "GeminiRatio5x4" -> "5:4"
        "GeminiRatio9x16" -> "9:16"
        "GeminiRatio16x9" -> "16:9"
        "GeminiRatio21x9" -> "21:9"
        other -> other

data GeminiExtendedImageRatio
    = GeminiExtendedRatio1x1
    | GeminiExtendedRatio1x4
    | GeminiExtendedRatio1x8
    | GeminiExtendedRatio2x3
    | GeminiExtendedRatio3x2
    | GeminiExtendedRatio3x4
    | GeminiExtendedRatio4x1
    | GeminiExtendedRatio4x3
    | GeminiExtendedRatio4x5
    | GeminiExtendedRatio5x4
    | GeminiExtendedRatio8x1
    | GeminiExtendedRatio9x16
    | GeminiExtendedRatio16x9
    | GeminiExtendedRatio21x9
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON GeminiExtendedImageRatio where
    toJSON = genericToJSON (geminiExtendedImageRatioOptions)

instance FromJSON GeminiExtendedImageRatio where
    parseJSON = genericParseJSON (geminiExtendedImageRatioOptions)

geminiExtendedImageRatioOptions :: Options
geminiExtendedImageRatioOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "GeminiExtendedRatio1x1" -> "1:1"
        "GeminiExtendedRatio1x4" -> "1:4"
        "GeminiExtendedRatio1x8" -> "1:8"
        "GeminiExtendedRatio2x3" -> "2:3"
        "GeminiExtendedRatio3x2" -> "3:2"
        "GeminiExtendedRatio3x4" -> "3:4"
        "GeminiExtendedRatio4x1" -> "4:1"
        "GeminiExtendedRatio4x3" -> "4:3"
        "GeminiExtendedRatio4x5" -> "4:5"
        "GeminiExtendedRatio5x4" -> "5:4"
        "GeminiExtendedRatio8x1" -> "8:1"
        "GeminiExtendedRatio9x16" -> "9:16"
        "GeminiExtendedRatio16x9" -> "16:9"
        "GeminiExtendedRatio21x9" -> "21:9"
        other -> other

data ImageDimensions ratio
    = ImageAspectRatio ratio
    | ImagePixels Int Int
    deriving stock (Show, Eq)

dimensionFields :: ToJSON ratio => Maybe (ImageDimensions ratio) -> [Pair]
dimensionFields = \case
    Nothing -> []
    Just (ImageAspectRatio ratio) -> ["aspect_ratio" .= ratio]
    Just (ImagePixels width height) -> ["size" .= (show width <> "x" <> show height :: Text)]

validateDimensions :: Maybe (ImageDimensions ratio) -> Either RakeError ()
validateDimensions = \case
    Just (ImagePixels width height)
        | width <= 0 || height <= 0 ->
            Left (LlmExpectationError "Image dimensions must be positive")
    _ -> Right ()

data GptImage25Flare = GptImage25Flare
    { quality :: GptImageQuality
    , dimensions :: Maybe (ImageDimensions GptImageRatio)
    , count :: Int
    , outputFormat :: RasterFormat
    , background :: Maybe ImageBackground
    , outputCompression :: Maybe Int
    , moderation :: Maybe ImageModeration
    }
    deriving stock (Show, Eq)

defaultGptImage25Flare :: GptImage25Flare
defaultGptImage25Flare = GptImage25Flare GptQualityAuto Nothing 1 FormatPng Nothing Nothing (Just ModerationLow)

instance OpenRouterImageModel GptImage25Flare where
    imageModelName _ = "openai/gpt-image-2.5-flare"
    imageModelConfig GptImage25Flare
                        { quality
                        , dimensions
                        , count
                        , outputFormat
                        , background
                        , outputCompression
                        , moderation
                        } =
        KM.fromList
            $ ["quality" .= quality, "n" .= count, "output_format" .= outputFormat]
            <> dimensionFields dimensions
            <> catMaybes
                [ ("background" .=) <$> background
                , ("output_compression" .=) <$> outputCompression
                , ( \value ->
                        "provider" .= object ["options" .= object ["openai" .= object ["moderation" .= value]]]
                  )
                    <$> moderation
                ]
    imageReferenceLimit _ = Just 16
    validateImageModel GptImage25Flare{dimensions, count, outputFormat, background, outputCompression} = do
        validateDimensions dimensions
        unless (count >= 1 && count <= 10)
            $ Left (LlmExpectationError "Image count must be between 1 and 10")
        forM_ outputCompression $ \compression ->
            unless (compression >= 0 && compression <= 100)
                $ Left (LlmExpectationError "Output compression must be between 0 and 100")
        when (background == Just BackgroundTransparent && outputFormat == FormatJpeg)
            $ Left (LlmExpectationError "Transparent images require PNG or WebP")

data GptImage25Sunburst = GptImage25Sunburst
    { quality :: GptImageQuality
    , dimensions :: Maybe (ImageDimensions GptImageRatio)
    , count :: Int
    , outputFormat :: RasterFormat
    , background :: Maybe ImageBackground
    , outputCompression :: Maybe Int
    , moderation :: Maybe ImageModeration
    }
    deriving stock (Show, Eq)

defaultGptImage25Sunburst :: GptImage25Sunburst
defaultGptImage25Sunburst =
    GptImage25Sunburst GptQualityAuto Nothing 1 FormatPng Nothing Nothing (Just ModerationLow)

instance OpenRouterImageModel GptImage25Sunburst where
    imageModelName _ = "openai/gpt-image-2.5-sunburst"
    imageModelConfig GptImage25Sunburst
                        { quality
                        , dimensions
                        , count
                        , outputFormat
                        , background
                        , outputCompression
                        , moderation
                        } =
        KM.fromList
            $ ["quality" .= quality, "n" .= count, "output_format" .= outputFormat]
            <> dimensionFields dimensions
            <> catMaybes
                [ ("background" .=) <$> background
                , ("output_compression" .=) <$> outputCompression
                , ( \value ->
                        "provider" .= object ["options" .= object ["openai" .= object ["moderation" .= value]]]
                  )
                    <$> moderation
                ]
    imageReferenceLimit _ = Just 16
    validateImageModel GptImage25Sunburst{dimensions, count, outputFormat, background, outputCompression} = do
        validateDimensions dimensions
        unless (count >= 1 && count <= 10)
            $ Left (LlmExpectationError "Image count must be between 1 and 10")
        forM_ outputCompression $ \compression ->
            unless (compression >= 0 && compression <= 100)
                $ Left (LlmExpectationError "Output compression must be between 0 and 100")
        when (background == Just BackgroundTransparent && outputFormat == FormatJpeg)
            $ Left (LlmExpectationError "Transparent images require PNG or WebP")

data GrokImagineImage20 = GrokImagineImage20
    { quality :: GrokImageQuality
    , ratio :: Maybe GrokImageRatio
    , resolution :: Maybe GrokImageResolution
    }
    deriving stock (Show, Eq)

defaultGrokImagineImage20 :: GrokImagineImage20
defaultGrokImagineImage20 = GrokImagineImage20 GrokQualityLow Nothing Nothing

instance OpenRouterImageModel GrokImagineImage20 where
    imageModelName _ = "x-ai/grok-imagine-image-2.0"
    imageModelConfig GrokImagineImage20{quality, ratio, resolution} =
        KM.fromList
            $ ["quality" .= quality]
            <> catMaybes [("aspect_ratio" .=) <$> ratio, ("resolution" .=) <$> resolution]
    imageReferenceLimit _ = Just 3

data GeminiNanoBanana21 = GeminiNanoBanana21
    { ratio :: Maybe GeminiExtendedImageRatio
    , resolution :: Maybe GeminiImageResolution
    }
    deriving stock (Show, Eq)

defaultGeminiNanoBanana21 :: GeminiNanoBanana21
defaultGeminiNanoBanana21 = GeminiNanoBanana21 Nothing Nothing

instance OpenRouterImageModel GeminiNanoBanana21 where
    imageModelName _ = "google/gemini-nano-banana-2.1"
    imageModelConfig GeminiNanoBanana21{ratio, resolution} =
        KM.fromList $ catMaybes [("aspect_ratio" .=) <$> ratio, ("resolution" .=) <$> resolution]
    imageReferenceLimit _ = Just 14

data GeminiFlashLiteImage31 = GeminiFlashLiteImage31
    { ratio :: Maybe GeminiExtendedImageRatio
    }
    deriving stock (Show, Eq)

defaultGeminiFlashLiteImage31 :: GeminiFlashLiteImage31
defaultGeminiFlashLiteImage31 = GeminiFlashLiteImage31 Nothing

instance OpenRouterImageModel GeminiFlashLiteImage31 where
    imageModelName _ = "google/gemini-3.1-flash-lite-image"
    imageModelConfig GeminiFlashLiteImage31{ratio} =
        KM.fromList $ catMaybes [("aspect_ratio" .=) <$> ratio]
    imageReferenceLimit _ = Just 14

data GeminiProImage3 = GeminiProImage3
    { ratio :: Maybe GeminiImageRatio
    , resolution :: Maybe GeminiImageResolution
    }
    deriving stock (Show, Eq)

defaultGeminiProImage3 :: GeminiProImage3
defaultGeminiProImage3 = GeminiProImage3 Nothing Nothing

instance OpenRouterImageModel GeminiProImage3 where
    imageModelName _ = "google/gemini-3-pro-image"
    imageModelConfig GeminiProImage3{ratio, resolution} =
        KM.fromList $ catMaybes [("aspect_ratio" .=) <$> ratio, ("resolution" .=) <$> resolution]
    imageReferenceLimit _ = Just 14
