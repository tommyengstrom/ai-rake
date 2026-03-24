module Rake.Providers.XAI.Imagine.Types
    ( XAIImagineSettings (..)
    , defaultXAIImagineSettings
    , XAIImagineImageRequest (..)
    , defaultXAIImagineImageRequest
    , XAIImagineVideoRequest (..)
    , defaultXAIImagineVideoRequest
    , validateXAIImagineVideoRequest
    ) where

import Data.Aeson
import Effectful
import Rake.Effect
import Rake.Providers.Internal (defaultWarningLogger)
import Relude

data XAIImagineSettings es = XAIImagineSettings
    { apiKey :: Text
    , baseUrl :: Text
    , pollIntervalMilliseconds :: Int
    , maxPollAttempts :: Int
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultXAIImagineSettings :: Text -> XAIImagineSettings es
defaultXAIImagineSettings apiKey =
    XAIImagineSettings
        { apiKey
        , baseUrl = "https://api.x.ai"
        , pollIntervalMilliseconds = 5000
        , maxPollAttempts = 120
        , requestLogger = defaultWarningLogger "xai.imagine"
        }

data XAIImagineImageRequest = XAIImagineImageRequest
    { model :: Text
    , prompt :: Text
    , inputImages :: [Text]
    , n :: Maybe Int
    , aspectRatio :: Maybe Text
    , resolution :: Maybe Text
    , responseFormat :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

defaultXAIImagineImageRequest :: Text -> XAIImagineImageRequest
defaultXAIImagineImageRequest prompt =
    XAIImagineImageRequest
        { model = "grok-imagine-image"
        , prompt
        , inputImages = []
        , n = Nothing
        , aspectRatio = Nothing
        , resolution = Nothing
        , responseFormat = Nothing
        }

instance ToJSON XAIImagineImageRequest where
    toJSON XAIImagineImageRequest{model, prompt, inputImages, n, aspectRatio, resolution, responseFormat} =
        object $
            [ "model" .= model
            , "prompt" .= prompt
            ]
                <> imageFields
                <> catMaybes
                    [ ("n" .=) <$> n
                    , ("aspect_ratio" .=) <$> aspectRatio
                    , ("resolution" .=) <$> resolution
                    , ("response_format" .=) <$> responseFormat
                    ]
      where
        imageFields = case inputImages of
            [] ->
                []
            [singleImage] ->
                ["image" .= xaiImageReferenceValue singleImage]
            manyImages ->
                ["images" .= map xaiImageReferenceValue manyImages]

data XAIImagineVideoRequest = XAIImagineVideoRequest
    { model :: Text
    , prompt :: Text
    , imageUrl :: Maybe Text
    , videoUrl :: Maybe Text
    , duration :: Maybe Int
    , aspectRatio :: Maybe Text
    , resolution :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

defaultXAIImagineVideoRequest :: Text -> XAIImagineVideoRequest
defaultXAIImagineVideoRequest prompt =
    XAIImagineVideoRequest
        { model = "grok-imagine-video"
        , prompt
        , imageUrl = Nothing
        , videoUrl = Nothing
        , duration = Nothing
        , aspectRatio = Nothing
        , resolution = Nothing
        }

instance ToJSON XAIImagineVideoRequest where
    toJSON XAIImagineVideoRequest{model, prompt, imageUrl, videoUrl, duration, aspectRatio, resolution} =
        object $
            [ "model" .= model
            , "prompt" .= prompt
            ]
                <> sourceField
                <> catMaybes
                    [ ("duration" .=) <$> duration
                    , ("aspect_ratio" .=) <$> aspectRatio
                    , ("resolution" .=) <$> resolution
                    ]
      where
        sourceField =
            case (imageUrl, videoUrl) of
                (Just sourceImage, Nothing) ->
                    ["image" .= object ["url" .= sourceImage]]
                (Nothing, Just sourceVideo) ->
                    ["video" .= object ["url" .= sourceVideo]]
                _ ->
                    []

validateXAIImagineVideoRequest :: XAIImagineVideoRequest -> Either RakeError ()
validateXAIImagineVideoRequest XAIImagineVideoRequest{imageUrl, videoUrl, duration, aspectRatio, resolution}
    | isJust imageUrl && isJust videoUrl =
        Left (LlmExpectationError "xAI video requests cannot set both imageUrl and videoUrl")
    | isJust videoUrl && (isJust duration || isJust aspectRatio || isJust resolution) =
        Left (LlmExpectationError "xAI video edit requests cannot set duration, aspectRatio, or resolution")
    | otherwise =
        Right ()

xaiImageReferenceValue :: Text -> Value
xaiImageReferenceValue imageUrl =
    object
        [ "url" .= imageUrl
        , "type" .= ("image_url" :: Text)
        ]
