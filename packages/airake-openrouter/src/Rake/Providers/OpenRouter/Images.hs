{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.Images
    ( module Rake.Providers.OpenRouter.ImageModels
    , ImageRequest (..)
    , defaultImageRequest
    , renderImageRequest
    , generateOpenRouterImage
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.Media
import Rake.Providers.OpenRouter.ImageModels
import Rake.Providers.OpenRouter.Internal.Request
import Rake.Providers.OpenRouter.Settings
import Relude

data ImageRequest = ImageRequest
    { prompt :: Text
    , inputImages :: [Text]
    }
    deriving stock (Show, Eq)

defaultImageRequest :: Text -> ImageRequest
defaultImageRequest prompt = ImageRequest prompt []

renderImageRequest
    :: OpenRouterImageModel model
    => OpenRouterSettings es -> model -> ImageRequest -> Either RakeError Value
renderImageRequest settings model ImageRequest{prompt, inputImages} = do
    validateImageModel model
    when (T.null (T.strip prompt))
        $ Left (LlmExpectationError "Image prompt must not be empty")
    forM_ (imageReferenceLimit model) $ \limit ->
        when (length inputImages > limit)
            $ Left (LlmExpectationError ("Too many reference images; model limit is " <> show limit))
    config <- requireBuffered (imageModelConfig model)
    assembleRequest
        settings
        (imageModelName model)
        config
        ["prompt", "input_references", "stream"]
        $ KM.fromList
        $ ["prompt" .= prompt]
        <> ["input_references" .= map reference inputImages | not (null inputImages)]
  where
    reference :: Text -> Value
    reference url = object ["type" .= ("image_url" :: Text), "image_url" .= object ["url" .= url]]

generateOpenRouterImage
    :: (OpenRouterImageModel model, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> model -> ImageRequest -> Eff es ImageGenerationResponse
generateOpenRouterImage settings model request = do
    body <- either throwError pure (renderImageRequest settings model request)
    value <- requestJson settings "POST" ["images"] (Just body)
    response@ImageGenerationResponse{images} <-
        case fromJSON value of
            Error err -> throwError (LlmInvalidResponseError err)
            Success response -> pure response
    when (null images)
        $ throwError (LlmInvalidResponseError "Image generation returned no images")
    forM_ images $ \GeneratedImage{url, b64Json} ->
        when (isNothing url && isNothing b64Json)
            $ throwError (LlmInvalidResponseError "Image result contains no image data")
    pure response
