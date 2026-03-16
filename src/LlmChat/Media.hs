module LlmChat.Media
    ( GeneratedImage (..)
    , ImageGenerationResponse (..)
    , XAIVideoRequestId (..)
    , XAIVideoStatus (..)
    , GeneratedVideo (..)
    , XAIVideoStartResponse (..)
    , XAIVideoResponse (..)
    ) where

import Data.Aeson
import Relude

data GeneratedImage = GeneratedImage
    { url :: Maybe Text
    , b64Json :: Maybe Text
    , revisedPrompt :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeneratedImage where
    parseJSON = withObject "GeneratedImage" $ \obj ->
        GeneratedImage
            <$> obj .:? "url"
            <*> obj .:? "b64_json"
            <*> obj .:? "revised_prompt"

instance ToJSON GeneratedImage where
    toJSON GeneratedImage{url, b64Json, revisedPrompt} =
        object $
            catMaybes
                [ ("url" .=) <$> url
                , ("b64_json" .=) <$> b64Json
                , ("revised_prompt" .=) <$> revisedPrompt
                ]

data ImageGenerationResponse = ImageGenerationResponse
    { created :: Maybe Int
    , images :: [GeneratedImage]
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON ImageGenerationResponse where
    parseJSON = withObject "ImageGenerationResponse" $ \obj ->
        ImageGenerationResponse
            <$> obj .:? "created"
            <*> obj .:? "data" .!= []

instance ToJSON ImageGenerationResponse where
    toJSON ImageGenerationResponse{created, images} =
        object $
            catMaybes
                [ ("created" .=) <$> created
                , Just ("data" .= images)
                ]

newtype XAIVideoRequestId = XAIVideoRequestId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON)

data XAIVideoStatus
    = XAIVideoPending
    | XAIVideoDone
    | XAIVideoExpired
    | XAIVideoUnknown Text
    deriving stock (Show, Eq, Generic)

instance FromJSON XAIVideoStatus where
    parseJSON = withText "XAIVideoStatus" $ \case
        "pending" -> pure XAIVideoPending
        "done" -> pure XAIVideoDone
        "expired" -> pure XAIVideoExpired
        other -> pure (XAIVideoUnknown other)

instance ToJSON XAIVideoStatus where
    toJSON = \case
        XAIVideoPending ->
            String "pending"
        XAIVideoDone ->
            String "done"
        XAIVideoExpired ->
            String "expired"
        XAIVideoUnknown other ->
            String other

data GeneratedVideo = GeneratedVideo
    { url :: Maybe Text
    , duration :: Maybe Int
    , respectModeration :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeneratedVideo where
    parseJSON = withObject "GeneratedVideo" $ \obj ->
        GeneratedVideo
            <$> obj .:? "url"
            <*> obj .:? "duration"
            <*> obj .:? "respect_moderation"

instance ToJSON GeneratedVideo where
    toJSON GeneratedVideo{url, duration, respectModeration} =
        object $
            catMaybes
                [ ("url" .=) <$> url
                , ("duration" .=) <$> duration
                , ("respect_moderation" .=) <$> respectModeration
                ]

newtype XAIVideoStartResponse = XAIVideoStartResponse
    { requestId :: XAIVideoRequestId
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON XAIVideoStartResponse where
    parseJSON = withObject "XAIVideoStartResponse" $ \obj ->
        XAIVideoStartResponse <$> obj .: "request_id"

instance ToJSON XAIVideoStartResponse where
    toJSON XAIVideoStartResponse{requestId} =
        object ["request_id" .= requestId]

data XAIVideoResponse = XAIVideoResponse
    { status :: XAIVideoStatus
    , model :: Maybe Text
    , video :: Maybe GeneratedVideo
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON XAIVideoResponse where
    parseJSON = withObject "XAIVideoResponse" $ \obj ->
        XAIVideoResponse
            <$> obj .: "status"
            <*> obj .:? "model"
            <*> obj .:? "video"

instance ToJSON XAIVideoResponse where
    toJSON XAIVideoResponse{status, model, video} =
        object $
            ["status" .= status]
                <> catMaybes
                    [ ("model" .=) <$> model
                    , ("video" .=) <$> video
                    ]
