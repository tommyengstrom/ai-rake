module Rake.Media
    ( Audio (..)
    , GeneratedImage (..)
    , ImageGenerationResponse (..)
    , XAIVideoRequestId (..)
    , XAIVideoStatus (..)
    , GeneratedVideo (..)
    , XAIVideoStartResponse (..)
    , XAIVideoResponse (..)
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Relude

data Audio = Audio
    { audioBytes :: ByteString
    , mimeType :: Maybe Text
    , fileName :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

data GeneratedImage = GeneratedImage
    { url :: Maybe Text
    , b64Json :: Maybe Text
    , revisedPrompt :: Maybe Text
    , mimeType :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON GeneratedImage where
    parseJSON = withObject "GeneratedImage" $ \obj ->
        parseStandardImage obj <|> parseGeminiInlineImage obj
      where
        parseStandardImage :: Object -> Parser GeneratedImage
        parseStandardImage standardObj = do
            let parsedImage :: Parser GeneratedImage
                parsedImage =
                    GeneratedImage
                        <$> standardObj
                        .:? "url"
                        <*> standardObj
                        .:? "b64_json"
                        <*> standardObj
                        .:? "revised_prompt"
                        <*> standardObj
                        .:? "mime_type"
            generatedImage@GeneratedImage{url, b64Json, revisedPrompt} <- parsedImage
            when (isNothing url && isNothing b64Json && isNothing revisedPrompt)
                $ fail "Expected an OpenAI/xAI image payload"
            pure generatedImage

        parseGeminiInlineImage :: Object -> Parser GeneratedImage
        parseGeminiInlineImage geminiObj = do
            maybeInlineData <- (<|>) <$> geminiObj .:? "inlineData" <*> geminiObj .:? "inline_data"
            case maybeInlineData of
                Nothing ->
                    fail "Expected a Gemini inlineData image payload"
                Just inlineDataValue ->
                    withObject "GeminiInlineData" parseInlineData inlineDataValue

        parseInlineData :: Object -> Parser GeneratedImage
        parseInlineData inlineDataObj = do
            maybeB64Json <- inlineDataObj .:? "data"
            mimeType <- (<|>) <$> inlineDataObj .:? "mimeType" <*> inlineDataObj .:? "mime_type"
            case maybeB64Json of
                Nothing ->
                    fail "Gemini inlineData payload contained no data"
                Just b64Json ->
                    pure
                        GeneratedImage
                            { url = Nothing
                            , b64Json = Just b64Json
                            , revisedPrompt = Nothing
                            , mimeType
                            }

instance ToJSON GeneratedImage where
    toJSON GeneratedImage{url, b64Json, revisedPrompt, mimeType} =
        object
            $ catMaybes
                [ ("url" .=) <$> url
                , ("b64_json" .=) <$> b64Json
                , ("revised_prompt" .=) <$> revisedPrompt
                , ("mime_type" .=) <$> mimeType
                ]

data ImageGenerationResponse = ImageGenerationResponse
    { created :: Maybe Int
    , images :: [GeneratedImage]
    , model :: Maybe Text
    , outputFormat :: Maybe Text
    , size :: Maybe Text
    , quality :: Maybe Text
    , background :: Maybe Text
    , usage :: Maybe Object
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON ImageGenerationResponse where
    parseJSON = withObject "ImageGenerationResponse" $ \obj ->
        ImageGenerationResponse
            <$> obj
            .:? "created"
            <*> ( do
                    maybeImages <- obj .:? "data"
                    case maybeImages of
                        Just images ->
                            pure images
                        Nothing -> do
                            maybeCandidates <- obj .:? "candidates"
                            case maybeCandidates of
                                Nothing ->
                                    pure []
                                Just candidates ->
                                    pure (parseGeminiCandidates candidates)
                )
            <*> ((<|>) <$> obj .:? "model" <*> obj .:? "modelVersion")
            <*> obj
            .:? "output_format"
            <*> obj
            .:? "size"
            <*> obj
            .:? "quality"
            <*> obj
            .:? "background"
            <*> ((<|>) <$> obj .:? "usage" <*> obj .:? "usageMetadata")

instance ToJSON ImageGenerationResponse where
    toJSON ImageGenerationResponse
            { created
            , images
            , model
            , outputFormat
            , size
            , quality
            , background
            , usage
            } =
        object
            $ catMaybes
                [ ("created" .=) <$> created
                , Just ("data" .= images)
                , ("model" .=) <$> model
                , ("output_format" .=) <$> outputFormat
                , ("size" .=) <$> size
                , ("quality" .=) <$> quality
                , ("background" .=) <$> background
                , ("usage" .=) <$> usage
                ]

parseGeminiCandidates :: [Value] -> [GeneratedImage]
parseGeminiCandidates =
    concatMap candidateImages
  where
    candidateImages candidateValue =
        fromMaybe []
            $ parseMaybe
                (withObject "GeminiCandidate" parseCandidate)
                candidateValue

    parseCandidate candidateObj = do
        maybeContent <- candidateObj .:? "content"
        case maybeContent of
            Nothing ->
                pure []
            Just contentValue ->
                withObject "GeminiContent" parseContent contentValue

    parseContent contentObj = do
        parts <- contentObj .:? "parts" .!= []
        pure (mapMaybe (parseMaybe parseJSON) parts)

newtype XAIVideoRequestId = XAIVideoRequestId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON)

data XAIVideoStatus
    = XAIVideoPending
    | XAIVideoDone
    | XAIVideoExpired
    | XAIVideoFailed
    | XAIVideoUnknown Text
    deriving stock (Show, Eq, Generic)

instance FromJSON XAIVideoStatus where
    parseJSON = withText "XAIVideoStatus" $ \case
        "pending" -> pure XAIVideoPending
        "done" -> pure XAIVideoDone
        "expired" -> pure XAIVideoExpired
        "failed" -> pure XAIVideoFailed
        other -> pure (XAIVideoUnknown other)

instance ToJSON XAIVideoStatus where
    toJSON = \case
        XAIVideoPending ->
            String "pending"
        XAIVideoDone ->
            String "done"
        XAIVideoExpired ->
            String "expired"
        XAIVideoFailed ->
            String "failed"
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
            <$> obj
            .:? "url"
            <*> obj
            .:? "duration"
            <*> obj
            .:? "respect_moderation"

instance ToJSON GeneratedVideo where
    toJSON GeneratedVideo{url, duration, respectModeration} =
        object
            $ catMaybes
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
            <$> obj
            .: "status"
            <*> obj
            .:? "model"
            <*> obj
            .:? "video"

instance ToJSON XAIVideoResponse where
    toJSON XAIVideoResponse{status, model, video} =
        object
            $ ["status" .= status]
            <> catMaybes
                [ ("model" .=) <$> model
                , ("video" .=) <$> video
                ]
