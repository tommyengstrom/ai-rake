{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.Videos
    ( OpenRouterVideoModel (..)
    , CustomVideoModel (..)
    , VideoResolution (..)
    , VideoRatio (..)
    , GrokImagineVideo15 (..)
    , GrokImagineVideo15Lite (..)
    , Veo31 (..)
    , defaultGrokImagineVideo15
    , defaultVeo31
    , VideoRequest (..)
    , defaultVideoRequest
    , VideoStatus (..)
    , VideoJob (..)
    , VideoPollSettings (..)
    , defaultVideoPollSettings
    , renderVideoRequest
    , startOpenRouterVideo
    , getOpenRouterVideo
    , waitOpenRouterVideo
    , generateOpenRouterVideo
    , downloadOpenRouterVideo
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Error.Static
import Rake.Effect
import Rake.Providers.OpenRouter.Internal.Request
import Rake.Providers.OpenRouter.Settings
import Relude

class OpenRouterVideoModel model where
    videoModelName :: model -> Text
    videoModelConfig :: model -> Object
    validateVideoModel :: model -> VideoRequest -> Either RakeError ()
    validateVideoModel _ _ = Right ()

data CustomVideoModel = CustomVideoModel
    { modelName :: Text
    , settings :: Object
    }
    deriving stock (Show, Eq)

instance OpenRouterVideoModel CustomVideoModel where
    videoModelName CustomVideoModel{modelName} = modelName
    videoModelConfig CustomVideoModel{settings} = settings

data VideoResolution = Video480p | Video720p | Video1080p | Video4K
    deriving stock (Show, Eq, Enum, Bounded, Generic)

instance ToJSON VideoResolution where
    toJSON = genericToJSON defaultOptions{constructorTagModifier = drop 5}

instance FromJSON VideoResolution where
    parseJSON = genericParseJSON defaultOptions{constructorTagModifier = drop 5}

data VideoRatio = Landscape | Portrait | Square | Ratio4x3 | Ratio3x4 | Ratio3x2 | Ratio2x3
    deriving stock (Show, Eq, Enum, Bounded, Generic)

videoRatioOptions :: Options
videoRatioOptions = defaultOptions{constructorTagModifier = tag}
  where
    tag :: String -> String
    tag = \case
        "Landscape" -> "16:9"
        "Portrait" -> "9:16"
        "Square" -> "1:1"
        "Ratio4x3" -> "4:3"
        "Ratio3x4" -> "3:4"
        "Ratio3x2" -> "3:2"
        "Ratio2x3" -> "2:3"
        other -> other

instance ToJSON VideoRatio where
    toJSON = genericToJSON videoRatioOptions

instance FromJSON VideoRatio where
    parseJSON = genericParseJSON videoRatioOptions

data GrokImagineVideo15 = GrokImagineVideo15
    { duration :: Int
    , resolution :: VideoResolution
    , ratio :: VideoRatio
    }
    deriving stock (Show, Eq)

defaultGrokImagineVideo15 :: GrokImagineVideo15
defaultGrokImagineVideo15 = GrokImagineVideo15 8 Video480p Landscape

instance OpenRouterVideoModel GrokImagineVideo15 where
    videoModelName _ = "x-ai/grok-imagine-video-1.5"
    videoModelConfig GrokImagineVideo15{duration, resolution, ratio} =
        KM.fromList ["duration" .= duration, "resolution" .= resolution, "aspect_ratio" .= ratio]
    validateVideoModel GrokImagineVideo15{duration, resolution} VideoRequest{lastFrame} = do
        unless (duration >= 1 && duration <= 15)
            $ Left (LlmExpectationError "Grok video duration must be 1–15 seconds")
        when (resolution == Video4K) $ Left (LlmExpectationError "Grok video does not support 4K")
        when (isJust lastFrame)
            $ Left (LlmExpectationError "OpenRouter's Grok video route does not support a last frame")

newtype GrokImagineVideo15Lite = GrokImagineVideo15Lite GrokImagineVideo15
    deriving stock (Show, Eq)

instance OpenRouterVideoModel GrokImagineVideo15Lite where
    videoModelName _ = "x-ai/grok-imagine-video-1.5-lite"
    videoModelConfig (GrokImagineVideo15Lite settings) = videoModelConfig settings
    validateVideoModel (GrokImagineVideo15Lite settings) = validateVideoModel settings

data Veo31 = Veo31
    { duration :: Int
    , resolution :: VideoResolution
    , ratio :: VideoRatio
    , generateAudio :: Bool
    , seed :: Maybe Int
    }
    deriving stock (Show, Eq)

defaultVeo31 :: Veo31
defaultVeo31 = Veo31 8 Video720p Landscape True Nothing

instance OpenRouterVideoModel Veo31 where
    videoModelName _ = "google/veo-3.1"
    videoModelConfig Veo31{duration, resolution, ratio, generateAudio, seed} =
        KM.fromList
            $ [ "duration" .= duration
              , "resolution" .= resolution
              , "aspect_ratio" .= ratio
              , "generate_audio" .= generateAudio
              ]
            <> catMaybes [("seed" .=) <$> seed]
    validateVideoModel Veo31{duration, resolution, ratio} _ = do
        unless (duration `elem` [4, 6, 8])
            $ Left (LlmExpectationError "Veo duration must be 4, 6 or 8 seconds")
        when (resolution == Video480p) $ Left (LlmExpectationError "Veo does not support 480p")
        unless (ratio `elem` [Landscape, Portrait])
            $ Left (LlmExpectationError "Veo requires 16:9 or 9:16")

data VideoRequest = VideoRequest
    { prompt :: Text
    , firstFrame :: Maybe Text
    , lastFrame :: Maybe Text
    }
    deriving stock (Show, Eq)

defaultVideoRequest :: Text -> VideoRequest
defaultVideoRequest prompt = VideoRequest prompt Nothing Nothing

renderVideoRequest
    :: OpenRouterVideoModel model
    => OpenRouterSettings es -> model -> VideoRequest -> Either RakeError Value
renderVideoRequest settings model request@VideoRequest{prompt, firstFrame, lastFrame} = do
    validateVideoModel model request
    when (T.null (T.strip prompt))
        $ Left (LlmExpectationError "Video prompt must not be empty")
    when (isJust lastFrame && isNothing firstFrame)
        $ Left (LlmExpectationError "A last frame requires a first frame")
    config <- requireBuffered (videoModelConfig model)
    let frames :: [Value]
        frames = catMaybes [frame "first_frame" <$> firstFrame, frame "last_frame" <$> lastFrame]
    assembleRequest
        settings
        (videoModelName model)
        config
        ["prompt", "frame_images", "stream"]
        $ KM.fromList
        $ ["prompt" .= prompt]
        <> ["frame_images" .= frames | not (null frames)]
  where
    frame :: Text -> Text -> Value
    frame kind url =
        object
            [ "type" .= ("image_url" :: Text)
            , "frame_type" .= kind
            , "image_url" .= object ["url" .= url]
            ]

data VideoStatus
    = Pending
    | InProgress
    | Completed
    | Failed
    | Cancelled
    | Expired
    | UnknownStatus Text
    deriving stock (Show, Eq)

instance FromJSON VideoStatus where
    parseJSON =
        withText "VideoStatus" $ pure . \case
            "pending" -> Pending
            "in_progress" -> InProgress
            "completed" -> Completed
            "failed" -> Failed
            "cancelled" -> Cancelled
            "expired" -> Expired
            other -> UnknownStatus other

instance ToJSON VideoStatus where
    toJSON =
        String . \case
            Pending -> "pending"
            InProgress -> "in_progress"
            Completed -> "completed"
            Failed -> "failed"
            Cancelled -> "cancelled"
            Expired -> "expired"
            UnknownStatus value -> value

data VideoJob = VideoJob
    { jobId :: Text
    , status :: VideoStatus
    , contentUrls :: Maybe [Text]
    , model :: Maybe Text
    , usage :: Maybe Object
    , failure :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)

videoJobOptions :: Options
videoJobOptions = defaultOptions{fieldLabelModifier = field, omitNothingFields = True}
  where
    field :: String -> String
    field = \case
        "jobId" -> "id"
        "contentUrls" -> "unsigned_urls"
        "failure" -> "error"
        other -> other

instance FromJSON VideoJob where
    parseJSON = genericParseJSON videoJobOptions

instance ToJSON VideoJob where
    toJSON = genericToJSON videoJobOptions

data VideoPollSettings = VideoPollSettings
    { intervalMilliseconds :: Int
    , maxAttempts :: Int
    }
    deriving stock (Show, Eq)

defaultVideoPollSettings :: VideoPollSettings
defaultVideoPollSettings = VideoPollSettings 5000 120

startOpenRouterVideo
    :: (OpenRouterVideoModel model, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> model -> VideoRequest -> Eff es VideoJob
startOpenRouterVideo settings model request = do
    body <- either throwError pure (renderVideoRequest settings model request)
    requestJson settings "POST" ["videos"] (Just body) >>= decodeJob

getOpenRouterVideo
    :: (IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> Text -> Eff es VideoJob
getOpenRouterVideo settings jobId = do
    when (T.null jobId) $ throwError (LlmExpectationError "Video job ID must not be empty")
    job@VideoJob{jobId = returnedId} <-
        requestJson settings "GET" ["videos", jobId] Nothing >>= decodeJob
    unless (returnedId == jobId)
        $ throwError (LlmInvalidResponseError "Video poll returned a different job ID")
    pure job

decodeJob :: Error RakeError :> es => Value -> Eff es VideoJob
decodeJob value = case fromJSON value of
    Error err -> throwError (LlmInvalidResponseError err)
    Success job@VideoJob{jobId} ->
        if T.null jobId
            then throwError (LlmInvalidResponseError "Video response contains an empty job ID")
            else pure job

waitOpenRouterVideo
    :: forall es
     . (Concurrent :> es, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> VideoPollSettings -> VideoJob -> Eff es VideoJob
waitOpenRouterVideo settings polling initial = withOpenRouterManager settings $ \client -> waitWithClient client polling initial

waitWithClient
    :: forall es
     . (Concurrent :> es, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> VideoPollSettings -> VideoJob -> Eff es VideoJob
waitWithClient settings VideoPollSettings{intervalMilliseconds, maxAttempts} initial = do
    either
        throwError
        pure
        (validateVideoPollSettings (VideoPollSettings intervalMilliseconds maxAttempts))
    loop maxAttempts initial
  where
    loop :: Int -> VideoJob -> Eff es VideoJob
    loop remaining job@VideoJob{jobId, status, failure} = case status of
        Completed -> pure job
        Pending -> poll remaining jobId
        InProgress -> poll remaining jobId
        Failed -> failed failure
        Cancelled -> failed failure
        Expired -> failed failure
        UnknownStatus value -> throwError (LlmInvalidResponseError ("Unknown video status: " <> toString value))
    poll :: Int -> Text -> Eff es VideoJob
    poll remaining jobId
        | remaining <= 0 =
            throwError
                (LlmTimeoutError (fromIntegral intervalMilliseconds * fromIntegral maxAttempts / 1000))
        | otherwise = do
            threadDelay (intervalMilliseconds * 1000)
            job <- getOpenRouterVideo settings jobId
            loop (remaining - 1) job
    failed :: Maybe Value -> Eff es VideoJob
    failed failure = throwError (LlmInvalidResponseError ("Video generation failed: " <> show failure))

validateVideoPollSettings :: VideoPollSettings -> Either RakeError ()
validateVideoPollSettings VideoPollSettings{intervalMilliseconds, maxAttempts} =
    when
        (maxAttempts <= 0 || intervalMilliseconds < 0 || intervalMilliseconds > maxBound `div` 1000)
        $ Left
            ( LlmExpectationError
                "Video polling needs positive attempts and a nonnegative bounded interval"
            )

generateOpenRouterVideo
    :: (OpenRouterVideoModel model, Concurrent :> es, IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> VideoPollSettings -> model -> VideoRequest -> Eff es VideoJob
generateOpenRouterVideo settings polling model request = do
    either throwError pure (validateVideoPollSettings polling)
    withOpenRouterManager settings $ \client ->
        startOpenRouterVideo client model request >>= waitWithClient client polling

downloadOpenRouterVideo
    :: (IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> VideoJob -> Int -> Eff es ByteString
downloadOpenRouterVideo settings VideoJob{jobId, status, contentUrls} index = do
    when (T.null jobId) $ throwError (LlmExpectationError "Video job ID must not be empty")
    unless (status == Completed)
        $ throwError (LlmExpectationError "Video is not ready for download")
    when (index < 0 || maybe False (index >=) (length <$> contentUrls))
        $ throwError (LlmExpectationError "Video output index is out of range")
    fst
        <$> requestBytesWithQuery
            settings
            "GET"
            ["videos", jobId, "content"]
            [("index", Just (Text.encodeUtf8 (show index)))]
            Nothing
