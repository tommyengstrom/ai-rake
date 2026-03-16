module LlmChat.Providers.XAI.Imagine
    ( XAIImagineSettings (..)
    , defaultXAIImagineSettings
    , XAIImagineImageRequest (..)
    , defaultXAIImagineImageRequest
    , XAIImagineVideoRequest (..)
    , defaultXAIImagineVideoRequest
    , generateXAIImage
    , startXAIVideo
    , getXAIVideo
    , generateXAIVideo
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Media
import LlmChat.Providers.Responses.Internal (defaultWarningLogger)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Relude
import Servant.API (Capture, Get, Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

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
                <> configFields
      where
        sourceField =
            case (imageUrl, videoUrl) of
                (Just sourceImage, Nothing) ->
                    ["image" .= object ["url" .= sourceImage]]
                (Nothing, Just sourceVideo) ->
                    ["video" .= object ["url" .= sourceVideo]]
                _ ->
                    []

        configFields
            | isJust videoUrl =
                []
            | otherwise =
                catMaybes
                    [ ("duration" .=) <$> duration
                    , ("aspect_ratio" .=) <$> aspectRatio
                    , ("resolution" .=) <$> resolution
                    ]

type XAIImagineAPI =
    "v1"
        Servant.:> ( "images"
                        Servant.:> ( "generations"
                                        Servant.:> Header "Authorization" Text
                                        Servant.:> ReqBody '[JSON] Value
                                        Servant.:> Post '[JSON] Value
                                  Servant.:<|> "edits"
                                        Servant.:> Header "Authorization" Text
                                        Servant.:> ReqBody '[JSON] Value
                                        Servant.:> Post '[JSON] Value
                                  )
                  Servant.:<|> "videos"
                        Servant.:> ( "generations"
                                        Servant.:> Header "Authorization" Text
                                        Servant.:> ReqBody '[JSON] Value
                                        Servant.:> Post '[JSON] Value
                                  Servant.:<|> "edits"
                                        Servant.:> Header "Authorization" Text
                                        Servant.:> ReqBody '[JSON] Value
                                        Servant.:> Post '[JSON] Value
                                  Servant.:<|> Capture "request_id" Text
                                        Servant.:> Header "Authorization" Text
                                        Servant.:> Get '[JSON] Value
                                  )
                  )

xaiImagineApi :: Proxy XAIImagineAPI
xaiImagineApi = Proxy

generateXAIImage
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> XAIImagineImageRequest
    -> Eff es ImageGenerationResponse
generateXAIImage settings request@XAIImagineImageRequest{inputImages} = do
    let requestBody = toJSON request
    responseValue <-
        runImageRequest
            settings
            requestBody
            (null inputImages)
    decodeImageResponse responseValue

startXAIVideo
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> XAIImagineVideoRequest
    -> Eff es XAIVideoStartResponse
startXAIVideo settings request@XAIImagineVideoRequest{imageUrl, videoUrl} = do
    when (isJust imageUrl && isJust videoUrl) $
        throwError (LlmExpectationError "xAI video requests cannot set both imageUrl and videoUrl")

    let requestBody = toJSON request
    responseValue <-
        runVideoStartRequest
            settings
            requestBody
            (isJust videoUrl)
    decodeVideoStartResponse responseValue

getXAIVideo
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> XAIVideoRequestId
    -> Eff es XAIVideoResponse
getXAIVideo settings@(XAIImagineSettings{requestLogger}) requestId@(XAIVideoRequestId requestIdText) = do
    let requestEnvelope =
            object
                [ "operation" .= ("get_xai_video" :: Text)
                , "request_id" .= requestIdText
                ]

    requestLogger (NativeMsgOut requestEnvelope)
    responseValue <- runVideoStatusRequest settings requestId
    requestLogger (NativeMsgIn responseValue)
    decodeVideoResponse responseValue

generateXAIVideo
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> XAIImagineVideoRequest
    -> Eff es XAIVideoResponse
generateXAIVideo settings@XAIImagineSettings{pollIntervalMilliseconds, maxPollAttempts} request = do
    XAIVideoStartResponse{requestId} <- startXAIVideo settings request
    pollUntilReady 0 requestId
  where
    pollUntilReady attempts requestId = do
        when (attempts >= maxPollAttempts) $
            throwError (LlmExpectationError "xAI video generation exceeded the configured poll limit")
        response@XAIVideoResponse{status} <- getXAIVideo settings requestId
        if isTerminalStatus status
            then pure response
            else
                do
                    liftIO (threadDelay (pollIntervalMilliseconds * 1000))
                    pollUntilReady (attempts + 1) requestId

runImageRequest
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> Bool
    -> Eff es Value
runImageRequest XAIImagineSettings{apiKey, baseUrl, requestLogger} requestBody useGenerationEndpoint = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        (postImageGeneration Servant.:<|> postImageEdit) Servant.:<|> _ = client xaiImagineApi
        clientCall
            | useGenerationEndpoint =
                postImageGeneration (Just ("Bearer " <> apiKey)) requestBody
            | otherwise =
                postImageEdit (Just ("Bearer " <> apiKey)) requestBody

    requestLogger (NativeMsgOut requestBody)
    liftIO (runClientM clientCall clientEnv) >>= \case
        Left err -> do
            requestLogger (NativeRequestFailure err)
            throwError (LlmClientError err)
        Right responseValue -> do
            requestLogger (NativeMsgIn responseValue)
            pure responseValue
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

runVideoStartRequest
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> Bool
    -> Eff es Value
runVideoStartRequest XAIImagineSettings{apiKey, baseUrl, requestLogger} requestBody useEditEndpoint = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        _ Servant.:<|> (postVideoGeneration Servant.:<|> postVideoEdit Servant.:<|> _) = client xaiImagineApi
        clientCall
            | useEditEndpoint =
                postVideoEdit (Just ("Bearer " <> apiKey)) requestBody
            | otherwise =
                postVideoGeneration (Just ("Bearer " <> apiKey)) requestBody

    requestLogger (NativeMsgOut requestBody)
    liftIO (runClientM clientCall clientEnv) >>= \case
        Left err -> do
            requestLogger (NativeRequestFailure err)
            throwError (LlmClientError err)
        Right responseValue -> do
            requestLogger (NativeMsgIn responseValue)
            pure responseValue
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

decodeImageResponse
    :: Error LlmChatError :> es
    => Value
    -> Eff es ImageGenerationResponse
decodeImageResponse responseValue =
    case fromJSON responseValue of
        Error err ->
            throwError (LlmExpectationError ("Failed to decode xAI image response: " <> err))
        Success response ->
            pure response

decodeVideoStartResponse
    :: Error LlmChatError :> es
    => Value
    -> Eff es XAIVideoStartResponse
decodeVideoStartResponse responseValue =
    case fromJSON responseValue of
        Error err ->
            throwError (LlmExpectationError ("Failed to decode xAI video start response: " <> err))
        Success response ->
            pure response

decodeVideoResponse
    :: Error LlmChatError :> es
    => Value
    -> Eff es XAIVideoResponse
decodeVideoResponse responseValue =
    case fromJSON responseValue of
        Error err ->
            throwError (LlmExpectationError ("Failed to decode xAI video response: " <> err))
        Success response ->
            pure response

xaiImageReferenceValue :: Text -> Value
xaiImageReferenceValue imageUrl =
    object
        [ "url" .= imageUrl
        , "type" .= ("image_url" :: Text)
        ]

runVideoStatusRequest
    :: forall es
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIImagineSettings es
    -> XAIVideoRequestId
    -> Eff es Value
runVideoStatusRequest XAIImagineSettings{apiKey, baseUrl} (XAIVideoRequestId requestId) = do
    manager <- liftIO newTlsManager
    request <-
        liftIO (try @SomeException (HttpClient.parseRequest (toString (videoStatusUrl baseUrl requestId)))) >>= \case
            Left err ->
                throwError (LlmExpectationError ("Failed to prepare xAI video status request: " <> displayException err))
            Right request ->
                pure
                    request
                        { HttpClient.requestHeaders =
                            [ ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> apiKey))
                            , ("Accept", "application/json")
                            ]
                        }

    liftIO (try @SomeException (HttpClient.httpLbs request manager)) >>= \case
        Left err ->
            throwError (LlmExpectationError ("xAI video status request failed: " <> displayException err))
        Right response -> do
            responseValue <- decodeJsonBody "xAI video response" (HttpClient.responseBody response)
            let responseStatus = HttpClient.responseStatus response
            case statusCode responseStatus of
                200 ->
                    pure responseValue
                202 ->
                    pure responseValue
                httpStatusCode ->
                    throwError
                        ( LlmExpectationError
                            ( "xAI video status request failed (HTTP "
                                <> show httpStatusCode
                                <> ")"
                                <> toString (renderStatusResponseDetail responseValue)
                            )
                        )

videoStatusUrl :: Text -> Text -> Text
videoStatusUrl baseUrl requestId =
    T.dropWhileEnd (== '/') baseUrl <> "/v1/videos/" <> requestId

decodeJsonBody
    :: Error LlmChatError :> es
    => String
    -> LBS.ByteString
    -> Eff es Value
decodeJsonBody label responseBody =
    case eitherDecode responseBody of
        Left err ->
            throwError (LlmExpectationError ("Failed to decode " <> label <> ": " <> err))
        Right responseValue ->
            pure responseValue

renderStatusResponseDetail :: Value -> Text
renderStatusResponseDetail = \case
    Object obj ->
        maybe "" ((": " <>) . stripStatusTrailingPeriod) $
            combineStatusDetail
                (lookupStatusText "code" obj)
                ( lookupStatusText "error" obj
                    <|> lookupStatusText "message" obj
                    <|> lookupNestedStatusText "error" "message" obj
                )
    String textValue ->
        ": " <> stripStatusTrailingPeriod textValue
    _ ->
        ""

combineStatusDetail :: Maybe Text -> Maybe Text -> Maybe Text
combineStatusDetail maybeCode maybeMessage =
    case (maybeCode, maybeMessage) of
        (Just code, Just message)
            | code == message ->
                Just code
            | otherwise ->
                Just (code <> ": " <> message)
        (Just code, Nothing) ->
            Just code
        (Nothing, Just message) ->
            Just message
        (Nothing, Nothing) ->
            Nothing

lookupStatusText :: Text -> Object -> Maybe Text
lookupStatusText fieldName obj =
    KM.lookup (fromText fieldName) obj >>= \case
        String textValue ->
            Just textValue
        _ ->
            Nothing

lookupNestedStatusText :: Text -> Text -> Object -> Maybe Text
lookupNestedStatusText outerField innerField obj =
    KM.lookup (fromText outerField) obj >>= \case
        Object nestedObj ->
            lookupStatusText innerField nestedObj
        _ ->
            Nothing

stripStatusTrailingPeriod :: Text -> Text
stripStatusTrailingPeriod textValue =
    fromMaybe textValue (T.stripSuffix "." textValue)

isTerminalStatus :: XAIVideoStatus -> Bool
isTerminalStatus = \case
    XAIVideoDone ->
        True
    XAIVideoExpired ->
        True
    XAIVideoPending ->
        False
    XAIVideoUnknown{} ->
        False
