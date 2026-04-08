module Rake.Providers.XAI.Imagine.Client
    ( generateXAIImage
    , startXAIVideo
    , getXAIVideo
    , generateXAIVideo
    ) where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Rake.Effect
import Rake.Media
import Rake.Providers.XAI.Imagine.Types
import Relude
import Servant.API (Capture, Get, Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

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
       , Error RakeError :> es
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
    decodeResponse "xAI image response" responseValue

startXAIVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> XAIImagineVideoRequest
    -> Eff es XAIVideoStartResponse
startXAIVideo settings request@XAIImagineVideoRequest{videoUrl} = do
    either throwError pure (validateXAIImagineVideoRequest request)
    let requestBody = toJSON request
    responseValue <-
        runVideoStartRequest
            settings
            requestBody
            (isJust videoUrl)
    decodeResponse "xAI video start response" responseValue

getXAIVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> XAIVideoRequestId
    -> Eff es XAIVideoResponse
getXAIVideo settings (XAIVideoRequestId requestIdText) = do
    let requestEnvelope =
            object
                [ "operation" .= ("get_xai_video" :: Text)
                , "request_id" .= requestIdText
                ]
    responseValue <- runVideoStatusRequest settings requestEnvelope requestIdText
    decodeResponse "xAI video response" responseValue

generateXAIVideo
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
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
            else do
                liftIO (threadDelay (pollIntervalMilliseconds * 1000))
                pollUntilReady (attempts + 1) requestId

runImageRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> Bool
    -> Eff es Value
runImageRequest settings@XAIImagineSettings{apiKey} requestBody useGenerationEndpoint = do
    let (postImageGeneration Servant.:<|> postImageEdit) Servant.:<|> _ = client xaiImagineApi
        clientCall
            | useGenerationEndpoint =
                postImageGeneration (Just ("Bearer " <> apiKey)) requestBody
            | otherwise =
                postImageEdit (Just ("Bearer " <> apiKey)) requestBody
    runClientCall settings requestBody clientCall

runVideoStartRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> Bool
    -> Eff es Value
runVideoStartRequest settings@XAIImagineSettings{apiKey} requestBody useEditEndpoint = do
    let _ Servant.:<|> (postVideoGeneration Servant.:<|> postVideoEdit Servant.:<|> _) = client xaiImagineApi
        clientCall
            | useEditEndpoint =
                postVideoEdit (Just ("Bearer " <> apiKey)) requestBody
            | otherwise =
                postVideoGeneration (Just ("Bearer " <> apiKey)) requestBody
    runClientCall settings requestBody clientCall

runVideoStatusRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> Text
    -> Eff es Value
runVideoStatusRequest settings@XAIImagineSettings{apiKey, requestLogger} requestEnvelope requestId = do
    let _ Servant.:<|> (_ Servant.:<|> _ Servant.:<|> getVideoStatus) = client xaiImagineApi
        clientCall =
            getVideoStatus requestId (Just ("Bearer " <> apiKey))
    runClientCallResult settings requestEnvelope clientCall >>= \case
        Left err@(FailureResponse _ response)
            | statusCode (responseStatusCode response) == 202
            , Just responseValue <- decode (responseBody response) -> do
                requestLogger (NativeMsgIn responseValue)
                pure responseValue
            | otherwise -> do
                requestLogger (NativeRequestFailure err)
                throwError (LlmClientError err)
        Left err -> do
            requestLogger (NativeRequestFailure err)
            throwError (LlmClientError err)
        Right responseValue -> do
            requestLogger (NativeMsgIn responseValue)
            pure responseValue

runClientCall
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> ClientM Value
    -> Eff es Value
runClientCall settings@XAIImagineSettings{requestLogger} requestEnvelope clientCall =
    runClientCallResult settings requestEnvelope clientCall >>= \case
        Left err -> do
            requestLogger (NativeRequestFailure err)
            throwError (LlmClientError err)
        Right responseValue -> do
            requestLogger (NativeMsgIn responseValue)
            pure responseValue

runClientCallResult
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => XAIImagineSettings es
    -> Value
    -> ClientM Value
    -> Eff es (Either ClientError Value)
runClientCallResult XAIImagineSettings{apiKey = _, baseUrl, requestLogger, pollIntervalMilliseconds = _, maxPollAttempts = _} requestEnvelope clientCall = do
    manager <- liftIO $ newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 180_000_000}
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl

    requestLogger (NativeMsgOut requestEnvelope)
    liftIO (runClientM clientCall clientEnv)
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

decodeResponse
    :: forall a es
     . ( Error RakeError :> es
       , FromJSON a
       )
    => String
    -> Value
    -> Eff es a
decodeResponse label responseValue =
    case fromJSON responseValue of
        Error err ->
            throwError (LlmExpectationError ("Failed to decode " <> label <> ": " <> err))
        Success response ->
            pure response

isTerminalStatus :: XAIVideoStatus -> Bool
isTerminalStatus = \case
    XAIVideoDone ->
        True
    XAIVideoExpired ->
        True
    XAIVideoFailed ->
        True
    XAIVideoPending ->
        False
    XAIVideoUnknown{} ->
        False
