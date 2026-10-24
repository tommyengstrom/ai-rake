{-# LANGUAGE NoOverloadedLists #-}
module Rake.Providers.OpenRouter.Internal.Http
    ( providerClientError
    , defaultWarningLogger
    , ensureSuccessfulStreamingResponse
    , httpClientFailureRequest
    , protectStreamCallbacks
    , protectStreamingInternalAction
    , runChatProvider
    , runStreamingSseRequest
    , validateProviderRound
    , valueToCompactText
    ) where

import Data.Aeson (Value, decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types.Status (Status, statusCode, statusMessage, statusIsSuccessful)
import Network.HTTP.Types.URI (parseQuery)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Rake.Effect
import Rake.Backend
import Rake.Providers.OpenRouter.Internal.Sse (SseStep, consumeServerSentEvents)
import Relude
import Servant.Client (BaseUrl (..), ClientError (..), ResponseF (..))
import Servant.Client.Core.Request (RequestF (..))
import UnliftIO.Exception qualified as Exception

valueToCompactText :: Value -> Text
valueToCompactText =
    TL.toStrict . encodeToLazyText

newtype StreamingClientErrorWrapper = StreamingClientErrorWrapper ClientError
    deriving stock (Show)

instance Exception.Exception StreamingClientErrorWrapper

newtype StreamingRakeErrorWrapper = StreamingRakeErrorWrapper RakeError
    deriving stock (Show)

instance Exception.Exception StreamingRakeErrorWrapper

ensureSuccessfulStreamingResponse
    :: BaseUrl
    -> HttpClient.Request
    -> HttpClient.Response HttpClient.BodyReader
    -> IO ()
ensureSuccessfulStreamingResponse baseUrl request response =
    unless (statusIsSuccessful (HttpClient.responseStatus response)) do
        responseBody <- LBS.fromChunks <$> HttpClient.brConsume (HttpClient.responseBody response)
        let clientError =
                FailureResponse
                    (httpClientFailureRequest baseUrl request)
                    Response
                        { responseStatusCode = HttpClient.responseStatus response
                        , responseHeaders = fromList (HttpClient.responseHeaders response)
                        , responseHttpVersion = HttpClient.responseVersion response
                        , responseBody = responseBody
                        }
        Exception.throwIO (StreamingClientErrorWrapper clientError)

httpClientFailureRequest
    :: BaseUrl -> HttpClient.Request -> RequestF () (BaseUrl, BS.ByteString)
httpClientFailureRequest baseUrl request =
    Request
        { requestPath = (baseUrl, stripBaseUrlPath (baseUrlPath baseUrl) (HttpClient.path request))
        , requestQueryString = fromList (parseQuery (HttpClient.queryString request))
        , requestBody = Nothing
        , requestAccept = mempty
        , requestHeaders = fromList (HttpClient.requestHeaders request)
        , requestHttpVersion = HttpClient.requestVersion request
        , requestMethod = HttpClient.method request
        }
  where
    stripBaseUrlPath basePath rawPath
        | null basePath =
            rawPath
        | otherwise =
            case BS.stripPrefix (BC8.pack basePath) rawPath of
                Just strippedPath
                    | BS.null strippedPath || BS.head strippedPath == slashByte ->
                        strippedPath
                _ ->
                    rawPath

    slashByte = 47

runStreamingSseRequest
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => BaseUrl
    -> HttpClient.Manager
    -> HttpClient.Request
    -> (ClientError -> Eff es ())
    -> (Maybe Text -> BS.ByteString -> Eff es (SseStep a))
    -> Eff es (Maybe a)
runStreamingSseRequest parsedBaseUrl manager streamingRequest onClientError onEvent =
    withEffToIO SeqUnlift \runInIO ->
        Exception.tryAny
            ( HttpClient.withResponse streamingRequest manager \response -> do
                ensureSuccessfulStreamingResponse parsedBaseUrl streamingRequest response
                consumeServerSentEvents
                    (HttpClient.responseBody response)
                    ( \maybeEventName payload -> do
                        eventResult <-
                            runInIO
                                $ (Right <$> onEvent maybeEventName payload)
                                `catchError` (\_ rakeError -> pure (Left rakeError))
                        either throwStreamingRakeError pure eventResult
                    )
            )
            >>= \case
                Left err
                    | Just rakeError <- streamingRakeErrorFromException err ->
                        runInIO (throwError rakeError)
                Left err
                    | Just clientErr <- streamingClientErrorFromException err ->
                        runInIO do
                            onClientError clientErr
                            throwError (LlmClientError (providerClientError clientErr))
                Left err -> do
                    runInIO do
                        let clientErr = ConnectionError err
                        onClientError clientErr
                        throwError (LlmClientError (providerClientError clientErr))
                Right maybeFinalResponse ->
                    pure maybeFinalResponse

streamingClientErrorFromException :: SomeException -> Maybe ClientError
streamingClientErrorFromException someException =
    (\(StreamingClientErrorWrapper clientError) -> clientError)
        <$> Exception.fromException someException

throwStreamingRakeError :: RakeError -> IO a
throwStreamingRakeError rakeError =
    Exception.throwIO (StreamingRakeErrorWrapper rakeError)

streamingRakeErrorFromException :: SomeException -> Maybe RakeError
streamingRakeErrorFromException someException =
    (\(StreamingRakeErrorWrapper rakeError) -> rakeError)
        <$> Exception.fromException someException

providerClientError :: ClientError -> ProviderError
providerClientError err =
    ProviderError
        { errorStatus = responseStatus <$> errorResponse err
        , errorMessage = renderClientError err
        , errorBody = errorResponse err >>= decode . responseBody
        }
  where
    responseStatus :: ResponseF LBS.ByteString -> Int
    responseStatus = statusCode . responseStatusCode

errorResponse :: ClientError -> Maybe (ResponseF LBS.ByteString)
errorResponse = \case
    FailureResponse _ response -> Just response
    DecodeFailure _ response -> Just response
    UnsupportedContentType _ response -> Just response
    InvalidContentTypeHeader response -> Just response
    ConnectionError _ -> Nothing

renderClientError :: ClientError -> Text
renderClientError = \case
    FailureResponse _ response ->
        "Provider request failed (HTTP "
            <> renderStatus (responseStatusCode response)
            <> ")"
            <> renderResponseDetail response
    DecodeFailure err response ->
        "Provider returned an unreadable response (HTTP "
            <> renderStatus (responseStatusCode response)
            <> "): "
            <> err
    UnsupportedContentType mediaType response ->
        "Provider returned an unsupported content type (HTTP "
            <> renderStatus (responseStatusCode response)
            <> "): "
            <> show mediaType
    InvalidContentTypeHeader response ->
        "Provider returned an invalid content-type header (HTTP "
            <> renderStatus (responseStatusCode response)
            <> ")"
    ConnectionError err ->
        "Provider connection failed: " <> toText (displayException err)

renderResponseDetail :: ResponseF LBS.ByteString -> Text
renderResponseDetail response =
    maybe "" ((": " <>) . stripTrailingPeriod) (extractResponseDetail response)

extractResponseDetail :: ResponseF LBS.ByteString -> Maybe Text
extractResponseDetail response =
    extractJsonErrorDetail =<< decode @Value (responseBody response)


renderStatus :: Status -> Text
renderStatus status =
    show (statusCode status)
        <> case T.strip (TextEncoding.decodeUtf8 (statusMessage status)) of
            "" ->
                ""
            reasonPhrase ->
                " " <> reasonPhrase

