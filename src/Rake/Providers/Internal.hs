module Rake.Providers.Internal
    ( defaultWarningLogger
    , ensureSuccessfulStreamingResponse
    , httpClientFailureRequest
    , protectStreamCallbacks
    , protectStreamingInternalAction
    , runChatProvider
    , runStreamingSseRequest
    , valueToCompactText
    ) where

import Data.Aeson (Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy qualified as TL
import Debug.Trace qualified as DebugTrace
import Effectful
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Error.Static
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types.Status (statusIsSuccessful)
import Network.HTTP.Types.URI (parseQuery)
import Rake.Effect
import Rake.Internal.Sse (SseStep, consumeServerSentEvents)
import Rake.Types
import Relude
import Servant.Client (BaseUrl (..), ClientError (..), ResponseF (..))
import Servant.Client.Core.Request (RequestF (..))
import UnliftIO.Exception qualified as Exception

runChatProvider
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => ( [ToolDeclaration]
         -> ResponseFormat
         -> SamplingOptions
         -> [HistoryItem]
         -> Eff es ProviderRound
       )
    -> ( StreamCallbacks es
         -> [ToolDeclaration]
         -> ResponseFormat
         -> SamplingOptions
         -> [HistoryItem]
         -> Eff es ProviderRound
       )
    -> Eff (Rake ': es) a
    -> Eff es a
runChatProvider runRound runStreamingRound eff =
    interpretWith eff \localEnv -> \case
        GetLlmResponse tools responseFormat samplingOptions history ->
            runRound tools responseFormat samplingOptions history
        GetLlmResponseStream
            onAssistantTextDelta
            onAssistantRefusalDelta
            tools
            responseFormat
            samplingOptions
            history ->
            localSeqUnlift localEnv \unlift ->
                runStreamingRound
                    ( protectStreamCallbacks
                        StreamCallbacks
                            { onAssistantTextDelta =
                                unlift . onAssistantTextDelta
                            , onAssistantRefusalDelta =
                                unlift . onAssistantRefusalDelta
                            }
                    )
                    tools
                    responseFormat
                    samplingOptions
                    history

defaultWarningLogger :: Applicative f => Text -> NativeMsgFormat -> f ()
defaultWarningLogger providerName = \case
    NativeConversionNote note ->
        logWarning (warningPrefix <> toString note)
    NativeRequestFailure err ->
        logWarning (warningPrefix <> toString (renderClientError err))
    NativeMsgOut{} ->
        pure ()
    NativeMsgIn{} ->
        pure ()
  where
    warningPrefix = "[ai-rake:" <> toString providerName <> "] "
    logWarning warningMessage =
        DebugTrace.trace warningMessage () `seq` pure ()

valueToCompactText :: Value -> Text
valueToCompactText =
    TL.toStrict . encodeToLazyText

protectStreamCallbacks
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => StreamCallbacks es
    -> StreamCallbacks es
protectStreamCallbacks StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} =
    StreamCallbacks
        { onAssistantTextDelta =
            \deltaText ->
                protectStreamingInternalAction
                    OnAssistantTextDeltaFailed
                    (onAssistantTextDelta deltaText)
        , onAssistantRefusalDelta =
            \refusalText ->
                protectStreamingInternalAction
                    OnAssistantRefusalDeltaFailed
                    (onAssistantRefusalDelta refusalText)
        }

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

protectStreamingInternalAction
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => (String -> StreamingInternalIssue)
    -> Eff es a
    -> Eff es a
protectStreamingInternalAction wrapIssue action = do
    result <-
        withEffToIO SeqUnlift \runInIO ->
            Exception.tryAny
                $ runInIO
                    ( (Right <$> action)
                        `catchError` (\_ (rakeError :: RakeError) -> pure (Left rakeError))
                    )
    case result of
        Left err ->
            throwError (StreamingInternalError (wrapIssue (displayException err)))
        Right (Left rakeError) ->
            throwError @RakeError rakeError
        Right (Right value) ->
            pure value

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
                            throwError (LlmClientError clientErr)
                Left err -> do
                    runInIO do
                        let clientErr = ConnectionError err
                        onClientError clientErr
                        throwError (LlmClientError clientErr)
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
