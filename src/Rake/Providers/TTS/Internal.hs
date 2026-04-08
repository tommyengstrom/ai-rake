module Rake.Providers.TTS.Internal
    ( BinaryAudioResponse (..)
    , postBinaryAudioRequest
    , streamBinaryAudioRequest
    ) where

import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client
    ( Manager
    , RequestBody (RequestBodyLBS)
    , brConsume
    , brReadSome
    , managerResponseTimeout
    , method
    , parseRequest
    , requestBody
    , requestHeaders
    , responseStatus
    , responseTimeout
    , responseTimeoutMicro
    )
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hContentDisposition, hContentType)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Network.HTTP.Types.URI (urlDecode)
import Rake.Effect (NativeMsgFormat (..))
import Rake.Error
import Rake.Providers.Internal (httpClientFailureRequest, protectStreamingInternalAction)
import Relude
import Servant.Client (BaseUrl, ClientError (..), ResponseF (..), parseBaseUrl)
import UnliftIO.Exception qualified as Exception

data BinaryAudioResponse = BinaryAudioResponse
    { responseBytes :: ByteString
    , responseMimeType :: Maybe Text
    , responseFileName :: Maybe Text
    }

data BinaryAudioRequestFailure
    = BinaryAudioClientError ClientError
    | BinaryAudioCallbackError RakeError

postBinaryAudioRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => (NativeMsgFormat -> Eff es ())
    -> Text
    -> Text
    -> [Maybe (HeaderName, ByteString)]
    -> Value
    -> Eff es BinaryAudioResponse
postBinaryAudioRequest requestLogger baseUrl path headers requestBodyValue =
    runBinaryAudioRequest Nothing requestLogger baseUrl path headers requestBodyValue

streamBinaryAudioRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => (ByteString -> Eff es ())
    -> (NativeMsgFormat -> Eff es ())
    -> Text
    -> Text
    -> [Maybe (HeaderName, ByteString)]
    -> Value
    -> Eff es BinaryAudioResponse
streamBinaryAudioRequest onChunk requestLogger baseUrl path headers requestBodyValue =
    runBinaryAudioRequest (Just onChunk) requestLogger baseUrl path headers requestBodyValue

runBinaryAudioRequest
    :: forall es
     . ( IOE :> es
       , Error RakeError :> es
       )
    => Maybe (ByteString -> Eff es ())
    -> (NativeMsgFormat -> Eff es ())
    -> Text
    -> Text
    -> [Maybe (HeaderName, ByteString)]
    -> Value
    -> Eff es BinaryAudioResponse
runBinaryAudioRequest maybeOnChunk requestLogger baseUrl path headers requestBodyValue = do
    manager <-
        liftIO
            $ newTlsManagerWith
                tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 180_000_000}
    parsedBaseUrl <-
        either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    rawRequest <- liftIO $ parseRequest (toString (joinBaseUrl baseUrl path))
    let request =
            rawRequest
                { method = "POST"
                , requestHeaders =
                    catMaybes headers
                        <> [ ("Content-Type", "application/json")
                           , ("Accept", "*/*")
                           ]
                , requestBody = RequestBodyLBS (encode requestBodyValue)
                , responseTimeout = responseTimeoutMicro 180_000_000
                }

    requestLogger (NativeMsgOut requestBodyValue)

    requestResult <-
        withEffToIO SeqUnlift \runInIO ->
            liftIO
                $ runBinaryAudioRequestIO
                    parsedBaseUrl
                    manager
                    request
                    ( \chunk ->
                        case maybeOnChunk of
                            Nothing ->
                                pure (Right ())
                            Just onChunk ->
                                runInIO
                                    $ ( (Right <$> protectStreamingInternalAction OnAudioChunkFailed (onChunk chunk))
                                            `catchError` (\_ rakeError -> pure (Left rakeError))
                                      )
                    )

    case requestResult of
        Left (BinaryAudioClientError clientError) -> do
            requestLogger (NativeRequestFailure clientError)
            throwError (LlmClientError clientError)
        Left (BinaryAudioCallbackError rakeError) ->
            throwError rakeError
        Right response -> do
            requestLogger (NativeConversionNote (binaryAudioResponseSummary response))
            pure response
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

runBinaryAudioRequestIO
    :: BaseUrl
    -> Manager
    -> HttpClient.Request
    -> (ByteString -> IO (Either RakeError ()))
    -> IO (Either BinaryAudioRequestFailure BinaryAudioResponse)
runBinaryAudioRequestIO parsedBaseUrl manager request invokeChunk =
    Exception.tryAny
        ( HttpClient.withResponse request manager \response ->
            if statusIsSuccessful (responseStatus response)
                then do
                    consumedChunks <- consumeResponseChunks invokeChunk (HttpClient.responseBody response)
                    pure
                        $ case consumedChunks of
                            Left rakeError ->
                                Left (BinaryAudioCallbackError rakeError)
                            Right chunks ->
                                Right
                                    BinaryAudioResponse
                                        { responseBytes = BS.concat chunks
                                        , responseMimeType = lookupResponseMimeType response
                                        , responseFileName = lookupResponseFileName response
                                        }
                else do
                    responseBodyBytes <- LBS.fromChunks <$> brConsume (HttpClient.responseBody response)
                    pure
                        $ Left
                            ( BinaryAudioClientError
                                ( FailureResponse
                                    (httpClientFailureRequest parsedBaseUrl request)
                                    Response
                                        { responseStatusCode = responseStatus response
                                        , responseHeaders = fromList (HttpClient.responseHeaders response)
                                        , responseHttpVersion = HttpClient.responseVersion response
                                        , responseBody = responseBodyBytes
                                        }
                                )
                            )
        )
        >>= \case
            Left err ->
                pure (Left (BinaryAudioClientError (ConnectionError err)))
            Right requestResult ->
                pure requestResult

consumeResponseChunks
    :: (ByteString -> IO (Either RakeError ()))
    -> HttpClient.BodyReader
    -> IO (Either RakeError [ByteString])
consumeResponseChunks invokeChunk bodyReader = go []
  where
    go chunks = do
        chunk <- brReadSome bodyReader 32_768
        let strictChunk = LBS.toStrict chunk
        if BS.null strictChunk
            then pure (Right (reverse chunks))
            else do
                chunkResult <- invokeChunk strictChunk
                case chunkResult of
                    Left rakeError ->
                        pure (Left rakeError)
                    Right () ->
                        go (strictChunk : chunks)

lookupResponseMimeType :: HttpClient.Response body -> Maybe Text
lookupResponseMimeType response =
    stripHeaderParameters
        . TextEncoding.decodeUtf8Lenient
        <$> List.lookup hContentType (HttpClient.responseHeaders response)

lookupResponseFileName :: HttpClient.Response body -> Maybe Text
lookupResponseFileName response =
    List.lookup hContentDisposition (HttpClient.responseHeaders response)
        >>= parseContentDispositionFileName

parseContentDispositionFileName :: ByteString -> Maybe Text
parseContentDispositionFileName rawHeader =
    parseExtendedFileName headerText <|> parseSimpleFileName headerText
  where
    headerText = TextEncoding.decodeUtf8Lenient rawHeader

    parseSimpleFileName =
        fmap stripWrappedQuotes
            . listToMaybe
            . mapMaybe (T.stripPrefix "filename=" . T.strip)
            . T.splitOn ";"

    parseExtendedFileName dispositionText = do
        encodedValue <-
            listToMaybe
                ( mapMaybe
                    (T.stripPrefix "filename*=" . T.strip)
                    (T.splitOn ";" dispositionText)
                )
        encodedFileName <-
            T.stripPrefix "UTF-8''" encodedValue <|> T.stripPrefix "utf-8''" encodedValue
        pure
            (TextEncoding.decodeUtf8Lenient (urlDecode True (TextEncoding.encodeUtf8 encodedFileName)))

stripWrappedQuotes :: Text -> Text
stripWrappedQuotes textValue =
    fromMaybe textValue (T.stripPrefix "\"" textValue >>= T.stripSuffix "\"")

stripHeaderParameters :: Text -> Text
stripHeaderParameters =
    T.strip . fst . T.breakOn ";"

joinBaseUrl :: Text -> Text -> Text
joinBaseUrl baseUrl path =
    fromMaybe baseUrl (T.stripSuffix "/" baseUrl) <> path

binaryAudioResponseSummary :: BinaryAudioResponse -> Text
binaryAudioResponseSummary BinaryAudioResponse{responseBytes, responseMimeType, responseFileName} =
    T.intercalate
        ", "
        ( [ "Received audio response"
          , "bytes=" <> show (BS.length responseBytes)
          ]
            <> maybe [] (\mimeTypeText -> ["content-type=" <> mimeTypeText]) responseMimeType
            <> maybe [] (\fileNameText -> ["file-name=" <> fileNameText]) responseFileName
        )
