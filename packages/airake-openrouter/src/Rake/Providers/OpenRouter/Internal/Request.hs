{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.Internal.Request
    ( requestJson
    , requestBytes
    , requestBytesWithQuery
    , requireBuffered
    ) where

import Control.Exception (try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Effectful
import Effectful.Error.Static
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Query, hContentType, renderQuery, statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Rake.Effect
import Rake.Providers.OpenRouter.Settings
import Relude

requestJson
    :: (IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es -> ByteString -> [Text] -> Maybe Value -> Eff es Value
requestJson settings@OpenRouterSettings{requestLogger} method path body = do
    (bytes, _) <- requestBytes settings method path body
    value <- either (throwError . LlmInvalidResponseError) pure (eitherDecodeStrict' bytes)
    requestLogger (NativeMsgIn value)
    case value of
        Object fields
            | Just err <- KM.lookup "error" fields
            , err /= Null
            , not (KM.member "status" fields) ->
                throwError
                    ( LlmInvalidResponseError
                        (toString (fromMaybe "OpenRouter returned an error" (extractJsonErrorDetail value)))
                    )
        _ -> pure value

requestBytes
    :: (IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es
    -> ByteString
    -> [Text]
    -> Maybe Value
    -> Eff es (ByteString, Maybe Text)
requestBytes settings method path = requestBytesWithQuery settings method path []

requestBytesWithQuery
    :: forall es
     . (IOE :> es, Error RakeError :> es)
    => OpenRouterSettings es
    -> ByteString
    -> [Text]
    -> Query
    -> Maybe Value
    -> Eff es (ByteString, Maybe Text)
requestBytesWithQuery
    OpenRouterSettings
        { apiKey
        , baseUrl
        , requestLogger
        , requestTimeoutMilliseconds
        , httpManager
        }
    method
    path
    query
    body = do
        when (requestTimeoutMilliseconds <= 0 || requestTimeoutMilliseconds > maxBound `div` 1000)
            $ throwError
                (LlmExpectationError "Request timeout must be positive and fit in microseconds")
        traverse_ (requestLogger . NativeMsgOut) body
        result <- liftIO $ try @HTTP.HttpException do
            manager <-
                maybe
                    (HTTP.newManager tlsManagerSettings{HTTP.managerRetryableException = const False})
                    pure
                    httpManager
            request <-
                HTTP.parseRequest
                    ( toString
                        (T.dropWhileEnd (== '/') baseUrl <> "/" <> T.intercalate "/" (map encodeSegment path))
                    )
            HTTP.httpLbs
                request
                    { HTTP.method = method
                    , HTTP.queryString = renderQuery True query
                    , HTTP.requestHeaders =
                        [ ("Authorization", "Bearer " <> Text.encodeUtf8 apiKey)
                        , (hContentType, "application/json")
                        ]
                    , HTTP.requestBody = maybe (HTTP.RequestBodyBS "") (HTTP.RequestBodyLBS . encode) body
                    , HTTP.responseTimeout = HTTP.responseTimeoutMicro (requestTimeoutMilliseconds * 1000)
                    , HTTP.redirectCount = 0
                    , HTTP.checkResponse = \_ _ -> pure ()
                    }
                manager
        case result of
            Left _ -> failRequest (ProviderError Nothing "OpenRouter connection failed" Nothing)
            Right response -> do
                let status :: Int
                    status = statusCode (HTTP.responseStatus response)
                    bytes :: LBS.ByteString
                    bytes = HTTP.responseBody response
                if status >= 200 && status < 300
                    then
                        pure
                            ( LBS.toStrict bytes
                            , lookup hContentType (HTTP.responseHeaders response)
                                >>= either (const Nothing) Just
                                . Text.decodeUtf8'
                            )
                    else
                        failRequest
                            ProviderError
                                { errorStatus = Just status
                                , errorMessage =
                                    "OpenRouter request failed (HTTP "
                                        <> show status
                                        <> ")"
                                        <> maybe "" (": " <>) (decode bytes >>= extractJsonErrorDetail)
                                , errorBody = decode bytes
                                }
      where
        encodeSegment :: Text -> Text
        encodeSegment = Text.decodeUtf8 . urlEncode False . Text.encodeUtf8
        failRequest :: ProviderError -> Eff es a
        failRequest err = requestLogger (NativeRequestFailure err) >> throwError (LlmClientError err)

requireBuffered :: Object -> Either RakeError Object
requireBuffered settings = case KM.lookup "stream" settings of
    Nothing -> Right settings
    Just (Bool False) -> Right (KM.delete "stream" settings)
    Just (Bool True) ->
        Left
            ( LlmExpectationError
                "This operation returns a buffered response; stream=true is unsupported"
            )
    Just _ -> Left (LlmExpectationError "stream must be a boolean")
