{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module TypeSafe.Jev.Client
    ( JevSettings (..)
    , defaultJevSettings
    , runJev
    ) where

import Control.Exception (try)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.Char (isControl)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (NominalDiffTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status qualified as HTTP
import System.Timeout (timeout)
import TypeSafe.Jev.Batch (CompiledBatch (..), compileBatch)
import TypeSafe.Jev.Effect
import TypeSafe.Jev.Error
import TypeSafe.Jev.Types
import TypeSafe.Jev.Wire qualified as Wire
import Prelude

data JevSettings = JevSettings
    { apiKey :: Text
    , baseUrl :: Text
    , model :: Text
    , requestTimeout :: NominalDiffTime
    }

defaultJevSettings :: Text -> JevSettings
defaultJevSettings apiKey =
    JevSettings
        { apiKey
        , baseUrl = "https://api.typesafe.ai"
        , model = "jev-latest"
        , requestTimeout = 30
        }

runJev
    :: (IOE :> es, Error JevError :> es)
    => JevSettings
    -> Eff (Jev ': es) a
    -> Eff es a
runJev settings action = do
    -- newTlsManagerWith overwrites the retry hook, re-enabling POST retries.
    manager <-
        liftIO
            (HTTP.newManager tlsManagerSettings{HTTP.managerRetryableException = const False})
    interpret
        ( \_ -> \case
            Assess state batch -> do
                CompiledBatch{questions, decodeAnswers} <- either throwError pure (compileBatch batch)
                if Map.null questions
                    then do
                        answers <- either throwError pure (decodeAnswers Map.empty)
                        pure Response{answers, metadata = Nothing}
                    else do
                        response <-
                            liftIO (requestAssessment manager settings state questions) >>= either throwError pure
                        let Wire.WireResponse
                                { model
                                , answers = wireAnswers
                                , usage = Wire.WireUsage{inputTokens, outputTokens}
                                } = response
                        if inputTokens < 0 || outputTokens < 0
                            then throwError (InvalidResponse "Token usage must be nonnegative")
                            else do
                                answers <- either throwError pure (decodeAnswers wireAnswers)
                                pure Response{answers, metadata = Just CallMetadata{model, inputTokens, outputTokens}}
        )
        action

requestAssessment
    :: HTTP.Manager
    -> JevSettings
    -> Content
    -> Map.Map Text Wire.WireQuestion
    -> IO (Either JevError Wire.WireResponse)
requestAssessment manager JevSettings{apiKey, baseUrl, model, requestTimeout} state questions =
    case validateSettings of
        Left err -> pure (Left err)
        Right timeoutMicros -> do
            parsed <- try @HTTP.HttpException (HTTP.parseRequest (Text.unpack baseUrl))
            case parsed of
                Left _ -> pure (Left (InvalidRequest "Invalid Jev base URL"))
                Right baseRequest
                    | not (BS.null (HTTP.queryString baseRequest)) ->
                        pure (Left (InvalidRequest "Jev base URL must not contain a query string"))
                    | otherwise -> do
                        let request :: HTTP.Request
                            request =
                                baseRequest
                                    { HTTP.method = "POST"
                                    , HTTP.path = BS.dropWhileEnd (== 47) (HTTP.path baseRequest) <> "/v1/systemone"
                                    , HTTP.requestHeaders =
                                        [ ("Authorization", Text.encodeUtf8 ("Bearer " <> apiKey))
                                        , ("Content-Type", "application/json")
                                        ]
                                    , HTTP.requestBody = HTTP.RequestBodyLBS (encode (Wire.WireRequest state model questions))
                                    , HTTP.responseTimeout = HTTP.responseTimeoutMicro timeoutMicros
                                    , HTTP.redirectCount = 0
                                    , HTTP.checkResponse = \_ _ -> pure ()
                                    }
                        result <- timeout timeoutMicros (try @HTTP.HttpException (HTTP.httpLbs request manager))
                        pure $ case result of
                            Nothing -> Left (RequestTimedOut requestTimeout)
                            Just (Left err) -> Left (transportError requestTimeout err)
                            Just (Right response)
                                | HTTP.statusCode (HTTP.responseStatus response) < 200
                                    || HTTP.statusCode (HTTP.responseStatus response) >= 300 ->
                                    Left
                                        HttpFailure
                                            { statusCode = HTTP.statusCode (HTTP.responseStatus response)
                                            , responseBody = HTTP.responseBody response
                                            , requestId =
                                                Text.decodeUtf8With lenientDecode
                                                    <$> lookup "x-typesafe-request-id" (HTTP.responseHeaders response)
                                            }
                                | otherwise ->
                                    either
                                        (Left . InvalidResponse . Text.pack)
                                        Right
                                        (eitherDecode (HTTP.responseBody response))
  where
    validateSettings :: Either JevError Int
    validateSettings
        | Text.any isControl apiKey =
            Left (InvalidRequest "Jev API key must not contain control characters")
        | otherwise = timeoutMicroseconds requestTimeout

timeoutMicroseconds :: NominalDiffTime -> Either JevError Int
timeoutMicroseconds seconds
    | seconds <= 0 || micros > toInteger (maxBound :: Int) =
        Left (InvalidRequest "Jev requestTimeout must be positive and fit in microseconds")
    | otherwise = Right (fromInteger (max 1 micros))
  where
    micros :: Integer
    micros = ceiling (toRational seconds * 1_000_000)

transportError :: NominalDiffTime -> HTTP.HttpException -> JevError
transportError requestTimeout = \case
    HTTP.InvalidUrlException _ _ -> InvalidRequest "Invalid Jev base URL"
    HTTP.HttpExceptionRequest _ detail -> case detail of
        HTTP.ResponseTimeout -> RequestTimedOut requestTimeout
        HTTP.ConnectionTimeout -> RequestTimedOut requestTimeout
        _ -> TransportFailure (Text.pack (show detail))
