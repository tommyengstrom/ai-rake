{-# LANGUAGE RecordWildCards #-}

module LlmChat.Providers.Responses.Runner
    ( ProviderTag (..)
    , ResponsesProviderConfig (..)
    , defaultWarningLogger
    , runResponsesProvider
    ) where

import Debug.Trace qualified as DebugTrace
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as Vector
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Providers.Responses.Request
import LlmChat.Types
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

type ResponsesAPI =
    "v1"
        Servant.:> "responses"
        Servant.:> Header "Authorization" Text
        Servant.:> Header "OpenAI-Organization" Text
        Servant.:> Header "OpenAI-Project" Text
        Servant.:> ReqBody '[JSON] Value
        Servant.:> Post '[JSON] Value

responsesApi :: Proxy ResponsesAPI
responsesApi = Proxy

runResponsesProvider
    :: forall es a
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => ResponsesProviderConfig es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runResponsesProvider config@ResponsesProviderConfig{..} eff = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postResponse = client responsesApi

    interpretWith eff \_ -> \case
        GetLlmResponse tools responseFormat samplingOptions history -> do
            requestBody <- buildRequestBody config tools responseFormat samplingOptions history
            requestLogger (NativeMsgOut requestBody)
            responseValue <-
                liftIO
                    ( runClientM
                        ( postResponse
                            (Just ("Bearer " <> apiKey))
                            organizationId
                            projectId
                            requestBody
                        )
                        clientEnv
                    )
                    >>= \case
                        Left err -> do
                            requestLogger (NativeRequestFailure err)
                            throwError (LlmClientError err)
                        Right response ->
                            pure response
            requestLogger (NativeMsgIn responseValue)
            either throwError pure (decodeResponse providerTag responseValue)
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

defaultWarningLogger :: Applicative f => Text -> NativeMsgFormat -> f ()
defaultWarningLogger providerName = \case
    NativeConversionNote note ->
        logWarning (warningPrefix <> toString note)
    NativeRequestFailure err ->
        logWarning (warningPrefix <> "Provider request failed: " <> show err)
    NativeMsgOut{} ->
        pure ()
    NativeMsgIn{} ->
        pure ()
  where
    warningPrefix = "[llmchat-effectful:" <> toString providerName <> "] "
    logWarning warningMessage =
        DebugTrace.trace warningMessage () `seq` pure ()

decodeResponse :: ProviderTag -> Value -> Either LlmChatError [HistoryItem]
decodeResponse providerTag responseValue = do
    responseObject <- expectObject "response" responseValue
    let responseId = lookupText "id" responseObject
    outputValue <-
        maybe
            (Left (LlmExpectationError "Responses API returned no output array"))
            Right
            (KM.lookup "output" responseObject)
    outputItems <- expectArray "response.output" outputValue
    forM (Vector.toList outputItems) $ \payload -> do
        payloadObject <- expectObject "response.output item" payload
        let nativeItemId = lookupText "id" payloadObject
            nativeItem =
                NativeResponseItem
                    { responseId
                    , nativeItemId
                    , payload
                    }
        pure (wrapNativeHistoryItem providerTag nativeItem)

wrapNativeHistoryItem :: ProviderTag -> NativeResponseItem -> HistoryItem
wrapNativeHistoryItem providerTag nativeItem =
    case providerTag of
        ProviderOpenAI ->
            HOpenAIResponses (OpenAIResponsesItem nativeItem)
        ProviderXAI ->
            HXAIResponses (XAIResponsesItem nativeItem)

lookupText :: Key -> Object -> Maybe Text
lookupText key objectValue = KM.lookup key objectValue >>= \case
    String text ->
        Just text
    _ ->
        Nothing

expectObject :: Text -> Value -> Either LlmChatError Object
expectObject label = \case
    Object objectValue ->
        Right objectValue
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an object"))

expectArray :: Text -> Value -> Either LlmChatError (Vector.Vector Value)
expectArray label = \case
    Array values ->
        Right values
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an array"))
