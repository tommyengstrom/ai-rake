{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LlmChat.Error where

import Data.Aeson (Value (..), decode)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Types.Status (statusCode)
import Data.Generics.Labels ()
import Servant.Client (ClientError (..), ResponseF (..))
import Relude

data LlmChatError
    = LlmClientError ClientError
    | LlmExpectationError String
    | ToolLoopLimitExceeded Int
    deriving stock (Show, Eq, Generic)

renderLlmChatError :: LlmChatError -> Text
renderLlmChatError = \case
    LlmClientError clientError ->
        renderClientError clientError
    LlmExpectationError err ->
        toText err
    ToolLoopLimitExceeded maxRounds ->
        "Tool loop limit exceeded after " <> show maxRounds <> " rounds"

renderClientError :: ClientError -> Text
renderClientError = \case
    FailureResponse _ response ->
        "Provider request failed (HTTP "
            <> show (statusCode (responseStatusCode response))
            <> ")"
            <> renderResponseDetail response
    DecodeFailure err response ->
        "Provider returned an unreadable response (HTTP "
            <> show (statusCode (responseStatusCode response))
            <> "): "
            <> err
    UnsupportedContentType mediaType response ->
        "Provider returned an unsupported content type (HTTP "
            <> show (statusCode (responseStatusCode response))
            <> "): "
            <> show mediaType
    InvalidContentTypeHeader response ->
        "Provider returned an invalid content-type header (HTTP "
            <> show (statusCode (responseStatusCode response))
            <> ")"
    ConnectionError err ->
        "Provider connection failed: " <> toText (displayException err)

renderResponseDetail :: ResponseF LBS.ByteString -> Text
renderResponseDetail response =
    maybe "" ((": " <>) . stripTrailingPeriod) (extractResponseDetail response)

extractResponseDetail :: ResponseF LBS.ByteString -> Maybe Text
extractResponseDetail response =
    extractJsonErrorDetail =<< decode @Value (responseBody response)

extractJsonErrorDetail :: Value -> Maybe Text
extractJsonErrorDetail = \case
    Object obj ->
        combineCodeAndMessage
            (lookupTextField "code" obj <|> lookupNestedTextField "error" "code" obj)
            ( lookupTextField "error" obj
                <|> lookupTextField "message" obj
                <|> lookupNestedTextField "error" "message" obj
            )
    String textValue ->
        Just textValue
    _ ->
        Nothing

lookupTextField :: Text -> KM.KeyMap Value -> Maybe Text
lookupTextField fieldName obj =
    KM.lookup (fromText fieldName) obj >>= \case
        String textValue ->
            Just textValue
        _ ->
            Nothing

lookupNestedTextField :: Text -> Text -> KM.KeyMap Value -> Maybe Text
lookupNestedTextField outerField innerField obj =
    KM.lookup (fromText outerField) obj >>= \case
        Object nestedObj ->
            lookupTextField innerField nestedObj
        _ ->
            Nothing

combineCodeAndMessage :: Maybe Text -> Maybe Text -> Maybe Text
combineCodeAndMessage maybeCode maybeMessage =
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

stripTrailingPeriod :: Text -> Text
stripTrailingPeriod textValue =
    fromMaybe textValue (T.stripSuffix "." textValue)
