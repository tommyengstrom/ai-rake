{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rake.Error where

import Data.Aeson (Value (..), decode)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Types.Status (statusCode)
import Data.Generics.Labels ()
import Rake.Types (ReplayBlockReason (..), ResetCheckpoint (..))
import Servant.Client (ClientError (..), ResponseF (..))
import Relude

data RakeError
    = LlmClientError ClientError
    | LlmExpectationError String
    | ConversationBlocked ReplayBlockReason (Maybe ResetCheckpoint)
    deriving stock (Show, Eq, Generic)

renderRakeError :: RakeError -> Text
renderRakeError = \case
    LlmClientError clientError ->
        renderClientError clientError
    LlmExpectationError err ->
        toText err
    ConversationBlocked blockedReason maybeCheckpoint ->
        renderBlockedConversation blockedReason maybeCheckpoint

renderBlockedConversation :: ReplayBlockReason -> Maybe ResetCheckpoint -> Text
renderBlockedConversation blockedReason maybeCheckpoint =
    stripTrailingPeriod (renderReplayBlockReason blockedReason)
        <> ". Reset before continuing."
        <> case maybeCheckpoint of
            Just checkpoint ->
                " Latest valid reset checkpoint: " <> renderResetCheckpoint checkpoint
            Nothing ->
                " No concrete reset checkpoint can be suggested because the supplied history has no stable item ids."

renderReplayBlockReason :: ReplayBlockReason -> Text
renderReplayBlockReason = \case
    ReplayInvalidReset checkpoint ->
        "Invalid reset checkpoint " <> renderResetCheckpoint checkpoint
    ReplayBlocked reason ->
        reason

renderResetCheckpoint :: ResetCheckpoint -> Text
renderResetCheckpoint = \case
    ResetToStart ->
        "start"
    ResetToItem itemId ->
        "item " <> show itemId

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
