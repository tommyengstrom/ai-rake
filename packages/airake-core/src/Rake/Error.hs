{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rake.Error where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text qualified as AesonText
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (NominalDiffTime)
import Rake.Types (ReplayBlockReason (..), ResetCheckpoint (..))
import Relude

data ProviderError = ProviderError
    { errorStatus :: Maybe Int
    , errorMessage :: Text
    , errorBody :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)

data RakeError
    = LlmClientError ProviderError
    | LlmExpectationError String
    | LlmInvalidResponseError String
    | LlmTimeoutError NominalDiffTime
    | StreamingInternalError StreamingInternalIssue
    | ConversationBlocked ReplayBlockReason (Maybe ResetCheckpoint)
    deriving stock (Show, Eq, Generic)

data StreamingInternalIssue
    = OnAssistantTextDeltaFailed String
    | OnAssistantRefusalDeltaFailed String
    | OnAudioChunkFailed String
    | RequestLoggerFailed String
    deriving stock (Show, Eq, Generic)

renderRakeError :: RakeError -> Text
renderRakeError = \case
    LlmClientError clientError ->
        renderProviderError clientError
    LlmExpectationError err ->
        toText err
    LlmInvalidResponseError err ->
        "Provider returned an invalid response: " <> toText err
    LlmTimeoutError timeoutSeconds ->
        "LLM call timed out after " <> show timeoutSeconds
    StreamingInternalError issue ->
        renderStreamingInternalIssue issue
    ConversationBlocked blockedReason maybeCheckpoint ->
        renderBlockedConversation blockedReason maybeCheckpoint

renderStreamingInternalIssue :: StreamingInternalIssue -> Text
renderStreamingInternalIssue = \case
    OnAssistantTextDeltaFailed err ->
        "Streaming assistant text callback failed: " <> toText err
    OnAssistantRefusalDeltaFailed err ->
        "Streaming assistant refusal callback failed: " <> toText err
    OnAudioChunkFailed err ->
        "Streaming audio chunk callback failed: " <> toText err
    RequestLoggerFailed err ->
        "Streaming request logger failed: " <> toText err

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

renderProviderError :: ProviderError -> Text
renderProviderError ProviderError{errorMessage} = errorMessage

extractJsonErrorDetail :: Value -> Maybe Text
extractJsonErrorDetail = \case
    Object obj ->
        combineCodeAndMessage
            (lookupTextField "code" obj <|> lookupNestedTextField "error" "code" obj)
            ( lookupTextField "error" obj
                <|> lookupTextField "message" obj
                <|> lookupNestedTextField "error" "message" obj
            )
            <|> renderStatusDetail obj
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

lookupRenderableField :: Text -> KM.KeyMap Value -> Maybe Text
lookupRenderableField fieldName obj =
    KM.lookup (fromText fieldName) obj >>= renderScalarValue

renderScalarValue :: Value -> Maybe Text
renderScalarValue = \case
    String textValue ->
        Just textValue
    Number numberValue ->
        Just (TL.toStrict (AesonText.encodeToLazyText numberValue))
    Bool boolValue ->
        Just (if boolValue then "true" else "false")
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

renderStatusDetail :: KM.KeyMap Value -> Maybe Text
renderStatusDetail obj = do
    statusText <- lookupRenderableField "status" obj
    let detailFields =
            [ "status=" <> statusText
            ]
                <> maybe
                    []
                    (\progressValue -> ["progress=" <> progressValue])
                    (lookupRenderableField "progress" obj)
    pure (T.intercalate ", " detailFields)

stripTrailingPeriod :: Text -> Text
stripTrailingPeriod textValue =
    fromMaybe textValue (T.stripSuffix "." textValue)
