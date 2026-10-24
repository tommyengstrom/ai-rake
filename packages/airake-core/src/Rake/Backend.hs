module Rake.Backend
    ( runChatProvider
    , validateProviderRound
    , protectStreamCallbacks
    , protectStreamingInternalAction
    , defaultWarningLogger
    ) where

import Debug.Trace qualified as DebugTrace
import Effectful
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Error.Static
import Rake.Effect
import Rake.Types
import Relude
import UnliftIO.Exception qualified as Exception

validateProviderRound :: ProviderRound -> Either RakeError ProviderRound
validateProviderRound roundResult@ProviderRound{action} =
    case action of
        ProviderRoundFailed (FailureContract reason) ->
            Left (LlmInvalidResponseError (toString reason))
        ProviderRoundFailed FailureProvider{} -> Right roundResult
        ProviderRoundDone -> Right roundResult
        ProviderRoundNeedsLocalTools{} -> Right roundResult
        ProviderRoundPaused{} -> Right roundResult

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
        logWarning (warningPrefix <> toString (renderProviderError err))
    NativeMsgOut{} ->
        pure ()
    NativeMsgIn{} ->
        pure ()
  where
    warningPrefix = "[ai-rake:" <> toString providerName <> "] "
    logWarning warningMessage =
        DebugTrace.trace warningMessage () `seq` pure ()

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

