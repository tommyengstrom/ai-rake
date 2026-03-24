module Rake.Providers.Internal
    ( defaultWarningLogger
    , runChatProvider
    , valueToCompactText
    ) where

import Debug.Trace qualified as DebugTrace
import Data.Aeson (Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Dispatch.Dynamic (interpretWith)
import Rake.Effect
import Rake.Types
import Relude

runChatProvider
    :: ( [ToolDeclaration]
         -> ResponseFormat
         -> SamplingOptions
         -> [HistoryItem]
         -> Eff es ProviderRound
       )
    -> Eff (Rake ': es) a
    -> Eff es a
runChatProvider runRound eff =
    interpretWith eff \_ -> \case
        GetLlmResponse tools responseFormat samplingOptions history ->
            runRound tools responseFormat samplingOptions history

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
    warningPrefix = "[ai-rake:" <> toString providerName <> "] "
    logWarning warningMessage =
        DebugTrace.trace warningMessage () `seq` pure ()

valueToCompactText :: Value -> Text
valueToCompactText =
    TL.toStrict . encodeToLazyText
