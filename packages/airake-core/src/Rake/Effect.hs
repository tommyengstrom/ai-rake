module Rake.Effect
    ( module Rake.Effect
    , module Rake.Error
    ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful
import Effectful.TH
import GHC.Generics (Generic)
import Rake.Error
import Rake.Types

data Rake :: Effect where
    GetLlmResponse
        :: [ToolDeclaration]
        -> ResponseFormat
        -> SamplingOptions
        -> [HistoryItem]
        -> Rake m ProviderRound
    GetLlmResponseStream
        :: (Text -> m ())
        -> (Text -> m ())
        -> [ToolDeclaration]
        -> ResponseFormat
        -> SamplingOptions
        -> [HistoryItem]
        -> Rake m ProviderRound

makeEffect ''Rake

data NativeMsgFormat
    = NativeMsgOut Value
    | NativeMsgIn Value
    | NativeRequestFailure ProviderError
    | NativeConversionNote Text
    deriving stock (Generic)
