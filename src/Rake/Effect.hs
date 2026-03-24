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
import Servant.Client (ClientError)

data Rake :: Effect where
    GetLlmResponse
        :: [ToolDeclaration]
        -> ResponseFormat
        -> SamplingOptions
        -> [HistoryItem]
        -> Rake m ProviderRound

makeEffect ''Rake

data NativeMsgFormat
    = NativeMsgOut Value
    | NativeMsgIn Value
    | NativeRequestFailure ClientError
    | NativeConversionNote Text
    deriving stock (Generic)
