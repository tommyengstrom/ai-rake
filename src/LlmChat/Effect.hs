module LlmChat.Effect
    ( module LlmChat.Effect
    , module LlmChat.Error
    ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful
import Effectful.TH
import GHC.Generics (Generic)
import LlmChat.Error
import LlmChat.Types
import Servant.Client (ClientError)

data LlmChat :: Effect where
    GetLlmResponse
        :: [ToolDeclaration]
        -> ResponseFormat
        -> SamplingOptions
        -> [HistoryItem]
        -> LlmChat m [HistoryItem]

makeEffect ''LlmChat

data NativeMsgFormat
    = NativeMsgOut Value
    | NativeMsgIn Value
    | NativeRequestFailure ClientError
    | NativeConversionNote Text
    deriving stock (Generic)
