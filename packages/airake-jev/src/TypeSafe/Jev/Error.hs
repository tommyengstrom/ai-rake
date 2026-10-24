module TypeSafe.Jev.Error
    ( JevError (..)
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Prelude

data JevError
    = InvalidRequest Text
    | TransportFailure Text
    | HttpFailure
        { statusCode :: Int
        , responseBody :: ByteString
        , requestId :: Maybe Text
        }
    | InvalidResponse Text
    | RequestTimedOut NominalDiffTime
    deriving stock (Show, Eq)
