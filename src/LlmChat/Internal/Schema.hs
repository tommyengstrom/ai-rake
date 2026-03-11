module LlmChat.Internal.Schema
    ( closeOpenObjectSchemas
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Relude

closeOpenObjectSchemas :: Value -> Value
closeOpenObjectSchemas = \case
    Object objectValue ->
        let normalizedObject = KM.map closeOpenObjectSchemas objectValue
            isObjectSchema =
                KM.lookup "type" normalizedObject == Just (String "object")
                    || KM.member "properties" normalizedObject
         in Object $
                if isObjectSchema && not (KM.member "additionalProperties" normalizedObject)
                    then KM.insert "additionalProperties" (Bool False) normalizedObject
                    else normalizedObject
    Array values ->
        Array (closeOpenObjectSchemas <$> values)
    other ->
        other
