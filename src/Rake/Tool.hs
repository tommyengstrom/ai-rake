module Rake.Tool where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as Map
import Data.OpenApi (ToSchema, toInlinedSchema)
import Effectful
import Rake.Internal.Schema (normalizeStructuredOutputSchema)
import Rake.Types
import Relude

runTool
    :: ToolDef es
    -> Map Text Value
    -> Eff es (Either String ToolResponse)
runTool ToolDef{executeFunction} args = do
    let argsValue = Object (KM.fromMap (Map.mapKeys fromText args))
    executeFunction argsValue

defineToolNoArgument
    :: ToolName
    -> ToolDescription
    -> Eff es (Either String ToolResponse)
    -> ToolDef es
defineToolNoArgument toolName toolDescription executeFunction =
    ToolDef
        { name = toolName
        , description = toolDescription
        , parameterSchema = Nothing
        , executeFunction = \_ -> executeFunction
        }

defineToolWithArgument
    :: forall a es
     . (FromJSON a, ToSchema a)
    => ToolName
    -> ToolDescription
    -> (a -> Eff es (Either String ToolResponse))
    -> ToolDef es
defineToolWithArgument toolName toolDescription executeFunction =
    ToolDef
        { name = toolName
        , description = toolDescription
        , parameterSchema =
            Just . normalizeStructuredOutputSchema . toJSON $ toInlinedSchema (Proxy @a)
        , executeFunction = \args ->
            case fromJSON args of
                Error err ->
                    pure $ Left $ "Failed to parse tool arguments: " <> err
                Success val ->
                    executeFunction val
        }
