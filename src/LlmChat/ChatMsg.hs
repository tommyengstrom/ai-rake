module LlmChat.ChatMsg where

import Control.Lens
import Data.Aeson
import Data.OpenApi
    ( AdditionalProperties (..)
    , NamedSchema (..)
    , OpenApiType (..)
    , Referenced (..)
    , ToSchema
    , declareSchemaRef
    )
import Data.OpenApi.Lens
import Data.OpenApi.Schema (ToSchema (..))
import Relude
import LlmChat.Error

-- | Conversion between the message type used by the provider and our representation
-- `toChatMsgIn` is subject to change if it turns out that one-to-one conversion isn't
-- general enough.
class (ToJSON in_, ToJSON out) => IsChatMsg in_ out | in_ -> out , out -> in_ where
    toChatMsg :: in_ -> Either LlmChatError ChatMsg

    fromChatMsg :: ChatMsg -> out

data ChatMsg
    = SystemMsg
        { content :: Text
        }
    | UserMsg
        { content :: Text
        }
    | AssistantMsg
        { content :: Text
        , toolCalls :: [ToolCall]
        }
    | ToolCallResponseMsg -- FIXME: rename to ToolResponseMsg
        { toolCallId :: ToolCallId
        , toolResponse :: ToolResponse
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

type ToolName = Text
type ToolDescription = Text

newtype ToolCallId = ToolCallId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema, IsString)

-- | Represents a tool/function call made by the LLM
data ToolCall = ToolCall
    { toolCallId :: ToolCallId
    -- ^ Unique identifier for this tool call
    , toolName :: ToolName
    -- ^ Name of the tool to invoke
    , toolArgs :: Map Text Value -- FIXME: Use `Object`?
    -- ^ Arguments as key-value pairs
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance ToSchema ToolCall where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy @Text)
        return $
            NamedSchema (Just "ToolCall") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ [ ("toolCallId", textSchema)
                           , ("toolName", textSchema)
                           ,
                               ( "toolArgs"
                               , Inline $
                                    mempty
                                        & type_ ?~ OpenApiObject
                                        & additionalProperties ?~ AdditionalPropertiesAllowed True
                               )
                           ]
                    & required .~ ["toolCallId", "toolName", "toolArgs"]

newtype ToolResponse = ToolResponse
    { response :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON, ToSchema, IsString)

