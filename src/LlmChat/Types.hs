module LlmChat.Types
    ( ResponseFormat (..)
    , jsonSchemaFormat
    , ConversationId (..)
    , ToolName
    , ToolDescription
    , ToolCallId (..)
    , ToolCall (..)
    , ToolResponse (..)
    , ToolResult (..)
    , ToolDef (..)
    , ToolDeclaration (..)
    , GenericRole (..)
    , GenericItem (..)
    , LocalItem (..)
    , NativeResponseItem (..)
    , OpenAIResponsesItem (..)
    , XAIResponsesItem (..)
    , HistoryItem (..)
    , ChatConfig (..)
    , defaultChatConfig
    , system
    , developer
    , user
    , assistantText
    , toolCall
    , toolResult
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.OpenApi (ToParamSchema, ToSchema, toInlinedSchema)
import Data.String (IsString)
import Data.Text (Text)
import Data.UUID (UUID)
import Control.Monad (when)
import Data.Proxy (Proxy (..))
import Effectful (Eff)
import GHC.Generics (Generic)
import LlmChat.Internal.Schema (closeOpenObjectSchemas)
import Web.HttpApiData
import Prelude

data ResponseFormat
    = Unstructured
    | JsonValue
    | JsonSchema Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

jsonSchemaFormat :: forall a. ToSchema a => ResponseFormat
jsonSchemaFormat = JsonSchema . closeOpenObjectSchemas . toJSON $ toInlinedSchema (Proxy @a)

newtype ConversationId = ConversationId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype
        ( FromJSON
        , ToJSON
        , ToParamSchema
        , FromHttpApiData
        , ToHttpApiData
        )

type ToolName = Text
type ToolDescription = Text

newtype ToolCallId = ToolCallId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, IsString)

data ToolCall = ToolCall
    { toolCallId :: ToolCallId
    , toolName :: ToolName
    , toolArgs :: Map Text Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ToolResponse = ToolResponse
    { response :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON, IsString)

data ToolResult = ToolResult
    { toolCallId :: ToolCallId
    , toolResponse :: ToolResponse
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ToolDef es = ToolDef
    { name :: ToolName
    , description :: ToolDescription
    , parameterSchema :: Maybe Value
    , executeFunction :: Value -> Eff es (Either String ToolResponse)
    }

data ToolDeclaration = ToolDeclaration
    { name :: ToolName
    , description :: ToolDescription
    , parameterSchema :: Maybe Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GenericRole
    = GenericSystem
    | GenericDeveloper
    | GenericUser
    | GenericAssistant
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GenericItem
    = GenericMessage
        { role :: GenericRole
        , content :: Text
        }
    | GenericToolCall
        { toolCall :: ToolCall
        }
    | GenericToolResult
        { toolResult :: ToolResult
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data LocalItem
    = LocalSystem
        { content :: Text
        }
    | LocalDeveloper
        { content :: Text
        }
    | LocalUser
        { content :: Text
        }
    | LocalAssistantText
        { content :: Text
        }
    | LocalToolCall
        { toolCall :: ToolCall
        }
    | LocalToolResult
        { toolResult :: ToolResult
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data NativeResponseItem = NativeResponseItem
    { responseId :: Maybe Text
    , nativeItemId :: Maybe Text
    , payload :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype OpenAIResponsesItem = OpenAIResponsesItem
    { nativeItem :: NativeResponseItem
    }
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

newtype XAIResponsesItem = XAIResponsesItem
    { nativeItem :: NativeResponseItem
    }
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

data HistoryItem
    = HLocal LocalItem
    | HOpenAIResponses OpenAIResponsesItem
    | HXAIResponses XAIResponsesItem
    deriving stock (Show, Eq, Generic)

data ChatConfig es = ChatConfig
    { tools :: [ToolDef es]
    , responseFormat :: ResponseFormat
    , onItem :: HistoryItem -> Eff es ()
    }

defaultChatConfig :: ChatConfig es
defaultChatConfig =
    ChatConfig
        { tools = []
        , responseFormat = Unstructured
        , onItem = \_ -> pure ()
        }

system :: Text -> HistoryItem
system = HLocal . LocalSystem

developer :: Text -> HistoryItem
developer = HLocal . LocalDeveloper

user :: Text -> HistoryItem
user = HLocal . LocalUser

assistantText :: Text -> HistoryItem
assistantText = HLocal . LocalAssistantText

toolCall :: ToolCallId -> ToolName -> Map Text Value -> HistoryItem
toolCall toolCallId toolName toolArgs =
    HLocal
        LocalToolCall
            { toolCall =
                ToolCall
                    { toolCallId
                    , toolName
                    , toolArgs
                    }
            }

toolResult :: ToolCallId -> ToolResponse -> HistoryItem
toolResult toolCallId toolResponse =
    HLocal
        LocalToolResult
            { toolResult =
                ToolResult
                    { toolCallId
                    , toolResponse
                    }
            }

historyItemSchemaVersion :: Int
historyItemSchemaVersion = 1

instance ToJSON HistoryItem where
    toJSON = \case
        HLocal item ->
            toEnvelope "local" (toJSON item)
        HOpenAIResponses item ->
            toEnvelope "openai.responses" (toJSON item)
        HXAIResponses item ->
            toEnvelope "xai.responses" (toJSON item)
      where
        toEnvelope :: Text -> Value -> Value
        toEnvelope apiFamily payload =
            object
                [ "apiFamily" .= apiFamily
                , "payload" .= payload
                ]

instance FromJSON HistoryItem where
    parseJSON = withObject "HistoryItem" $ \obj -> do
        apiFamily <- obj .: "apiFamily"
        schemaVersion <- obj .:? "schemaVersion" :: Parser (Maybe Int)
        payload <- obj .: "payload"

        when
            ( case schemaVersion of
                Nothing -> False
                Just version -> version /= historyItemSchemaVersion
            )
            $
            fail $ "Unsupported HistoryItem schema version: " <> show schemaVersion

        case (apiFamily :: Text) of
            "local" ->
                HLocal <$> parseJSON payload
            "openai.responses" ->
                HOpenAIResponses <$> parseJSON payload
            "xai.responses" ->
                HXAIResponses <$> parseJSON payload
            other ->
                fail $ "Unsupported HistoryItem apiFamily: " <> show other
