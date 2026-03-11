module LlmChat.Types
    ( ResponseFormat (..)
    , jsonSchemaFormat
    , ConversationId (..)
    , ToolName
    , ToolDescription
    , ToolCallId (..)
    , ToolCall (..)
    , ToolResponse (..)
    , toolResponseText
    , toolResponseJson
    , ToolResult (..)
    , ToolDef (..)
    , ToolDeclaration (..)
    , MessagePart (..)
    , textPart
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
    , systemText
    , systemParts
    , developer
    , developerText
    , developerParts
    , user
    , userText
    , userParts
    , assistantText
    , assistantParts
    , toolCall
    , toolResult
    , toolResultText
    , toolResultJson
    ) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.OpenApi (ToParamSchema, ToSchema, toInlinedSchema)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.UUID (UUID)
import Effectful (Eff)
import GHC.Generics (Generic)
import LlmChat.Internal.Schema (closeOpenObjectSchemas)
import Prelude
import Web.HttpApiData

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

data ToolResponse
    = ToolResponseText
        { text :: Text
        }
    | ToolResponseJson
        { json :: Value
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance IsString ToolResponse where
    fromString = ToolResponseText . fromString

toolResponseText :: Text -> ToolResponse
toolResponseText = ToolResponseText

toolResponseJson :: Value -> ToolResponse
toolResponseJson = ToolResponseJson

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

data MessagePart
    = PartText
        { text :: Text
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

textPart :: Text -> MessagePart
textPart = PartText

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
        , parts :: [MessagePart]
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
    = LocalMessage
        { role :: GenericRole
        , parts :: [MessagePart]
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
    , maxToolRounds :: Int
    }

defaultChatConfig :: ChatConfig es
defaultChatConfig =
    ChatConfig
        { tools = []
        , responseFormat = Unstructured
        , onItem = \_ -> pure ()
        , maxToolRounds = 8
        }

system :: Text -> HistoryItem
system = systemText

systemText :: Text -> HistoryItem
systemText content = systemParts [textPart content]

systemParts :: [MessagePart] -> HistoryItem
systemParts = localMessage GenericSystem

developer :: Text -> HistoryItem
developer = developerText

developerText :: Text -> HistoryItem
developerText content = developerParts [textPart content]

developerParts :: [MessagePart] -> HistoryItem
developerParts = localMessage GenericDeveloper

user :: Text -> HistoryItem
user = userText

userText :: Text -> HistoryItem
userText content = userParts [textPart content]

userParts :: [MessagePart] -> HistoryItem
userParts = localMessage GenericUser

assistantText :: Text -> HistoryItem
assistantText content = assistantParts [textPart content]

assistantParts :: [MessagePart] -> HistoryItem
assistantParts = localMessage GenericAssistant

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

toolResultText :: ToolCallId -> Text -> HistoryItem
toolResultText toolCallId = toolResult toolCallId . ToolResponseText

toolResultJson :: ToolCallId -> Value -> HistoryItem
toolResultJson toolCallId = toolResult toolCallId . ToolResponseJson

localMessage :: GenericRole -> [MessagePart] -> HistoryItem
localMessage role parts =
    HLocal LocalMessage{role, parts}

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
