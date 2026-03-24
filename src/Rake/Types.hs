module Rake.Types
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
    , ItemLifecycle (..)
    , GenericItem (..)
    , LocalItem (..)
    , ProviderApiFamily (..)
    , NativeProviderItem (..)
    , ProviderHistoryItem (..)
    , HistoryItem (..)
    , historyItemLifecycle
    , ProviderRoundAction (..)
    , ProviderRound (..)
    , ChatPauseReason (..)
    , ChatFailureReason (..)
    , ChatOutcome (..)
    , SamplingOptions (..)
    , defaultSamplingOptions
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

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToParamSchema, ToSchema, toInlinedSchema)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.UUID (UUID)
import Effectful (Eff)
import GHC.Generics (Generic)
import Rake.Internal.Schema (normalizeStructuredOutputSchema)
import Prelude
import Web.HttpApiData

data ResponseFormat
    = Unstructured
    | JsonValue
    | JsonSchema Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

jsonSchemaFormat :: forall a. ToSchema a => ResponseFormat
jsonSchemaFormat = JsonSchema . normalizeStructuredOutputSchema . toJSON $ toInlinedSchema (Proxy @a)

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

data ItemLifecycle
    = ItemPending
    | ItemCompleted
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GenericItem
    = GenericMessage
        { itemLifecycle :: ItemLifecycle
        , role :: GenericRole
        , parts :: [MessagePart]
        }
    | GenericToolCall
        { toolCall :: ToolCall
        , itemLifecycle :: ItemLifecycle
        }
    | GenericToolResult
        { toolResult :: ToolResult
        , itemLifecycle :: ItemLifecycle
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

data ProviderApiFamily
    = ProviderOpenAIResponses
    | ProviderXAIResponses
    | ProviderGeminiInteractions
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

data NativeProviderItem = NativeProviderItem
    { exchangeId :: Maybe Text
    , nativeItemId :: Maybe Text
    , payload :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ProviderHistoryItem = ProviderHistoryItem
    { apiFamily :: ProviderApiFamily
    , itemLifecycle :: ItemLifecycle
    , nativeItem :: NativeProviderItem
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data HistoryItem
    = HLocal LocalItem
    | HProvider ProviderHistoryItem
    deriving stock (Show, Eq, Generic)

historyItemLifecycle :: HistoryItem -> ItemLifecycle
historyItemLifecycle = \case
    HLocal{} ->
        ItemCompleted
    HProvider ProviderHistoryItem{itemLifecycle} ->
        itemLifecycle

data ChatPauseReason
    = PauseIncomplete Text
    | PauseProviderWaiting Text
    | PauseToolLoopLimit Int
    deriving stock (Show, Eq, Generic)

data ChatFailureReason
    = FailureProvider Text
    | FailureContract Text
    deriving stock (Show, Eq, Generic)

data ProviderRoundAction
    = ProviderRoundDone
    | ProviderRoundNeedsLocalTools [ToolCall]
    | ProviderRoundPaused ChatPauseReason
    | ProviderRoundFailed ChatFailureReason
    deriving stock (Show, Eq, Generic)

data ProviderRound = ProviderRound
    { historyItems :: [HistoryItem]
    , action :: ProviderRoundAction
    }
    deriving stock (Show, Eq, Generic)

data ChatOutcome
    = ChatFinished
        { historyItems :: [HistoryItem]
        }
    | ChatPaused
        { historyItems :: [HistoryItem]
        , pauseReason :: ChatPauseReason
        }
    | ChatFailed
        { historyItems :: [HistoryItem]
        , failureReason :: ChatFailureReason
        }
    deriving stock (Show, Eq, Generic)

data SamplingOptions = SamplingOptions
    { temperature :: Maybe Double
    , topP :: Maybe Double
    }
    deriving stock (Show, Eq, Generic)

defaultSamplingOptions :: SamplingOptions
defaultSamplingOptions =
    SamplingOptions
        { temperature = Nothing
        , topP = Nothing
        }

data ChatConfig es = ChatConfig
    { tools :: [ToolDef es]
    , responseFormat :: ResponseFormat
    , sampling :: SamplingOptions
    , onItem :: HistoryItem -> Eff es ()
    , maxToolRounds :: Int
    }

defaultChatConfig :: ChatConfig es
defaultChatConfig =
    ChatConfig
        { tools = []
        , responseFormat = Unstructured
        , sampling = defaultSamplingOptions
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
historyItemSchemaVersion = 2

instance ToJSON HistoryItem where
    toJSON historyItem =
        case historyItem of
            HLocal item ->
                toEnvelope "local" (toJSON item)
            HProvider ProviderHistoryItem{apiFamily, nativeItem} ->
                toEnvelope (providerApiFamilyText apiFamily) (toJSON nativeItem)
      where
        toEnvelope :: Text -> Value -> Value
        toEnvelope apiFamily payload =
            object
                [ "schemaVersion" .= historyItemSchemaVersion
                , "apiFamily" .= apiFamily
                , "itemLifecycle" .= historyItemLifecycle historyItem
                , "payload" .= payload
                ]

instance FromJSON HistoryItem where
    parseJSON = withObject "HistoryItem" $ \obj -> do
        apiFamily <- obj .: "apiFamily"
        schemaVersion <- obj .:? "schemaVersion" :: Parser (Maybe Int)
        itemLifecycle <- fromMaybe ItemCompleted <$> (obj .:? "itemLifecycle")
        payload <- obj .: "payload"

        case schemaVersion of
            Nothing ->
                parseV1 apiFamily payload
            Just 1 ->
                parseV1 apiFamily payload
            Just 2 ->
                parseV2 apiFamily itemLifecycle payload
            Just version ->
                fail $ "Unsupported HistoryItem schema version: " <> show version
      where
        parseV1 :: Text -> Value -> Parser HistoryItem
        parseV1 apiFamily payload =
            case apiFamily of
                "local" ->
                    HLocal <$> parseJSON payload
                "openai.responses" ->
                    HProvider . ProviderHistoryItem ProviderOpenAIResponses ItemCompleted <$> parseJSON payload
                "xai.responses" ->
                    HProvider . ProviderHistoryItem ProviderXAIResponses ItemCompleted <$> parseJSON payload
                "gemini.interactions" ->
                    HProvider . ProviderHistoryItem ProviderGeminiInteractions ItemCompleted <$> parseJSON payload
                other ->
                    fail $ "Unsupported HistoryItem apiFamily: " <> show other

        parseV2 :: Text -> ItemLifecycle -> Value -> Parser HistoryItem
        parseV2 apiFamily itemLifecycle payload =
            case apiFamily of
                "local" ->
                    HLocal <$> parseJSON payload
                "openai.responses" ->
                    HProvider . ProviderHistoryItem ProviderOpenAIResponses itemLifecycle <$> parseJSON payload
                "xai.responses" ->
                    HProvider . ProviderHistoryItem ProviderXAIResponses itemLifecycle <$> parseJSON payload
                "gemini.interactions" ->
                    HProvider . ProviderHistoryItem ProviderGeminiInteractions itemLifecycle <$> parseJSON payload
                other ->
                    fail $ "Unsupported HistoryItem apiFamily: " <> show other

providerApiFamilyText :: ProviderApiFamily -> Text
providerApiFamilyText = \case
    ProviderOpenAIResponses ->
        "openai.responses"
    ProviderXAIResponses ->
        "xai.responses"
    ProviderGeminiInteractions ->
        "gemini.interactions"
