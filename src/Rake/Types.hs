{-# LANGUAGE PatternSynonyms #-}

module Rake.Types
    ( ResponseFormat (..)
    , jsonSchemaFormat
    , ConversationId (..)
    , HistoryItemId (..)
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
    , ResetCheckpoint (..)
    , ControlItem (..)
    , HistoryItem (HLocal, HProvider, HControl)
    , historyItemId
    , setHistoryItemId
    , ensureHistoryItemId
    , ensureHistoryItemIds
    , historyItemLifecycle
    , ReplayBlockReason (..)
    , ReplayState (..)
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
    , resetToStart
    , resetTo
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
import Data.UUID.V4 (nextRandom)
import Effectful (Eff, IOE, (:>), liftIO)
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

newtype HistoryItemId = HistoryItemId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON)

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

data ResetCheckpoint
    = ResetToStart
    | ResetToItem HistoryItemId
    deriving stock (Show, Eq, Ord, Generic)

instance ToJSON ResetCheckpoint where
    toJSON = \case
        ResetToStart ->
            object ["type" .= ("start" :: Text)]
        ResetToItem itemId ->
            object
                [ "type" .= ("item" :: Text)
                , "itemId" .= itemId
                ]

instance FromJSON ResetCheckpoint where
    parseJSON = withObject "ResetCheckpoint" $ \obj -> do
        checkpointType <- obj .: "type"
        case checkpointType :: Text of
            "start" ->
                pure ResetToStart
            "item" ->
                ResetToItem <$> obj .: "itemId"
            other ->
                fail $ "Unsupported ResetCheckpoint type: " <> show other

data ControlItem
    = ResetTo ResetCheckpoint
    | ReplayBarrier Text
    deriving stock (Show, Eq, Generic)

instance ToJSON ControlItem where
    toJSON = \case
        ResetTo checkpoint ->
            object
                [ "type" .= ("reset_to" :: Text)
                , "checkpoint" .= checkpoint
                ]
        ReplayBarrier reason ->
            object
                [ "type" .= ("replay_barrier" :: Text)
                , "reason" .= reason
                ]

instance FromJSON ControlItem where
    parseJSON = withObject "ControlItem" $ \obj -> do
        controlType <- obj .: "type"
        case controlType :: Text of
            "reset_to" ->
                ResetTo <$> obj .: "checkpoint"
            "replay_barrier" ->
                ReplayBarrier <$> obj .: "reason"
            other ->
                fail $ "Unsupported ControlItem type: " <> show other

data HistoryItemBody
    = HistoryItemLocal LocalItem
    | HistoryItemProvider ProviderHistoryItem
    | HistoryItemControl ControlItem
    deriving stock (Show, Eq, Generic)

data HistoryItem = HistoryItem
    { historyItemIdField :: Maybe HistoryItemId
    , historyItemBody :: HistoryItemBody
    }
    deriving stock (Eq, Generic)

pattern HLocal :: LocalItem -> HistoryItem
pattern HLocal item <- HistoryItem _ (HistoryItemLocal item)
    where
        HLocal item = HistoryItem Nothing (HistoryItemLocal item)

pattern HProvider :: ProviderHistoryItem -> HistoryItem
pattern HProvider item <- HistoryItem _ (HistoryItemProvider item)
    where
        HProvider item = HistoryItem Nothing (HistoryItemProvider item)

pattern HControl :: ControlItem -> HistoryItem
pattern HControl item <- HistoryItem _ (HistoryItemControl item)
    where
        HControl item = HistoryItem Nothing (HistoryItemControl item)

{-# COMPLETE HLocal, HProvider, HControl #-}

instance Show HistoryItem where
    show HistoryItem{historyItemIdField, historyItemBody} =
        "HistoryItem {historyItemId = "
            <> show historyItemIdField
            <> ", historyItemBody = "
            <> show historyItemBody
            <> "}"

historyItemId :: HistoryItem -> Maybe HistoryItemId
historyItemId HistoryItem{historyItemIdField} = historyItemIdField

setHistoryItemId :: Maybe HistoryItemId -> HistoryItem -> HistoryItem
setHistoryItemId itemId historyItem =
    historyItem{historyItemIdField = itemId}

ensureHistoryItemId :: IOE :> es => HistoryItem -> Eff es HistoryItem
ensureHistoryItemId historyItem =
    case historyItemId historyItem of
        Just{} ->
            pure historyItem
        Nothing -> do
            itemId <- HistoryItemId <$> liftIO nextRandom
            pure (setHistoryItemId (Just itemId) historyItem)

ensureHistoryItemIds :: IOE :> es => [HistoryItem] -> Eff es [HistoryItem]
ensureHistoryItemIds =
    traverse ensureHistoryItemId

historyItemLifecycle :: HistoryItem -> ItemLifecycle
historyItemLifecycle = \case
    HLocal{} ->
        ItemCompleted
    HProvider ProviderHistoryItem{itemLifecycle} ->
        itemLifecycle
    HControl{} ->
        ItemCompleted

data ReplayBlockReason
    = ReplayInvalidReset ResetCheckpoint
    | ReplayBlocked Text
    deriving stock (Show, Eq, Generic)

data ReplayState = ReplayState
    { activeHistory :: [HistoryItem]
    , replayHistory :: [HistoryItem]
    , resumableToolCalls :: [ToolCall]
    , pendingArtifacts :: [HistoryItem]
    , blocked :: Maybe ReplayBlockReason
    }
    deriving stock (Show, Eq, Generic)

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
    { roundItems :: [HistoryItem]
    , action :: ProviderRoundAction
    }
    deriving stock (Show, Eq, Generic)

data ChatOutcome
    = ChatFinished
        { appendedItems :: [HistoryItem]
        }
    | ChatPaused
        { appendedItems :: [HistoryItem]
        , pauseReason :: ChatPauseReason
        }
    | ChatFailed
        { appendedItems :: [HistoryItem]
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

resetToStart :: HistoryItem
resetToStart =
    HControl (ResetTo ResetToStart)

resetTo :: HistoryItemId -> HistoryItem
resetTo =
    HControl . ResetTo . ResetToItem

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
historyItemSchemaVersion = 4

instance ToJSON HistoryItem where
    toJSON historyItem =
        case historyItem of
            HLocal item ->
                toEnvelope "local" (toJSON item)
            HProvider ProviderHistoryItem{apiFamily, nativeItem} ->
                toEnvelope (providerApiFamilyText apiFamily) (toJSON nativeItem)
            HControl controlItem ->
                toEnvelope "control" (toJSON controlItem)
      where
        toEnvelope :: Text -> Value -> Value
        toEnvelope apiFamily payload =
            object $
                [ "schemaVersion" .= historyItemSchemaVersion
                , "apiFamily" .= apiFamily
                , "itemLifecycle" .= historyItemLifecycle historyItem
                , "payload" .= payload
                ]
                    <> maybe [] (\itemId -> ["historyItemId" .= itemId]) (historyItemId historyItem)

instance FromJSON HistoryItem where
    parseJSON = withObject "HistoryItem" $ \obj -> do
        apiFamily <- obj .: "apiFamily"
        schemaVersion <- obj .: "schemaVersion" :: Parser Int
        itemLifecycle <- fromMaybe ItemCompleted <$> (obj .:? "itemLifecycle")
        itemId <- obj .:? "historyItemId"
        payload <- obj .: "payload"

        case schemaVersion of
            4 ->
                parseV4 apiFamily itemLifecycle itemId payload
            version ->
                fail $ "Unsupported HistoryItem schema version: " <> show version
      where
        wrapHistoryItem :: Maybe HistoryItemId -> HistoryItem -> HistoryItem
        wrapHistoryItem maybeItemId =
            setHistoryItemId maybeItemId

        parseV4 :: Text -> ItemLifecycle -> Maybe HistoryItemId -> Value -> Parser HistoryItem
        parseV4 apiFamily itemLifecycle itemId payload =
            case apiFamily of
                "local" ->
                    wrapHistoryItem itemId . HLocal <$> parseJSON payload
                "openai.responses" ->
                    wrapHistoryItem itemId . HProvider . ProviderHistoryItem ProviderOpenAIResponses itemLifecycle <$> parseJSON payload
                "xai.responses" ->
                    wrapHistoryItem itemId . HProvider . ProviderHistoryItem ProviderXAIResponses itemLifecycle <$> parseJSON payload
                "gemini.interactions" ->
                    wrapHistoryItem itemId . HProvider . ProviderHistoryItem ProviderGeminiInteractions itemLifecycle <$> parseJSON payload
                "control" ->
                    wrapHistoryItem itemId . HControl <$> parseJSON payload
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
