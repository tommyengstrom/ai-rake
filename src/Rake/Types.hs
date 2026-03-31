{-# LANGUAGE PatternSynonyms #-}

module Rake.Types
    ( ResponseFormat (..)
    , jsonSchemaFormat
    , ConversationId (..)
    , HistoryItemId (..)
    , ToolName
    , ToolDescription
    , ToolCallId (..)
    , MediaBlobId (..)
    , StoredMedia (..)
    , ToolCallContinuation (..)
    , ToolCall (..)
    , ToolResponse (..)
    , toolResponseText
    , toolResponseJson
    , ToolResult (..)
    , ToolDef (..)
    , ToolDeclaration (..)
    , MessagePart (..)
    , textPart
    , refusalPart
    , imagePart
    , audioPart
    , filePart
    , GenericRole (..)
    , ItemLifecycle (..)
    , GenericItem (..)
    , ProviderApiFamily (..)
    , providerApiFamilyText
    , pattern ProviderOpenAIResponses
    , pattern ProviderXAIResponses
    , pattern ProviderGeminiInteractions
    , MediaProviderReference (..)
    , ProviderItem (..)
    , ResetCheckpoint (..)
    , HistoryItem (..)
    , itemLifecycle
    , genericItem
    , providerItem
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
    , toolCallWithContinuations
    , resetToStart
    , resetTo
    , toolResult
    , toolResultText
    , toolResultJson
    , nonPortableHistoryItem
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
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

newtype MediaBlobId = MediaBlobId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, IsString)

data StoredMedia = StoredMedia
    { mediaBlobId :: MediaBlobId
    , mimeType :: Maybe Text
    , fileName :: Maybe Text
    , mediaBytes :: ByteString
    }
    deriving stock (Show, Eq, Generic)

data ToolCallContinuation = ToolCallContinuation
    { continuationProviderFamily :: ProviderApiFamily
    , continuationPayload :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ToolCall = ToolCall
    { toolCallId :: ToolCallId
    , toolName :: ToolName
    , toolArgs :: Map Text Value
    , continuationAttachments :: [ToolCallContinuation]
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
    | PartRefusal
        { text :: Text
        }
    | PartImage
        { blobId :: MediaBlobId
        , mimeType :: Maybe Text
        , altText :: Maybe Text
        }
    | PartAudio
        { blobId :: MediaBlobId
        , mimeType :: Maybe Text
        , transcript :: Maybe Text
        }
    | PartFile
        { blobId :: MediaBlobId
        , mimeType :: Maybe Text
        , fileName :: Maybe Text
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

textPart :: Text -> MessagePart
textPart = PartText

refusalPart :: Text -> MessagePart
refusalPart = PartRefusal

imagePart :: MediaBlobId -> Maybe Text -> Maybe Text -> MessagePart
imagePart = PartImage

audioPart :: MediaBlobId -> Maybe Text -> Maybe Text -> MessagePart
audioPart = PartAudio

filePart :: MediaBlobId -> Maybe Text -> Maybe Text -> MessagePart
filePart = PartFile

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
        { role :: GenericRole
        , parts :: [MessagePart]
        }
    | GenericToolCall
        { toolCall :: ToolCall
        }
    | GenericToolResult
        { toolResult :: ToolResult
        }
    | GenericResetTo
        { checkpoint :: ResetCheckpoint
        }
    | GenericReplayBarrier
        { reason :: Text
        }
    | GenericNonPortable
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ProviderApiFamily = ProviderApiFamily
    { providerApiFamilyText :: Text
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, IsString)

pattern ProviderOpenAIResponses :: ProviderApiFamily
pattern ProviderOpenAIResponses = ProviderApiFamily "openai.responses"

pattern ProviderXAIResponses :: ProviderApiFamily
pattern ProviderXAIResponses = ProviderApiFamily "xai.responses"

pattern ProviderGeminiInteractions :: ProviderApiFamily
pattern ProviderGeminiInteractions = ProviderApiFamily "gemini.interactions"

providerApiFamilyText :: ProviderApiFamily -> Text
providerApiFamilyText (ProviderApiFamily familyText) = familyText

data ProviderItem = ProviderItem
    { apiFamily :: ProviderApiFamily
    , exchangeId :: Maybe Text
    , nativeItemId :: Maybe Text
    , payload :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MediaProviderReference = MediaProviderReference
    { mediaBlobId :: MediaBlobId
    , providerFamily :: ProviderApiFamily
    , providerRequestPart :: Value
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

{-# COMPLETE ProviderOpenAIResponses, ProviderXAIResponses, ProviderGeminiInteractions, ProviderApiFamily #-}

data ResetCheckpoint
    = ResetToStart
    | ResetToItem HistoryItemId
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

data HistoryItem = HistoryItem
    { historyItemIdField :: Maybe HistoryItemId
    , itemLifecycle :: ItemLifecycle
    , genericItem :: GenericItem
    , providerItem :: Maybe ProviderItem
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

historyItemId :: HistoryItem -> Maybe HistoryItemId
historyItemId HistoryItem{historyItemIdField} = historyItemIdField

itemLifecycle :: HistoryItem -> ItemLifecycle
itemLifecycle HistoryItem{itemLifecycle = lifecycle} = lifecycle

genericItem :: HistoryItem -> GenericItem
genericItem HistoryItem{genericItem = item} = item

providerItem :: HistoryItem -> Maybe ProviderItem
providerItem HistoryItem{providerItem = item} = item

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
historyItemLifecycle = itemLifecycle

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
    , mediaReferences :: [MediaProviderReference]
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
    toolCallWithContinuations toolCallId toolName toolArgs []

toolCallWithContinuations :: ToolCallId -> ToolName -> Map Text Value -> [ToolCallContinuation] -> HistoryItem
toolCallWithContinuations toolCallId toolName toolArgs continuationAttachments =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem =
            GenericToolCall
                { toolCall =
                    ToolCall
                        { toolCallId
                        , toolName
                        , toolArgs
                        , continuationAttachments
                        }
                }
        , providerItem = Nothing
        }

resetToStart :: HistoryItem
resetToStart =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem = GenericResetTo{checkpoint = ResetToStart}
        , providerItem = Nothing
        }

resetTo :: HistoryItemId -> HistoryItem
resetTo itemId =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem = GenericResetTo{checkpoint = ResetToItem itemId}
        , providerItem = Nothing
        }

toolResult :: ToolCallId -> ToolResponse -> HistoryItem
toolResult toolCallId toolResponse =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem =
            GenericToolResult
                { toolResult =
                    ToolResult
                        { toolCallId
                        , toolResponse
                        }
                }
        , providerItem = Nothing
        }

toolResultText :: ToolCallId -> Text -> HistoryItem
toolResultText toolCallId = toolResult toolCallId . ToolResponseText

toolResultJson :: ToolCallId -> Value -> HistoryItem
toolResultJson toolCallId = toolResult toolCallId . ToolResponseJson

localMessage :: GenericRole -> [MessagePart] -> HistoryItem
localMessage role parts =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem = GenericMessage{role, parts}
        , providerItem = Nothing
        }

nonPortableHistoryItem :: ItemLifecycle -> ProviderItem -> HistoryItem
nonPortableHistoryItem lifecycle rawProviderItem =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = lifecycle
        , genericItem = GenericNonPortable
        , providerItem = Just rawProviderItem
        }
