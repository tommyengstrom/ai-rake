module Rake.Providers.XAI.Chat
    ( XAIReasoningEffort (..)
    , XAIChatSettings (..)
    , defaultXAIChatSettings
    , minimumXAIReasoningEffort
    , decodeXAIResponse
    , runRakeXAIChat
    ) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.MediaStorage.Effect
import Rake.Providers.Chat.Responses
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Types (ProviderRound)
import Relude

data XAIReasoningEffort
    = XAIReasoningNone
    | XAIReasoningLow
    | XAIReasoningMedium
    | XAIReasoningHigh
    deriving stock (Show, Eq)

instance ToJSON XAIReasoningEffort where
    toJSON =
        String . \case
            XAIReasoningNone -> "none"
            XAIReasoningLow -> "low"
            XAIReasoningMedium -> "medium"
            XAIReasoningHigh -> "high"

minimumXAIReasoningEffort :: Text -> XAIReasoningEffort
minimumXAIReasoningEffort model
    | any (`T.isPrefixOf` normalizedModel) mandatoryReasoningPrefixes =
        XAIReasoningLow
    | "reasoning"
        `T.isInfixOf` normalizedModel
        && not ("non-reasoning" `T.isInfixOf` normalizedModel) =
        XAIReasoningLow
    | otherwise =
        XAIReasoningNone
  where
    normalizedModel :: Text
    normalizedModel = T.toCaseFold (T.strip model)
    mandatoryReasoningPrefixes :: [Text]
    mandatoryReasoningPrefixes =
        [ "grok-4.5"
        , "grok-4.6"
        , "grok-build-latest"
        , "grok-4.20-multi-agent"
        ]

data XAIChatSettings es = XAIChatSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , reasoningEffort :: Maybe XAIReasoningEffort
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultXAIChatSettings :: Text -> XAIChatSettings es
defaultXAIChatSettings apiKey =
    XAIChatSettings
        { apiKey
        , model = "grok-4-fast-non-reasoning"
        , baseUrl = "https://api.x.ai"
        , reasoningEffort = Nothing
        , requestLogger = defaultWarningLogger "xai.chat"
        }

runRakeXAIChat
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => XAIChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeXAIChat XAIChatSettings{..} =
    runResponsesChatProvider
        ResponsesProviderConfig
            { providerTag = ResponsesProviderXAI
            , apiKey
            , model
            , baseUrl
            , organizationId = Nothing
            , projectId = Nothing
            , reasoningConfig = (\effort -> object ["effort" .= effort]) <$> reasoningEffort
            , requestLogger
            }

decodeXAIResponse :: Value -> Either RakeError ProviderRound
decodeXAIResponse =
    decodeResponsesResponse ResponsesProviderXAI
