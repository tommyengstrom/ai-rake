module LlmChat.Providers.XAI.Responses
    ( XAIResponsesSettings (..)
    , defaultXAIResponsesSettings
    , runLlmChatXAIResponses
    ) where

import Effectful
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Providers.Responses.Internal
import Relude

data XAIResponsesSettings es = XAIResponsesSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultXAIResponsesSettings :: Text -> XAIResponsesSettings es
defaultXAIResponsesSettings apiKey =
    XAIResponsesSettings
        { apiKey
        , model = "grok-4-fast-non-reasoning"
        , baseUrl = "https://api.x.ai"
        , requestLogger = defaultWarningLogger "xai.responses"
        }

runLlmChatXAIResponses
    :: forall es a
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => XAIResponsesSettings es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runLlmChatXAIResponses XAIResponsesSettings{..} =
    runResponsesProvider
        ResponsesProviderConfig
            { providerTag = ProviderXAI
            , apiKey
            , model
            , baseUrl
            , organizationId = Nothing
            , projectId = Nothing
            , requestLogger
            }
