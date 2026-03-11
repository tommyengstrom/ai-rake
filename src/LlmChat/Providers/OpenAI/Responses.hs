module LlmChat.Providers.OpenAI.Responses
    ( OpenAIResponsesSettings (..)
    , defaultOpenAIResponsesSettings
    , runLlmChatOpenAIResponses
    ) where

import Effectful
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Providers.Responses.Internal
import Relude

data OpenAIResponsesSettings es = OpenAIResponsesSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenAIResponsesSettings :: Text -> OpenAIResponsesSettings es
defaultOpenAIResponsesSettings apiKey =
    OpenAIResponsesSettings
        { apiKey
        , model = "gpt-4.1-mini"
        , baseUrl = "https://api.openai.com"
        , organizationId = Nothing
        , projectId = Nothing
        , requestLogger = defaultWarningLogger "openai.responses"
        }

runLlmChatOpenAIResponses
    :: forall es a
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => OpenAIResponsesSettings es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runLlmChatOpenAIResponses OpenAIResponsesSettings{..} =
    runResponsesProvider
        ResponsesProviderConfig
            { providerTag = ProviderOpenAI
            , apiKey
            , model
            , baseUrl
            , organizationId
            , projectId
            , requestLogger
            }
