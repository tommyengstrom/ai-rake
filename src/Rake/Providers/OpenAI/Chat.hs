module Rake.Providers.OpenAI.Chat
    ( OpenAIChatSettings (..)
    , defaultOpenAIChatSettings
    , decodeOpenAIResponse
    , runRakeOpenAIChat
    ) where

import Data.Aeson (Value)
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.MediaStorage.Effect
import Rake.Providers.Chat.Responses
import Rake.Providers.Internal (defaultWarningLogger)
import Rake.Types (ProviderRound)
import Relude

data OpenAIChatSettings es = OpenAIChatSettings
    { apiKey :: Text
    , model :: Text
    , baseUrl :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenAIChatSettings :: Text -> OpenAIChatSettings es
defaultOpenAIChatSettings apiKey =
    OpenAIChatSettings
        { apiKey
        , model = "gpt-4.1-mini"
        , baseUrl = "https://api.openai.com"
        , organizationId = Nothing
        , projectId = Nothing
        , requestLogger = defaultWarningLogger "openai.chat"
        }

runRakeOpenAIChat
    :: forall es a
     . ( IOE :> es
       , Error RakeError :> es
       , RakeMediaStorage :> es
       )
    => OpenAIChatSettings es
    -> Eff (Rake ': es) a
    -> Eff es a
runRakeOpenAIChat OpenAIChatSettings{..} =
    runResponsesChatProvider
        ResponsesProviderConfig
            { providerTag = ResponsesProviderOpenAI
            , apiKey
            , model
            , baseUrl
            , organizationId
            , projectId
            , requestLogger
            }

decodeOpenAIResponse :: Value -> Either RakeError ProviderRound
decodeOpenAIResponse =
    decodeResponsesResponse ResponsesProviderOpenAI
