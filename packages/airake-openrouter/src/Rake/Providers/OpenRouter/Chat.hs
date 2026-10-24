{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.Chat
    ( OpenRouterChatModel (..)
    , CustomChatModel (..)
    , Gpt41Mini (..)
    , defaultGpt41Mini
    , runRakeOpenRouterChat
    , decodeOpenRouterResponse
    ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Effectful
import Effectful.Error.Static
import Rake.Effect
import Rake.MediaStorage.Effect
import Rake.Providers.OpenRouter.Internal.Responses
import Rake.Providers.OpenRouter.Settings
import Rake.Types (ProviderRound)
import Relude

class OpenRouterChatModel model where
    chatModelName :: model -> Text
    chatModelConfig :: model -> Object

data CustomChatModel = CustomChatModel
    { modelName :: Text
    , settings :: Object
    }
    deriving stock (Show, Eq)

instance OpenRouterChatModel CustomChatModel where
    chatModelName CustomChatModel{modelName} = modelName
    chatModelConfig CustomChatModel{settings} = settings

data Gpt41Mini = Gpt41Mini
    { temperature :: Maybe Double
    , topP :: Maybe Double
    , maxOutputTokens :: Maybe Int
    }
    deriving stock (Show, Eq)

defaultGpt41Mini :: Gpt41Mini
defaultGpt41Mini = Gpt41Mini Nothing Nothing Nothing

instance OpenRouterChatModel Gpt41Mini where
    chatModelName _ = "openai/gpt-4.1-mini"
    chatModelConfig Gpt41Mini{temperature, topP, maxOutputTokens} =
        KM.fromList
            $ catMaybes
                [ ("temperature" .=) <$> temperature
                , ("top_p" .=) <$> topP
                , ("max_output_tokens" .=) <$> maxOutputTokens
                ]

runRakeOpenRouterChat
    :: (OpenRouterChatModel model, IOE :> es, Error RakeError :> es, RakeMediaStorage :> es)
    => OpenRouterSettings es -> model -> Eff (Rake ': es) a -> Eff es a
runRakeOpenRouterChat OpenRouterSettings
                        { apiKey
                        , baseUrl
                        , provider
                        , requestLogger
                        , requestTimeoutMilliseconds
                        , httpManager
                        } model =
    runResponsesChatProvider
        ResponsesProviderConfig
            { providerTag = ResponsesProviderOpenRouter (chatModelName model)
            , httpManager
            , requestTimeoutMilliseconds
            , apiKey
            , baseUrl
            , model = chatModelName model
            , extraConfig = chatModelConfig model
            , providerRouting = provider
            , organizationId = Nothing
            , projectId = Nothing
            , reasoningConfig = Nothing
            , requestLogger
            }

decodeOpenRouterResponse :: Text -> Value -> Either RakeError ProviderRound
decodeOpenRouterResponse model = decodeResponsesResponse (ResponsesProviderOpenRouter model)
