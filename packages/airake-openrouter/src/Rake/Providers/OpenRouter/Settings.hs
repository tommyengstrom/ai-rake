{-# LANGUAGE NoOverloadedLists #-}

module Rake.Providers.OpenRouter.Settings
    ( OpenRouterSettings (..)
    , withOpenRouterManager
    , defaultOpenRouterSettings
    , assembleRequest
    ) where

import Control.Monad (foldM)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Effectful
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Rake.Backend (defaultWarningLogger)
import Rake.Effect
import Relude

data OpenRouterSettings es = OpenRouterSettings
    { apiKey :: Text
    , baseUrl :: Text
    , provider :: Object
    , httpManager :: Maybe HTTP.Manager
    , requestTimeoutMilliseconds :: Int
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

defaultOpenRouterSettings :: Text -> OpenRouterSettings es
defaultOpenRouterSettings apiKey =
    OpenRouterSettings
        { apiKey
        , baseUrl = "https://openrouter.ai/api/v1"
        , provider = mempty
        , httpManager = Nothing
        , requestTimeoutMilliseconds = 180000
        , requestLogger = defaultWarningLogger "openrouter"
        }

withOpenRouterManager
    :: IOE :> es => OpenRouterSettings es -> (OpenRouterSettings es -> Eff es a) -> Eff es a
withOpenRouterManager settings@OpenRouterSettings{httpManager} action = do
    manager <-
        maybe
            (liftIO (HTTP.newManager tlsManagerSettings{HTTP.managerRetryableException = const False}))
            pure
            httpManager
    action settings{httpManager = Just manager}

assembleRequest
    :: OpenRouterSettings es -> Text -> Object -> [Text] -> Object -> Either RakeError Value
assembleRequest OpenRouterSettings{provider} model settings reserved content = do
    when (T.null (T.strip model)) $ Left (LlmExpectationError "Model name must not be empty")
    forM_ ("model" : reserved) $ \key ->
        when (KM.member (Key.fromText key) settings)
            $ Left
                (LlmExpectationError ("Model settings cannot override request field: " <> toString key))
    merged <- mergeObjects settings (KM.insert "model" (String model) content)
    withProvider <-
        if KM.null provider
            then Right merged
            else mergeObjects merged (KM.singleton "provider" (Object provider))
    pure (Object withProvider)

mergeObjects :: Object -> Object -> Either RakeError Object
mergeObjects left right =
    foldM (\fields (key, value) -> mergeField (Right fields) key value) left (KM.toList right)
  where
    mergeField :: Either RakeError Object -> Key.Key -> Value -> Either RakeError Object
    mergeField accumulated key value = do
        fields <- accumulated
        case KM.lookup key fields of
            Nothing -> Right (KM.insert key value fields)
            Just (Object old) | Object new <- value -> do
                nested <- mergeObjects old new
                Right (KM.insert key (Object nested) fields)
            Just old | old == value -> Right fields
            Just _ -> Left (LlmExpectationError ("Conflicting request setting: " <> Key.toString key))
