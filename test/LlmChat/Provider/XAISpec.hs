module LlmChat.Provider.XAISpec where

import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import LlmChat
import LlmChat.Providers.XAI.Responses
import LlmChat.Storage.InMemory
import ProviderAgnosticTests
import Relude
import Test.Hspec

runEffectStack
    :: Text
    -> Eff
        '[ LlmChat
         , LlmChatStorage
         , Error ChatStorageError
         , Error LlmChatError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO a
runEffectStack apiKey action = do
    let settings = defaultXAIResponsesSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runLlmChatStorageInMemory
        $ runLlmChatXAIResponses settings action

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "XAI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "LlmChat Provider - xAI" $
                it "requires XAI_API_KEY" $
                    pendingWith "Set XAI_API_KEY to run xAI integration tests."
        Just apiKey ->
            describe "LlmChat Provider - xAI" $
                specWithProvider (runEffectStack (T.pack apiKey))
