module Rake.Provider.OpenAISpec where

import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import Rake
import Rake.Providers.OpenAI.Chat
import Rake.Storage.InMemory
import ProviderAgnosticTests
import Relude
import Test.Hspec

runEffectStack
    :: Text
    -> Eff
        '[ Rake
         , RakeStorage
         , Error ChatStorageError
         , Error RakeError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO a
runEffectStack apiKey action = do
    let settings = defaultOpenAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runRakeStorageInMemory
        $ runRakeOpenAIChat settings action

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "OPENAI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "Rake Provider - OpenAI" $
                it "requires OPENAI_API_KEY" $
                    pendingWith "Set OPENAI_API_KEY to run OpenAI integration tests."
        Just apiKey ->
            describe "Rake Provider - OpenAI" $
                specWithProvider (runEffectStack (T.pack apiKey))
