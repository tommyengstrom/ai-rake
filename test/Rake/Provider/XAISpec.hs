module Rake.Provider.XAISpec where

import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import Rake
import Rake.Providers.XAI.Chat
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
    let settings = defaultXAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runRakeStorageInMemory
        $ runRakeXAIChat settings action

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "XAI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "Rake Provider - xAI" $
                it "requires XAI_API_KEY" $
                    pendingWith "Set XAI_API_KEY to run xAI integration tests."
        Just apiKey ->
            describe "Rake Provider - xAI" $
                specWithProvider (runEffectStack (T.pack apiKey))
