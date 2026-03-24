module Rake.Provider.GeminiSpec where

import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import Rake
import Rake.Providers.Gemini.Chat
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
    let settings = defaultGeminiChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runRakeStorageInMemory
        $ runRakeGeminiChat settings action

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "GEMINI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "Rake Provider - Gemini" $
                it "requires GEMINI_API_KEY" $
                    pendingWith "Set GEMINI_API_KEY to run Gemini integration tests."
        Just apiKey ->
            describe "Rake Provider - Gemini" $
                specWithProvider (runEffectStack (T.pack apiKey))
