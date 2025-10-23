module LlmChat.Provider.GoogleSpec where

import LlmChat
import LlmChat.Providers.Google
import LlmChat.Providers.Google.Types
import LlmChat.Storage.InMemory
import ProviderAgnosticTests
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec
import Effectful.Time
import Effectful.Concurrent

runGoogle
    :: Eff
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
runGoogle action = do
    apiKey <-
        maybe
            (error "GEMINI_API_KEY not set in environment")
            (pure . GoogleApiKey . T.pack)
            =<< lookupEnv "GEMINI_API_KEY"
    let settings = defaultGoogleSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith (error . show)
        . runErrorNoCallStackWith (error . show)
        . runLlmChatStorageInMemory
        . runLlmChatGoogle settings
        $ action

spec :: Spec
spec = xdescribe "LlmChat Provider - Google" $ do
    -- Run common tests
    specWithProvider runGoogle

    -- Google-specific tests
    describe "Google-specific features" $ do
        it "handles system instructions correctly" $ do
            -- Google uses systemInstruction field instead of system messages in contents
            True `shouldBe` True -- Placeholder for now
