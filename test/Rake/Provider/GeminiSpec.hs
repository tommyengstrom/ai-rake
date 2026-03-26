module Rake.Provider.GeminiSpec where

import Control.Concurrent qualified as IO
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import Network.HTTP.Types.Status (statusCode)
import Rake
import Rake.Providers.Gemini.Chat
import Rake.Storage.InMemory
import ProviderAgnosticTests
import Relude
import Servant.Client (ClientError (..), ResponseF (..))
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
    go (3 :: Int)
  where
    settings = defaultGeminiChatSettings apiKey

    go attemptsRemaining = do
        result <-
            runEff
                . runConcurrent
                . runTime
                . runErrorNoCallStack @RakeError
                . runErrorNoCallStack @ChatStorageError
                . runRakeStorageInMemory
                $ runRakeGeminiChat settings action

        case result of
            Left rakeError
                | attemptsRemaining > 1 && isTransientGeminiFailure rakeError -> do
                    IO.threadDelay 500000
                    go (attemptsRemaining - 1)
                | otherwise ->
                    error (show rakeError)
            Right (Left storageError) ->
                error (show storageError)
            Right (Right value) ->
                pure value

    isTransientGeminiFailure = \case
        LlmClientError (FailureResponse _ response) ->
            any (== statusCode (responseStatusCode response)) ([429, 500, 502, 503, 504] :: [Int])
        _ ->
            False

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
