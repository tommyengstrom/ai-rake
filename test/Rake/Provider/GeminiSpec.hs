module Rake.Provider.GeminiSpec where

import Control.Concurrent qualified as IO
import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import Network.HTTP.Types.Status (statusCode)
import ProviderStructuredSchemaBehaviorTests
import Rake
import Rake.MediaStorage.InMemory
import Rake.Providers.Gemini.Chat
import Rake.Storage.InMemory
import ProviderAgnosticTests
import Relude
import Servant.Client (ClientError (..), ResponseF (..))
import Test.Hspec

isTransientGeminiFailure :: RakeError -> Bool
isTransientGeminiFailure = \case
    LlmClientError (FailureResponse _ response) ->
        any (== statusCode (responseStatusCode response)) ([429, 500, 502, 503, 504] :: [Int])
    _ ->
        False

runEffectStack
    :: Text
    -> Eff
        '[ Rake
         , RakeStorage
         , Error ChatStorageError
         , RakeMediaStorage
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
                . runRakeMediaStorageInMemory
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

runEffectStackResult
    :: Text
    -> Eff
        '[ Rake
         , RakeStorage
         , Error ChatStorageError
         , RakeMediaStorage
         , Error RakeError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either RakeError a)
runEffectStackResult apiKey action = do
    go (3 :: Int)
  where
    settings = defaultGeminiChatSettings apiKey

    go attemptsRemaining = do
        result <-
            runEff
                . runConcurrent
                . runTime
                . runErrorNoCallStack @RakeError
                . runRakeMediaStorageInMemory
                . runErrorNoCallStackWith @ChatStorageError (error . show)
                . runRakeStorageInMemory
                $ runRakeGeminiChat settings action

        case result of
            Left rakeError
                | attemptsRemaining > 1 && isTransientGeminiFailure rakeError -> do
                    IO.threadDelay 500000
                    go (attemptsRemaining - 1)
                | otherwise ->
                    pure (Left rakeError)
            Right value ->
                pure (Right value)

imageFixture :: RakeMediaStorage :> es => ProviderImageFixture es
imageFixture =
    ProviderImageFixture
        { providerFamily = ProviderGeminiInteractions
        , registerImageFixture = \blobId ->
            saveMediaReference
                MediaProviderReference
                    { mediaBlobId = blobId
                    , providerFamily = ProviderGeminiInteractions
                    , providerRequestPart =
                        object
                            [ "type" .= ("image" :: Text)
                            , "uri"
                                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                                        :: Text
                                   )
                            ]
                    }
        }

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "GEMINI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "Rake Provider - Gemini" $
                it "requires GEMINI_API_KEY" $
                    pendingWith "Set GEMINI_API_KEY to run Gemini integration tests."
        Just apiKey ->
            describe "Rake Provider - Gemini" $ do
                specWithProvider imageFixture (runEffectStack (T.pack apiKey))
                specWithStructuredSchemaProvider allStructuredSchemasAccepted (runEffectStackResult (T.pack apiKey))
