module Rake.Provider.XAISpec where

import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import ProviderStructuredSchemaBehaviorTests
import Rake
import Rake.MediaStorage.InMemory
import Rake.Providers.XAI.Chat
import Rake.Storage.InMemory
import ProviderAgnosticTests
import Relude
import Test.Hspec

imageFixture :: RakeMediaStorage :> es => ProviderImageFixture es
imageFixture =
    ProviderImageFixture
        { providerFamily = ProviderXAIResponses
        , registerImageFixture = \blobId ->
            saveMediaReference
                MediaProviderReference
                    { mediaBlobId = blobId
                    , providerFamily = ProviderXAIResponses
                    , providerRequestPart =
                        object
                            [ "type" .= ("input_image" :: Text)
                            , "image_url"
                                .= ( "https://raw.githubusercontent.com/philschmid/gemini-samples/refs/heads/main/assets/cats-and-dogs.jpg"
                                        :: Text
                                   )
                            ]
                    }
        }

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
    let settings = defaultXAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStackWith @RakeError (error . show)
        . runRakeMediaStorageInMemory
        . runErrorNoCallStackWith @ChatStorageError (error . show)
        . runRakeStorageInMemory
        $ runRakeXAIChat settings action

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
    let settings = defaultXAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack @RakeError
        . runRakeMediaStorageInMemory
        . runErrorNoCallStackWith @ChatStorageError (error . show)
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
            describe "Rake Provider - xAI" $ do
                specWithProvider imageFixture (runEffectStack (T.pack apiKey))
                specWithStructuredSchemaProvider allStructuredSchemasAccepted (runEffectStackResult (T.pack apiKey))
