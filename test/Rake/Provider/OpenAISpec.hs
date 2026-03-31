module Rake.Provider.OpenAISpec where

import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Time
import ProviderStructuredSchemaBehaviorTests
import Rake
import Rake.MediaStorage.InMemory
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
         , RakeMediaStorage
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
        . runErrorNoCallStackWith @RakeError (error . show)
        . runRakeMediaStorageInMemory
        . runErrorNoCallStackWith @ChatStorageError (error . show)
        . runRakeStorageInMemory
        $ runRakeOpenAIChat settings action

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
    let settings = defaultOpenAIChatSettings apiKey
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack @RakeError
        . runRakeMediaStorageInMemory
        . runErrorNoCallStackWith @ChatStorageError (error . show)
        . runRakeStorageInMemory
        $ runRakeOpenAIChat settings action

structuredSchemaExpectations :: ProviderStructuredSchemaExpectations
structuredSchemaExpectations =
    allStructuredSchemasAccepted
        { nonNullarySumResponse = SchemaRejected "schema must be a JSON Schema of 'type: \"object\"'"
        , nullaryEnumResponse = SchemaRejected "schema must be a JSON Schema of 'type: \"object\"'"
        }

imageFixture :: RakeMediaStorage :> es => ProviderImageFixture es
imageFixture =
    ProviderImageFixture
        { providerFamily = ProviderOpenAIResponses
        , registerImageFixture = \blobId ->
            saveMediaReference
                MediaProviderReference
                    { mediaBlobId = blobId
                    , providerFamily = ProviderOpenAIResponses
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

spec :: Spec
spec = do
    maybeApiKey <- runIO (lookupEnv "OPENAI_API_KEY")
    case maybeApiKey of
        Nothing ->
            describe "Rake Provider - OpenAI" $
                it "requires OPENAI_API_KEY" $
                    pendingWith "Set OPENAI_API_KEY to run OpenAI integration tests."
        Just apiKey ->
            describe "Rake Provider - OpenAI" $ do
                specWithProvider imageFixture (runEffectStack (T.pack apiKey))
                specWithStructuredSchemaProvider structuredSchemaExpectations (runEffectStackResult (T.pack apiKey))
