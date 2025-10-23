module LlmChat.WithStorageSpec where

import LlmChat.Effect
import LlmChat.Storage.Effect
import LlmChat.Storage.InMemory
import LlmChat.Types
import LlmChat.WithStorage
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Time
import Relude
import Test.Hspec

spec :: Spec
spec = describe "respondWithToolsSourceIOStorage" $ do
    it "Returns a stream with Either ChatStorageError StoredMsg" $ do
        -- Just verify the function can be called and returns the expected type
        result <- runTest
        case result of
            Left err -> expectationFailure $ "Expected Right but got LlmChatError: " <> show err
            Right (Left err) -> expectationFailure $ "Expected Right but got ChatStorageError: " <> show err
            Right (Right _) -> pure ()  -- Successfully created the stream

-- | Mock LlmChat effect that returns a simple assistant message
runMockLlmChat :: Eff (LlmChat ': es) a -> Eff es a
runMockLlmChat = interpret $ \_ -> \case
    GetLlmResponse _tools _format _msgs ->
        pure $ AssistantMsg
            { content = "Hello, this is a test response"
            , toolCalls = []
            }

-- | Run a test with in-memory storage and mock LLM
runTest :: IO (Either LlmChatError (Either ChatStorageError ()))
runTest =
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack @LlmChatError
        . runErrorNoCallStack @ChatStorageError
        . runLlmChatStorageInMemory
        . runMockLlmChat
        $ do
            convId <- createConversation "Test system prompt"
            _stream <- respondWithToolsSourceIOStorage Unstructured [] convId
            -- Just verify we can create the stream without errors
            pure ()
