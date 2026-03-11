module LlmChat.ChatSpec where

import Data.Aeson
import Data.IORef qualified as IORef
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic (interpretWith)
import Effectful.Error.Static
import Effectful.Time (Time, runTime)
import LlmChat
import LlmChat.Storage.InMemory (runLlmChatStorageInMemory)
import Relude
import Test.Hspec

spec :: Spec
spec = describe "LlmChat" $ do
    describe "tool loop" $ do
        it "allows a final assistant response after tool rounds within the cap" $ do
            result <-
                runMockChat
                    [ [toolCall "loop-1" "loop_tool" mempty]
                    , [assistantText "{\"answer\":4}"]
                    ]
                    ( chat
                        defaultChatConfig
                            { maxToolRounds = 1
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result `shouldBe` Right [toolCall "loop-1" "loop_tool" mempty, toolResult "loop-1" "looped", assistantText "{\"answer\":4}"]

        it "fails when maxToolRounds is exceeded" $ do
            result <-
                runMockChat
                    [ [toolCall "loop-1" "loop_tool" mempty]
                    , [toolCall "loop-2" "loop_tool" mempty]
                    , [toolCall "loop-3" "loop_tool" mempty]
                    ]
                    ( chat
                        defaultChatConfig
                            { maxToolRounds = 2
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result `shouldBe` Left (ToolLoopLimitExceeded 2)

    describe "assistant helpers" $ do
        it "keeps lastAssistantTexts as a best-effort helper" $ do
            let history :: [HistoryItem]
                history =
                    [ assistantText "{\"answer\":4}"
                    , user "Tell me something else"
                    ]

            lastAssistantTexts history `shouldBe` ["{\"answer\":4}"]
            lastAssistantTextsStrict history `shouldBe` []

        it "decodeLastAssistantStrict only decodes the latest assistant tail" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                freshTurn :: [HistoryItem]
                freshTurn = [assistantText "{\"answer\":4}"]
                staleHistory = freshTurn <> [user "New question"]

            decodeLastAssistantStrict @Value freshTurn `shouldBe` Right assistantPayload
            decodeLastAssistantStrict @Value staleHistory
                `shouldBe` Left (LlmExpectationError "Assistant returned no message in latest turn")

        it "decodes multipart assistant text messages" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                response = [assistantParts [textPart "{\"answer\":", textPart "4}"]]

            decodeLastAssistantStrict @Value response `shouldBe` Right assistantPayload

    describe "withStorageBy" $ do
        it "strips an existing conversation prefix before appending" $ do
            result <- runStorageTest do
                convId <- createConversation
                let initialHistory = [system "You are concise.", user "Hi"]
                appendItems convId initialHistory
                fullHistory <-
                    withStorageBy
                        identity
                        (\conversation -> pure (conversation <> [assistantText "Hello"]))
                        convId
                storedHistory <- getConversation convId
                pure (fullHistory, storedHistory)

            result
                `shouldBe` Right
                    ( [system "You are concise.", user "Hi", assistantText "Hello"]
                    , [system "You are concise.", user "Hi", assistantText "Hello"]
                    )

        it "appends non-prefixed results unchanged" $ do
            result <- runStorageTest do
                convId <- createConversation
                appendItems convId [user "Hi"]
                _ <- withStorageBy identity (\_ -> pure [assistantText "Hello"]) convId
                getConversation convId

            result `shouldBe` Right [user "Hi", assistantText "Hello"]

loopTool :: ToolDef es
loopTool =
    defineToolNoArgument
        "loop_tool"
        "Loop forever"
        (pure (Right "looped"))

runMockChat
    :: [[HistoryItem]]
    -> Eff '[LlmChat, Error LlmChatError, IOE] a
    -> IO (Either LlmChatError a)
runMockChat plannedResponses =
    runEff
        . runErrorNoCallStack
        . runMockLlmChat plannedResponses

runMockLlmChat
    :: IOE :> es
    => [[HistoryItem]]
    -> Eff (LlmChat ': es) a
    -> Eff es a
runMockLlmChat plannedResponses eff = do
    responsesRef <- liftIO (IORef.newIORef plannedResponses)
    interpretWith eff \_ -> \case
        GetLlmResponse{} -> do
            remainingResponses <- liftIO (IORef.readIORef responsesRef)
            case remainingResponses of
                response : rest -> do
                    liftIO (IORef.writeIORef responsesRef rest)
                    pure response
                [] ->
                    pure []

runStorageTest
    :: Eff
        '[ LlmChatStorage
         , Error ChatStorageError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either ChatStorageError a)
runStorageTest =
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack
        . runLlmChatStorageInMemory
