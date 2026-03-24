module Rake.ChatSpec where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.IORef qualified as IORef
import Data.Map qualified as Map
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic (interpretWith)
import Effectful.Error.Static
import Effectful.Time (Time, runTime)
import Rake
import Rake.Storage.InMemory (runRakeStorageInMemory)
import Relude
import Test.Hspec

spec :: Spec
spec = describe "Rake" $ do
    describe "chat" $ do
        it "returns canonical history including provider tool calls, tool results, and final assistant output" $ do
            let pendingCall = loopToolCall "loop-1"
            result <-
                runMockChatWithRounds
                    [ toolProviderRound [responsesToolCallItem ItemPending pendingCall] [pendingCall]
                    , finalProviderRound [responsesAssistantItem ItemCompleted "{\"answer\":4}"]
                    ]
                    ( chat
                        defaultChatConfig
                            { maxToolRounds = 1
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    [ responsesToolCallItem ItemPending pendingCall
                    , toolResult "loop-1" "looped"
                    , responsesAssistantItem ItemCompleted "{\"answer\":4}"
                    ]

        it "fails when maxToolRounds is exceeded" $ do
            result <-
                runMockChatWithRounds
                    [ toolProviderRound [responsesToolCallItem ItemPending (loopToolCall "loop-1")] [loopToolCall "loop-1"]
                    , toolProviderRound [responsesToolCallItem ItemPending (loopToolCall "loop-2")] [loopToolCall "loop-2"]
                    , toolProviderRound [responsesToolCallItem ItemPending (loopToolCall "loop-3")] [loopToolCall "loop-3"]
                    ]
                    ( chat
                        defaultChatConfig
                            { maxToolRounds = 2
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result `shouldBe` Left (ToolLoopLimitExceeded 2)

        it "keeps chat strict for paused incomplete rounds" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "working on it"
            result <-
                runMockChatWithRounds
                    [ pausedProviderRound
                        (PauseIncomplete "Responses response status was incomplete")
                        [pendingAssistant]
                    ]
                    (chat defaultChatConfig [user "start"])

            result `shouldBe` Left (LlmExpectationError "Responses response status was incomplete")

        it "maps terminal provider failures onto ProviderTerminalFailure" $ do
            result <-
                runMockChatWithRounds
                    [ failedProviderRound
                        (FailureProvider "Responses response status was failed")
                        []
                    ]
                    (chat defaultChatConfig [user "start"])

            result `shouldBe` Left (ProviderTerminalFailure "Responses response status was failed")

        it "threads sampling options through every request round" $ do
            samplingRef <- IORef.newIORef []
            let samplingOptions =
                    defaultSamplingOptions
                        { temperature = Just 0
                        , topP = Just 0.2
                        }
                pendingCall = loopToolCall "loop-1"
            result <-
                runRecordedMockChat samplingRef
                    [ toolProviderRound [responsesToolCallItem ItemPending pendingCall] [pendingCall]
                    , finalProviderRound [responsesAssistantItem ItemCompleted "{\"answer\":4}"]
                    ]
                    ( chat
                        defaultChatConfig
                            { maxToolRounds = 1
                            , tools = [loopTool]
                            , sampling = samplingOptions
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    [ responsesToolCallItem ItemPending pendingCall
                    , toolResult "loop-1" "looped"
                    , responsesAssistantItem ItemCompleted "{\"answer\":4}"
                    ]
            recordedSampling <- IORef.readIORef samplingRef
            recordedSampling `shouldBe` [samplingOptions, samplingOptions]

    describe "chatOutcome" $ do
        it "returns paused outcomes with canonical pending history" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "working on it"
            result <-
                runMockChatOutcome
                    [ pausedProviderRound
                        (PauseIncomplete "Responses response status was incomplete")
                        [pendingAssistant]
                    ]
                    (chatOutcome defaultChatConfig [user "start"])

            result
                `shouldBe` Right
                    ChatPaused
                        { historyItems = [pendingAssistant]
                        , pauseReason = PauseIncomplete "Responses response status was incomplete"
                        }

        it "keeps mixed assistant and tool-call rounds in canonical history before a later pause" $ do
            let pendingCall = loopToolCall "loop-1"
                pendingAssistant = responsesAssistantItem ItemPending "working on it"
            result <-
                runMockChatOutcome
                    [ toolProviderRound
                        [ pendingAssistant
                        , responsesToolCallItem ItemPending pendingCall
                        ]
                        [pendingCall]
                    , pausedProviderRound (PauseProviderWaiting "Gemini interaction status was in_progress") []
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { maxToolRounds = 2
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatPaused
                        { historyItems =
                            [ pendingAssistant
                            , responsesToolCallItem ItemPending pendingCall
                            , toolResult "loop-1" "looped"
                            ]
                        , pauseReason = PauseProviderWaiting "Gemini interaction status was in_progress"
                        }

        it "keeps unresolved tool calls in canonical history on tool loop limit pauses" $ do
            let firstCall = loopToolCall "loop-1"
                secondCall = loopToolCall "loop-2"
            result <-
                runMockChatOutcome
                    [ toolProviderRound [responsesToolCallItem ItemPending firstCall] [firstCall]
                    , toolProviderRound [responsesToolCallItem ItemPending secondCall] [secondCall]
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { maxToolRounds = 1
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatPaused
                        { historyItems =
                            [ responsesToolCallItem ItemPending firstCall
                            , toolResult "loop-1" "looped"
                            , responsesToolCallItem ItemPending secondCall
                            ]
                        , pauseReason = PauseToolLoopLimit 1
                        }

        it "returns failed outcomes with canonical history items included" $ do
            result <-
                runMockChatOutcome
                    [ failedProviderRound
                        (FailureContract "Gemini interaction completed without tool calls or assistant message")
                        [geminiThought]
                    ]
                    (chatOutcome defaultChatConfig [user "start"])

            result
                `shouldBe` Right
                    ChatFailed
                        { historyItems = [geminiThought]
                        , failureReason = FailureContract "Gemini interaction completed without tool calls or assistant message"
                        }

        it "exposes canonical suffixes through chatOutcomeItems" $ do
            let pausedOutcome =
                    ChatPaused
                        { historyItems =
                            [ responsesToolCallItem ItemPending (loopToolCall "loop-1")
                            , toolResult "loop-1" "looped"
                            ]
                        , pauseReason = PauseToolLoopLimit 1
                        }
                failedOutcome =
                    ChatFailed
                        { historyItems = [geminiThought]
                        , failureReason = FailureContract "bad round"
                        }

            chatOutcomeItems pausedOutcome
                `shouldBe`
                    [ responsesToolCallItem ItemPending (loopToolCall "loop-1")
                    , toolResult "loop-1" "looped"
                    ]
            chatOutcomeItems failedOutcome
                `shouldBe` [geminiThought]

    describe "assistant helpers" $ do
        it "keeps lastAssistantTexts as a best-effort helper" $ do
            let history :: [HistoryItem]
                history =
                    [ assistantText "{\"answer\":4}"
                    , user "Tell me something else"
                    ]

            lastAssistantTexts history `shouldBe` ["{\"answer\":4}"]
            lastAssistantTextsStrict history `shouldBe` []

        it "decodeLastAssistantStrict only decodes the latest completed assistant tail" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                freshTurn :: [HistoryItem]
                freshTurn = [assistantText "{\"answer\":4}"]
                staleHistory = freshTurn <> [user "New question"]

            decodeLastAssistantStrict @Value freshTurn `shouldBe` Right assistantPayload
            decodeLastAssistantStrict @Value staleHistory
                `shouldBe` Left (LlmExpectationError "Assistant returned no message in latest turn")

        it "ignores pending assistant output in strict helpers" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "{\"answer\":4}"

            lastAssistantTextsStrict [pendingAssistant] `shouldBe` []
            decodeLastAssistantStrict @Value [pendingAssistant]
                `shouldBe` Left (LlmExpectationError "Assistant returned no message in latest turn")

        it "ignores pending assistant output in best-effort helpers too" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "{\"answer\":4}"

            lastAssistantTexts [pendingAssistant] `shouldBe` []
            decodeLastAssistant @Value [pendingAssistant]
                `shouldBe` Left (LlmExpectationError "Assistant returned no message")

        it "decodes multipart assistant text messages" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                response = [assistantParts [textPart "{\"answer\":", textPart "4}"]]

            decodeLastAssistantStrict @Value response `shouldBe` Right assistantPayload

    describe "withStorage and withStorageBy" $ do
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

        it "persists completed items from strict chat" $ do
            let finalAssistant = responsesAssistantItem ItemCompleted "Hello"
            result <-
                runStorageOutcomeTest
                    [ finalProviderRound [finalAssistant] ]
                    do
                        convId <- createConversation
                        appendItems convId [user "Hi"]
                        newItems <- withStorage (chat defaultChatConfig) convId
                        storedHistory <- getConversation convId
                        pure (newItems, storedHistory)

            result
                `shouldBe` Right
                    ( [finalAssistant]
                    , [user "Hi", finalAssistant]
                    )

        it "persists paused canonical history from chatOutcome" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "working on it"
            result <-
                runStorageOutcomeTest
                    [ pausedProviderRound
                        (PauseIncomplete "Responses response status was incomplete")
                        [pendingAssistant]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        outcome <-
                            withStorageBy
                                chatOutcomeItems
                                (chatOutcome defaultChatConfig)
                                convId
                        storedHistory <- getConversation convId
                        pure (outcome, storedHistory)

            result
                `shouldBe` Right
                    ( ChatPaused
                        { historyItems = [pendingAssistant]
                        , pauseReason = PauseIncomplete "Responses response status was incomplete"
                        }
                    , [user "start", pendingAssistant]
                    )

        it "persists unresolved tool calls on tool loop limit pauses" $ do
            let pendingCall = loopToolCall "loop-1"
                unresolvedCall = loopToolCall "loop-2"
                pendingCallItem = responsesToolCallItem ItemPending pendingCall
                unresolvedCallItem = responsesToolCallItem ItemPending unresolvedCall
            result <-
                runStorageOutcomeTest
                    [ toolProviderRound [pendingCallItem] [pendingCall]
                    , toolProviderRound [unresolvedCallItem] [unresolvedCall]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        outcome <-
                            withStorageBy
                                chatOutcomeItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { maxToolRounds = 1
                                        , tools = [loopTool]
                                        }
                                )
                                convId
                        storedHistory <- getConversation convId
                        pure (outcome, storedHistory)

            result
                `shouldBe` Right
                    ( ChatPaused
                        { historyItems =
                            [ pendingCallItem
                            , toolResult "loop-1" "looped"
                            , unresolvedCallItem
                            ]
                        , pauseReason = PauseToolLoopLimit 1
                        }
                    , [ user "start"
                      , pendingCallItem
                      , toolResult "loop-1" "looped"
                      , unresolvedCallItem
                      ]
                    )

        it "keeps strict assistant helpers blind to pending rounds after storage" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "{\"answer\":4}"
            result <-
                runStorageOutcomeTest
                    [ pausedProviderRound
                        (PauseIncomplete "Responses response status was incomplete")
                        [pendingAssistant]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        _ <-
                            withStorageBy
                                chatOutcomeItems
                                (chatOutcome defaultChatConfig)
                                convId
                        storedHistory <- getConversation convId
                        pure
                            ( lastAssistantTextsStrict storedHistory
                            , decodeLastAssistantStrict @Value storedHistory
                            )

            result
                `shouldBe` Right
                    ( []
                    , Left (LlmExpectationError "Assistant returned no message in latest turn")
                    )

        it "resumes stored pending tool calls before the next provider round" $ do
            historyRef <- IORef.newIORef []
            let pendingCall = loopToolCall "loop-1"
                pendingCallItem = responsesToolCallItem ItemPending pendingCall
                finalAssistant = responsesAssistantItem ItemCompleted "done"
            result <-
                runHistoryRecordedMockChat historyRef
                    [ finalProviderRound [finalAssistant] ]
                    ( chat
                        defaultChatConfig
                            { tools = [loopTool]
                            }
                        [user "start", pendingCallItem]
                    )

            result
                `shouldBe` Right
                    [ toolResult "loop-1" "looped"
                    , finalAssistant
                    ]
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ user "start"
                      , pendingCallItem
                      , toolResult "loop-1" "looped"
                      ]
                    ]

loopTool :: ToolDef es
loopTool =
    defineToolNoArgument
        "loop_tool"
        "Loop forever"
        (pure (Right "looped"))

runMockChatWithRounds
    :: [ProviderRound]
    -> Eff '[Rake, Error RakeError, IOE] a
    -> IO (Either RakeError a)
runMockChatWithRounds plannedRounds =
    runEff
        . runErrorNoCallStack
        . runMockRake plannedRounds

runMockChatOutcome
    :: [ProviderRound]
    -> Eff '[Rake, Error RakeError, IOE] a
    -> IO (Either RakeError a)
runMockChatOutcome =
    runMockChatWithRounds

runRecordedMockChat
    :: IORef.IORef [SamplingOptions]
    -> [ProviderRound]
    -> Eff '[Rake, Error RakeError, IOE] a
    -> IO (Either RakeError a)
runRecordedMockChat samplingRef plannedRounds =
    runEff
        . runErrorNoCallStack
        . runRecordedMockRake samplingRef plannedRounds

runHistoryRecordedMockChat
    :: IORef.IORef [[HistoryItem]]
    -> [ProviderRound]
    -> Eff '[Rake, Error RakeError, IOE] a
    -> IO (Either RakeError a)
runHistoryRecordedMockChat historyRef plannedRounds =
    runEff
        . runErrorNoCallStack
        . runHistoryRecordedMockRake historyRef plannedRounds

runMockRake
    :: IOE :> es
    => [ProviderRound]
    -> Eff (Rake ': es) a
    -> Eff es a
runMockRake plannedRounds eff = do
    responsesRef <- liftIO (IORef.newIORef plannedRounds)
    interpretWith eff \_ -> \case
        GetLlmResponse{} -> do
            remainingResponses <- liftIO (IORef.readIORef responsesRef)
            case remainingResponses of
                response : rest -> do
                    liftIO (IORef.writeIORef responsesRef rest)
                    pure response
                [] ->
                    pure (failedProviderRound (FailureContract "Unexpected provider round request in test") [])

runRecordedMockRake
    :: IOE :> es
    => IORef.IORef [SamplingOptions]
    -> [ProviderRound]
    -> Eff (Rake ': es) a
    -> Eff es a
runRecordedMockRake samplingRef plannedRounds eff = do
    responsesRef <- liftIO (IORef.newIORef plannedRounds)
    interpretWith eff \_ -> \case
        GetLlmResponse _ _ samplingOptions _history -> do
            liftIO $ IORef.modifyIORef' samplingRef (<> [samplingOptions])
            remainingResponses <- liftIO (IORef.readIORef responsesRef)
            case remainingResponses of
                response : rest -> do
                    liftIO (IORef.writeIORef responsesRef rest)
                    pure response
                [] ->
                    pure (failedProviderRound (FailureContract "Unexpected provider round request in test") [])

runHistoryRecordedMockRake
    :: IOE :> es
    => IORef.IORef [[HistoryItem]]
    -> [ProviderRound]
    -> Eff (Rake ': es) a
    -> Eff es a
runHistoryRecordedMockRake historyRef plannedRounds eff = do
    responsesRef <- liftIO (IORef.newIORef plannedRounds)
    interpretWith eff \_ -> \case
        GetLlmResponse _ _ _ history -> do
            liftIO $ IORef.modifyIORef' historyRef (<> [history])
            remainingResponses <- liftIO (IORef.readIORef responsesRef)
            case remainingResponses of
                response : rest -> do
                    liftIO (IORef.writeIORef responsesRef rest)
                    pure response
                [] ->
                    pure (failedProviderRound (FailureContract "Unexpected provider round request in test") [])

runStorageTest
    :: Eff
        '[ RakeStorage
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
        . runRakeStorageInMemory

runStorageOutcomeTest
    :: [ProviderRound]
    -> Eff
        '[ Rake
         , Error RakeError
         , RakeStorage
         , Error ChatStorageError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either ChatStorageError a)
runStorageOutcomeTest plannedRounds action =
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack
        . runRakeStorageInMemory
        $ do
            result <- runErrorNoCallStack @RakeError (runMockRake plannedRounds action)
            either (error . show) pure result

finalProviderRound :: [HistoryItem] -> ProviderRound
finalProviderRound historyItems =
    ProviderRound
        { historyItems
        , action = ProviderRoundDone
        }

toolProviderRound :: [HistoryItem] -> [ToolCall] -> ProviderRound
toolProviderRound historyItems toolCalls =
    ProviderRound
        { historyItems
        , action = ProviderRoundNeedsLocalTools toolCalls
        }

pausedProviderRound :: ChatPauseReason -> [HistoryItem] -> ProviderRound
pausedProviderRound pauseReason historyItems =
    ProviderRound
        { historyItems
        , action = ProviderRoundPaused pauseReason
        }

failedProviderRound :: ChatFailureReason -> [HistoryItem] -> ProviderRound
failedProviderRound failureReason historyItems =
    ProviderRound
        { historyItems
        , action = ProviderRoundFailed failureReason
        }

loopToolCall :: ToolCallId -> ToolCall
loopToolCall toolCallId =
    ToolCall
        { toolCallId
        , toolName = "loop_tool"
        , toolArgs = mempty
        }

responsesAssistantItem :: ItemLifecycle -> Text -> HistoryItem
responsesAssistantItem itemLifecycle textValue =
    HProvider
        ProviderHistoryItem
            { apiFamily = ProviderOpenAIResponses
            , itemLifecycle
            , nativeItem =
                NativeProviderItem
                    { exchangeId = Just "response-openai"
                    , nativeItemId = Just ("message-" <> lifecycleSuffix itemLifecycle)
                    , payload =
                        object
                            [ "id" .= ("native-message-" <> lifecycleSuffix itemLifecycle)
                            , "type" .= ("message" :: Text)
                            , "role" .= ("assistant" :: Text)
                            , "content"
                                .= ( [ object
                                            [ "type" .= ("output_text" :: Text)
                                            , "text" .= textValue
                                            ]
                                       ]
                                        :: [Value]
                                   )
                            ]
                    }
            }

responsesToolCallItem :: ItemLifecycle -> ToolCall -> HistoryItem
responsesToolCallItem itemLifecycle ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs} =
    HProvider
        ProviderHistoryItem
            { apiFamily = ProviderOpenAIResponses
            , itemLifecycle
            , nativeItem =
                NativeProviderItem
                    { exchangeId = Just "response-openai"
                    , nativeItemId = Just ("call-" <> toolCallId)
                    , payload =
                        object
                            [ "id" .= ("call-" <> toolCallId)
                            , "type" .= ("function_call" :: Text)
                            , "call_id" .= toolCallId
                            , "name" .= toolName
                            , "arguments" .= Object (KM.fromMap (Map.mapKeys fromText toolArgs))
                            ]
                    }
            }

lifecycleSuffix :: ItemLifecycle -> Text
lifecycleSuffix = \case
    ItemPending ->
        "pending"
    ItemCompleted ->
        "completed"

geminiThought :: HistoryItem
geminiThought =
    HProvider
        ProviderHistoryItem
            { apiFamily = ProviderGeminiInteractions
            , itemLifecycle = ItemPending
            , nativeItem =
                NativeProviderItem
                    { exchangeId = Just "interaction-gemini"
                    , nativeItemId = Just "thought-1"
                    , payload = object ["type" .= ("thought" :: Text)]
                    }
            }
