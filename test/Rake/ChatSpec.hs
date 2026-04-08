module Rake.ChatSpec where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.IORef qualified as IORef
import Data.Map qualified as Map
import Data.UUID qualified as UUID
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Error.Static
import Effectful.Time (Time, runTime)
import Rake
import Rake.MediaStorage.InMemory (runRakeMediaStorageInMemory)
import Rake.Storage.InMemory (runRakeStorageInMemory)
import Relude
import Test.Hspec
import UnliftIO.Exception qualified as Exception

spec :: Spec
spec = describe "Rake" $ do
    describe "chatOutcome" $ do
        it "returns canonical history including provider tool calls, tool results, and final assistant output" $ do
            let pendingCall = loopToolCall "loop-1"
            result <-
                runMockChatOutcome
                    [ toolProviderRound [responsesToolCallItem ItemPending pendingCall] [pendingCall]
                    , finalProviderRound [responsesAssistantItem ItemCompleted "{\"answer\":4}"]
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
                    ChatFinished
                        { appendedItems =
                            [ responsesToolCallItem ItemPending pendingCall
                            , toolResult "loop-1" "looped"
                            , responsesAssistantItem ItemCompleted "{\"answer\":4}"
                            ]
                        }

        it "returns ConversationBlocked without a reset hint for id-less blocked histories" $ do
            let barrierMessage = "Responses response status was failed"
            result <-
                runMockChatOutcome
                    []
                    (chatOutcome defaultChatConfig [assistantText "{\"answer\":4}", replayBarrierItem barrierMessage])

            case result of
                Left (ConversationBlocked (ReplayBlocked reason) Nothing)
                    | reason == barrierMessage ->
                        pure ()
                other ->
                    expectationFailure ("Unexpected blocked-chat result: " <> show other)

        it "returns ConversationBlocked with a reset hint when the supplied history already has ids" $ do
            let barrierMessage = "Responses response status was failed"
                priorAnswer =
                    setHistoryItemId (Just (fixedHistoryItemId 1)) (assistantText "{\"answer\":4}")
                barrier =
                    setHistoryItemId (Just (fixedHistoryItemId 2)) (replayBarrierItem barrierMessage)
            result <-
                runMockChatOutcome
                    []
                    (chatOutcome defaultChatConfig [priorAnswer, barrier])

            case result of
                Left (ConversationBlocked (ReplayBlocked reason) (Just (ResetToItem checkpointId)))
                    | reason == barrierMessage
                    , checkpointId == fixedHistoryItemId 1 ->
                        pure ()
                other ->
                    expectationFailure ("Unexpected blocked-chat result with ids: " <> show other)

        it "threads sampling options through every request round" $ do
            samplingRef <- IORef.newIORef []
            let samplingOptions =
                    defaultSamplingOptions
                        { temperature = Just 0
                        , topP = Just 0.2
                        }
                pendingCall = loopToolCall "loop-1"
            result <-
                runRecordedMockChatOutcome samplingRef
                    [ toolProviderRound [responsesToolCallItem ItemPending pendingCall] [pendingCall]
                    , finalProviderRound [responsesAssistantItem ItemCompleted "{\"answer\":4}"]
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { maxToolRounds = 1
                            , tools = [loopTool]
                            , sampling = samplingOptions
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatFinished
                        { appendedItems =
                            [ responsesToolCallItem ItemPending pendingCall
                            , toolResult "loop-1" "looped"
                            , responsesAssistantItem ItemCompleted "{\"answer\":4}"
                            ]
                        }
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
                        { appendedItems = [pendingAssistant]
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
                        { appendedItems =
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
                        { appendedItems =
                            [ responsesToolCallItem ItemPending firstCall
                            , toolResult "loop-1" "looped"
                            , responsesToolCallItem ItemPending secondCall
                            ]
                        , pauseReason = PauseToolLoopLimit 1
                        }

        it "returns failed outcomes with canonical history items included" $ do
            let failureMessage =
                    "Gemini interaction completed without tool calls or assistant message"
            result <-
                runMockChatOutcome
                    [ failedProviderRound
                        (FailureContract failureMessage)
                        [geminiThought]
                    ]
                    (chatOutcome defaultChatConfig [user "start"])

            result
                `shouldBe` Right
                    ChatFailed
                        { appendedItems = [geminiThought, replayBarrierItem failureMessage]
                        , failureReason = FailureContract failureMessage
                        }

        it "emits replay barriers through onItem for failed outcomes" $ do
            let failureMessage = "Responses response status was failed"
            emittedItemsRef <- IORef.newIORef ([] :: [HistoryItem])
            result <-
                runMockChatOutcome
                    [ failedProviderRound
                        (FailureProvider failureMessage)
                        []
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { onItem = \historyItem -> liftIO (IORef.modifyIORef' emittedItemsRef (<> [historyItem]))
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatFailed
                        { appendedItems = [replayBarrierItem failureMessage]
                        , failureReason = FailureProvider failureMessage
                        }
            emittedItems <- IORef.readIORef emittedItemsRef
            stripHistoryItemIds emittedItems `shouldBe` [replayBarrierItem failureMessage]

        it "fails duplicate fresh tool-call rounds before any tool executes" $ do
            executionsRef <- IORef.newIORef (0 :: Int)
            let duplicatedCall = countingToolCall "dup-1"
                duplicatedCallItem = responsesToolCallItem ItemPending duplicatedCall
                failureMessage = duplicateToolCallFailureMessage "dup-1"
            result <-
                runMockChatOutcome
                    [ toolProviderRound
                        [duplicatedCallItem, duplicatedCallItem]
                        [duplicatedCall, duplicatedCall]
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { tools = [countingTool executionsRef]
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatFailed
                        { appendedItems =
                            [ duplicatedCallItem
                            , duplicatedCallItem
                            , replayBarrierItem failureMessage
                            ]
                        , failureReason = FailureContract failureMessage
                        }
            IORef.readIORef executionsRef `shouldReturn` 0

        it "fails fresh rounds whose declared tool calls do not match appended round history" $ do
            executionsRef <- IORef.newIORef (0 :: Int)
            let historyCall = countingToolCall "hist-1"
                declaredCall = countingToolCall "declared-1"
                historyCallItem = responsesToolCallItem ItemPending historyCall
            result <-
                runMockChatOutcome
                    [ toolProviderRound
                        [historyCallItem]
                        [declaredCall]
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { tools = [countingTool executionsRef]
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatFailed
                        { appendedItems =
                            [ historyCallItem
                            , replayBarrierItem toolCallProjectionMismatchFailureMessage
                            ]
                        , failureReason = FailureContract toolCallProjectionMismatchFailureMessage
                        }
            IORef.readIORef executionsRef `shouldReturn` 0

        it "allows fresh tool calls that reuse an already resolved tool call id" $ do
            executionsRef <- IORef.newIORef (0 :: Int)
            let repeatedCall = countingToolCall "reused-1"
                repeatedCallItem = responsesToolCallItem ItemPending repeatedCall
                finalAssistant = responsesAssistantItem ItemCompleted "done"
            result <-
                runMockChatOutcome
                    [ toolProviderRound [repeatedCallItem] [repeatedCall]
                    , finalProviderRound [finalAssistant]
                    ]
                    ( chatOutcome
                        defaultChatConfig
                            { tools = [countingTool executionsRef]
                            }
                        [ user "start"
                        , repeatedCallItem
                        , toolResult "reused-1" "already done"
                        ]
                    )

            result
                `shouldBe` Right
                    ChatFinished
                        { appendedItems =
                            [ repeatedCallItem
                            , toolResult "reused-1" "counted"
                            , finalAssistant
                            ]
                        }
            IORef.readIORef executionsRef `shouldReturn` 1

        it "exposes canonical suffixes through chatOutcomeAppendedItems" $ do
            let pausedOutcome =
                    ChatPaused
                        { appendedItems =
                            [ responsesToolCallItem ItemPending (loopToolCall "loop-1")
                            , toolResult "loop-1" "looped"
                            ]
                        , pauseReason = PauseToolLoopLimit 1
                        }
                failedOutcome =
                    ChatFailed
                        { appendedItems = [geminiThought, replayBarrierItem "bad round"]
                        , failureReason = FailureContract "bad round"
                        }

            chatOutcomeAppendedItems pausedOutcome
                `shouldBe`
                    [ responsesToolCallItem ItemPending (loopToolCall "loop-1")
                    , toolResult "loop-1" "looped"
                    ]
            chatOutcomeAppendedItems failedOutcome
                `shouldBe` [geminiThought, replayBarrierItem "bad round"]

    describe "streamChatOutcome" $ do
        it "emits assistant text deltas while only appending finalized history items" $ do
            streamedTextRef <- IORef.newIORef ([] :: [Text])
            emittedItemsRef <- IORef.newIORef ([] :: [HistoryItem])
            let finalAssistant = responsesAssistantItem ItemCompleted "Hello world"

            result <-
                runMockStreamChatOutcome
                    [ streamedProviderRound
                        [ MockTextDelta "Hello "
                        , MockTextDelta "world"
                        ]
                        (finalProviderRound [finalAssistant])
                    ]
                    ( streamChatOutcome
                        defaultStreamCallbacks
                            { onAssistantTextDelta = \deltaText -> liftIO (IORef.modifyIORef' streamedTextRef (<> [deltaText]))
                            }
                        defaultChatConfig
                            { onItem = \historyItem -> liftIO (IORef.modifyIORef' emittedItemsRef (<> [historyItem]))
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatFinished
                        { appendedItems = [finalAssistant]
                        }
            IORef.readIORef streamedTextRef `shouldReturn` ["Hello ", "world"]
            emittedItems <- IORef.readIORef emittedItemsRef
            stripHistoryItemIds emittedItems `shouldBe` [finalAssistant]

        it "streams assistant text from later rounds after local tool execution" $ do
            streamedTextRef <- IORef.newIORef ([] :: [Text])
            let pendingCall = loopToolCall "loop-1"
                pendingCallItem = responsesToolCallItem ItemPending pendingCall
                finalAssistant = responsesAssistantItem ItemCompleted "{\"answer\":4}"

            result <-
                runMockStreamChatOutcome
                    [ streamedProviderRound [] (toolProviderRound [pendingCallItem] [pendingCall])
                    , streamedProviderRound
                        [ MockTextDelta "{\"answer\":"
                        , MockTextDelta "4}"
                        ]
                        (finalProviderRound [finalAssistant])
                    ]
                    ( streamChatOutcome
                        defaultStreamCallbacks
                            { onAssistantTextDelta = \deltaText -> liftIO (IORef.modifyIORef' streamedTextRef (<> [deltaText]))
                            }
                        defaultChatConfig
                            { maxToolRounds = 1
                            , tools = [loopTool]
                            }
                        [user "start"]
                    )

            result
                `shouldBe` Right
                    ChatFinished
                        { appendedItems =
                            [ pendingCallItem
                            , toolResult "loop-1" "looped"
                            , finalAssistant
                            ]
                        }
            IORef.readIORef streamedTextRef `shouldReturn` ["{\"answer\":", "4}"]

        it "returns OnAssistantTextDeltaFailed when a stream text callback throws synchronously" $ do
            let finalAssistant = responsesAssistantItem ItemCompleted "Hello"

            result <-
                runMockStreamChatOutcome
                    [ streamedProviderRound
                        [MockTextDelta "Hello"]
                        (finalProviderRound [finalAssistant])
                    ]
                    ( streamChatOutcome
                        defaultStreamCallbacks
                            { onAssistantTextDelta = \_ -> error "closed socket"
                            }
                        defaultChatConfig
                        [user "start"]
                    )

            case result of
                Left (StreamingInternalError (OnAssistantTextDeltaFailed message)) -> do
                    message `shouldContain` "closed socket"
                other ->
                    expectationFailure ("Unexpected streaming callback failure result: " <> show other)

        it "returns OnAssistantRefusalDeltaFailed when a stream refusal callback throws synchronously" $ do
            let finalAssistant = responsesAssistantItem ItemCompleted "No"

            result <-
                runMockStreamChatOutcome
                    [ streamedProviderRound
                        [MockRefusalDelta "No"]
                        (finalProviderRound [finalAssistant])
                    ]
                    ( streamChatOutcome
                        defaultStreamCallbacks
                            { onAssistantRefusalDelta = \_ -> error "closed refusal socket"
                            }
                        defaultChatConfig
                        [user "start"]
                    )

            case result of
                Left (StreamingInternalError (OnAssistantRefusalDeltaFailed message)) -> do
                    message `shouldContain` "closed refusal socket"
                other ->
                    expectationFailure ("Unexpected streaming refusal callback failure result: " <> show other)

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

        it "treats replay barriers as blocking in strict helpers" $ do
            let barrierMessage = "Responses response status was failed"
                history =
                    [ assistantText "{\"answer\":4}"
                    , replayBarrierItem barrierMessage
                    ]

            lastAssistantTextsStrict history `shouldBe` []
            decodeLastAssistantStrict @Value history
                `shouldBe` Left (ConversationBlocked (ReplayBlocked barrierMessage) Nothing)

        it "ignores pending assistant output in best-effort helpers too" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "{\"answer\":4}"

            lastAssistantTexts [pendingAssistant] `shouldBe` []
            decodeLastAssistant @Value [pendingAssistant]
                `shouldBe` Left (LlmExpectationError "Assistant returned no message")

        it "decodes multipart assistant text messages" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                response = [assistantParts [textPart "{\"answer\":", textPart "4}"]]

            decodeLastAssistantStrict @Value response `shouldBe` Right assistantPayload

        it "treats refusal parts as assistant text in strict helpers" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                response = [assistantParts [refusalPart "{\"answer\":4}"]]

            lastAssistantTextsStrict response `shouldBe` ["{\"answer\":4}"]
            decodeLastAssistantStrict @Value response `shouldBe` Right assistantPayload

        it "ignores media parts in assistant text helpers" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                response =
                    [ assistantParts
                        [ imagePart "blob-image-1" (Just "image/png") (Just "diagram")
                        , textPart "{\"answer\":4}"
                        ]
                    ]

            lastAssistantTextsStrict response `shouldBe` ["{\"answer\":4}"]
            decodeLastAssistantStrict @Value response `shouldBe` Right assistantPayload

        it "ignores trailing completed non-portable artifacts in strict helpers" $ do
            let assistantPayload = object ["answer" .= (4 :: Int)]
                completedGeminiThought =
                    nonPortableHistoryItem
                        ItemCompleted
                        ProviderItem
                            { apiFamily = ProviderGeminiInteractions
                            , exchangeId = Just "interaction-gemini"
                            , nativeItemId = Just "thought-1"
                            , payload = object ["type" .= ("thought" :: Text)]
                            }
                history = [assistantText "{\"answer\":4}", completedGeminiThought]

            lastAssistantTextsStrict history `shouldBe` ["{\"answer\":4}"]
            decodeLastAssistantStrict @Value history `shouldBe` Right assistantPayload

    describe "filterUnavailableMediaHistory" $ do
        it "drops unavailable media parts but keeps remaining text" $ do
            let history =
                    [ HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem =
                            GenericMessage
                                { role = GenericAssistant
                                , parts =
                                    [ textPart "hello"
                                    , imagePart "blob-missing" (Just "image/png") (Just "diagram")
                                    ]
                                }
                        , providerItem =
                            Just
                                ProviderItem
                                    { apiFamily = ProviderOpenAIResponses
                                    , exchangeId = Just "response-openai"
                                    , nativeItemId = Just "item-openai"
                                    , payload = object ["type" .= ("message" :: Text)]
                                    }
                        }
                    ]

            filteredHistory <-
                runEff
                    . runRakeMediaStorageInMemory
                    $ filterUnavailableMediaHistory ProviderOpenAIResponses history

            filteredHistory
                `shouldBe`
                    [ HistoryItem
                        { historyItemIdField = Nothing
                        , itemLifecycle = ItemCompleted
                        , genericItem = GenericMessage{role = GenericAssistant, parts = [textPart "hello"]}
                        , providerItem = Nothing
                        }
                    ]

        it "drops media-only messages when no media ref exists" $ do
            let history =
                    [ userParts [imagePart "blob-missing" (Just "image/png") (Just "diagram")]
                    , assistantParts [filePart "blob-missing-file" Nothing (Just "notes.txt")]
                    ]

            filteredHistory <-
                runEff
                    . runRakeMediaStorageInMemory
                    $ filterUnavailableMediaHistory ProviderOpenAIResponses history

            filteredHistory `shouldBe` []

        it "leaves history unchanged when all required media refs exist" $ do
            let history =
                    [ assistantParts
                        [ textPart "hello"
                        , imagePart "blob-present" (Just "image/png") (Just "diagram")
                        ]
                    ]

            filteredHistory <-
                runEff
                    . runRakeMediaStorageInMemory
                    $ do
                        saveMediaReference
                            MediaProviderReference
                                { mediaBlobId = "blob-present"
                                , providerFamily = ProviderOpenAIResponses
                                , providerRequestPart =
                                    object
                                        [ "type" .= ("input_image" :: Text)
                                        , "image_url" .= ("https://example.com/present.png" :: Text)
                                        ]
                                }
                        filterUnavailableMediaHistory ProviderOpenAIResponses history

            filteredHistory `shouldBe` history

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
                pure (stripHistoryItemIds fullHistory, stripHistoryItemIds storedHistory)

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
                stripHistoryItemIds <$> getConversation convId

            result `shouldBe` Right [user "Hi", assistantText "Hello"]

        it "does not strip a prefix when the embedded history ids disagree" $ do
            result <- runStorageTest do
                convId <- createConversation
                appendItems convId [user "Hi"]
                _ <-
                    withStorageBy
                        identity
                        ( \conversation ->
                            pure
                                ( [ setHistoryItemId (Just (fixedHistoryItemId 99)) historyItem
                                  | historyItem <- conversation
                                  ]
                                    <> [assistantText "Hello"]
                                )
                        )
                        convId
                getConversation convId

            case result of
                Right [firstItem, secondItem, thirdItem] -> do
                    setHistoryItemId Nothing firstItem `shouldBe` user "Hi"
                    secondItem `shouldBe` setHistoryItemId (Just (fixedHistoryItemId 99)) (user "Hi")
                    setHistoryItemId Nothing thirdItem `shouldBe` assistantText "Hello"
                other ->
                    expectationFailure ("Unexpected stored history: " <> show other)

        it "returns stored history with embedded ids matching storage ids" $ do
            result <- runStorageTest do
                convId <- createConversation
                appendItems convId [user "Hi", assistantText "Hello"]
                storedConversation <- getStoredConversation convId
                history <- getConversation convId
                pure (storedConversation, history)

            case result of
                Right (storedConversation, history) -> do
                    fmap historyItemId history
                        `shouldBe` [Just storedItemId | StoredItem{itemId = storedItemId} <- storedConversation]
                Left err ->
                    expectationFailure ("Unexpected storage failure: " <> show err)

        it "persists completed items from withResumableChat" $ do
            let finalAssistant = responsesAssistantItem ItemCompleted "Hello"
            result <-
                runStorageOutcomeTest
                    [ finalProviderRound [finalAssistant] ]
                    do
                        convId <- createConversation
                        appendItems convId [user "Hi"]
                        outcome <- withResumableChat defaultChatConfig convId
                        storedHistory <- getConversation convId
                        pure (stripHistoryItemIdsFromOutcome outcome, stripHistoryItemIds storedHistory)

            result
                `shouldBe` Right
                    ( ChatFinished{appendedItems = [finalAssistant]}
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
                                chatOutcomeAppendedItems
                                (chatOutcome defaultChatConfig)
                                convId
                        storedHistory <- getConversation convId
                        pure (stripHistoryItemIdsFromOutcome outcome, stripHistoryItemIds storedHistory)

            result
                `shouldBe` Right
                    ( ChatPaused
                        { appendedItems = [pendingAssistant]
                        , pauseReason = PauseIncomplete "Responses response status was incomplete"
                        }
                    , [user "start", pendingAssistant]
                    )

        it "persists failed rounds with an explicit replay barrier" $ do
            let failureMessage = "Responses response status was failed"
            result <-
                runStorageOutcomeTest
                    [ failedProviderRound
                        (FailureProvider failureMessage)
                        []
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        outcome <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                (chatOutcome defaultChatConfig)
                                convId
                        storedHistory <- getConversation convId
                        pure (stripHistoryItemIdsFromOutcome outcome, stripHistoryItemIds storedHistory)

            result
                `shouldBe` Right
                    ( ChatFailed
                        { appendedItems = [replayBarrierItem failureMessage]
                        , failureReason = FailureProvider failureMessage
                        }
                    , [user "start", replayBarrierItem failureMessage]
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
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { maxToolRounds = 1
                                        , tools = [loopTool]
                                        }
                                )
                                convId
                        storedHistory <- getConversation convId
                        pure (stripHistoryItemIdsFromOutcome outcome, stripHistoryItemIds storedHistory)

            result
                `shouldBe` Right
                    ( ChatPaused
                        { appendedItems =
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
                                chatOutcomeAppendedItems
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
                runHistoryRecordedMockChatOutcome historyRef
                    [ finalProviderRound [finalAssistant] ]
                    ( chatOutcome
                        defaultChatConfig
                            { tools = [loopTool]
                            }
                        [user "start", pendingCallItem]
                    )

            result
                `shouldBe` Right
                    ChatFinished
                        { appendedItems =
                            [ toolResult "loop-1" "looped"
                            , finalAssistant
                            ]
                        }
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ user "start"
                      , pendingCallItem
                      , toolResult "loop-1" "looped"
                      ]
                    ]

        it "withResumableChat matches the raw durable storage wrapper" $ do
            let pendingAssistant = responsesAssistantItem ItemPending "working on it"
                pauseReason = PauseIncomplete "Responses response status was incomplete"
                expectedOutcome =
                    ChatPaused
                        { appendedItems = [pendingAssistant]
                        , pauseReason
                        }
                expectedStoredHistory = [user "start", pendingAssistant]
            result <-
                runStorageOutcomeTest
                    [ pausedProviderRound pauseReason [pendingAssistant]
                    , pausedProviderRound pauseReason [pendingAssistant]
                    ]
                    do
                        resumableConversationId <- createConversation
                        appendItems resumableConversationId [user "start"]
                        resumableOutcome <- withResumableChat defaultChatConfig resumableConversationId
                        resumableHistory <- stripHistoryItemIds <$> getConversation resumableConversationId

                        rawConversationId <- createConversation
                        appendItems rawConversationId [user "start"]
                        rawOutcome <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                (chatOutcome defaultChatConfig)
                                rawConversationId
                        rawHistory <- stripHistoryItemIds <$> getConversation rawConversationId

                        pure
                            ( stripHistoryItemIdsFromOutcome resumableOutcome
                            , resumableHistory
                            , stripHistoryItemIdsFromOutcome rawOutcome
                            , rawHistory
                            )

            result
                `shouldBe` Right
                    ( expectedOutcome
                    , expectedStoredHistory
                    , expectedOutcome
                    , expectedStoredHistory
                    )

        it "persists only finalized items from withResumableStreamingChat" $ do
            streamedTextRef <- IORef.newIORef ([] :: [Text])
            let finalAssistant = responsesAssistantItem ItemCompleted "Hello"
            result <-
                runStorageStreamOutcomeTest
                    [ streamedProviderRound
                        [ MockTextDelta "Hel"
                        , MockTextDelta "lo"
                        ]
                        (finalProviderRound [finalAssistant])
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        outcome <-
                            withResumableStreamingChat
                                defaultStreamCallbacks
                                    { onAssistantTextDelta = \deltaText -> liftIO (IORef.modifyIORef' streamedTextRef (<> [deltaText]))
                                    }
                                defaultChatConfig
                                convId
                        storedHistory <- getConversation convId
                        pure (stripHistoryItemIdsFromOutcome outcome, stripHistoryItemIds storedHistory)

            result
                `shouldBe` Right
                    ( ChatFinished{appendedItems = [finalAssistant]}
                    , [user "start", finalAssistant]
                    )
            IORef.readIORef streamedTextRef `shouldReturn` ["Hel", "lo"]

loopTool :: ToolDef es
loopTool =
    defineToolNoArgument
        "loop_tool"
        "Loop forever"
        (pure (Right "looped"))

countingTool :: IOE :> es => IORef.IORef Int -> ToolDef es
countingTool executionsRef =
    defineToolNoArgument
        "counting_tool"
        "Counts executions"
        do
            liftIO $ IORef.modifyIORef' executionsRef (+ 1)
            pure (Right "counted")

countingToolCall :: ToolCallId -> ToolCall
countingToolCall toolCallId =
    ToolCall
        { toolCallId
        , toolName = "counting_tool"
        , toolArgs = mempty
        , continuationAttachments = []
        }

data MockStreamDelta
    = MockTextDelta Text
    | MockRefusalDelta Text

data MockStreamingRound = MockStreamingRound
    { streamDeltas :: [MockStreamDelta]
    , streamRound :: ProviderRound
    }

streamedProviderRound :: [MockStreamDelta] -> ProviderRound -> MockStreamingRound
streamedProviderRound streamDeltas streamRound =
    MockStreamingRound{streamDeltas, streamRound}

runMockChatOutcome
    :: [ProviderRound]
    -> Eff '[Rake, RakeMediaStorage, Error RakeError, IOE] ChatOutcome
    -> IO (Either RakeError ChatOutcome)
runMockChatOutcome plannedRounds action =
    fmap (fmap stripHistoryItemIdsFromOutcome) $
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            . runMockRake plannedRounds
            $ action

runMockStreamChatOutcome
    :: [MockStreamingRound]
    -> Eff '[Rake, RakeMediaStorage, Error RakeError, IOE] ChatOutcome
    -> IO (Either RakeError ChatOutcome)
runMockStreamChatOutcome plannedRounds action =
    fmap (fmap stripHistoryItemIdsFromOutcome) $
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            . runMockStreamingRake plannedRounds
            $ action

runRecordedMockChatOutcome
    :: IORef.IORef [SamplingOptions]
    -> [ProviderRound]
    -> Eff '[Rake, RakeMediaStorage, Error RakeError, IOE] ChatOutcome
    -> IO (Either RakeError ChatOutcome)
runRecordedMockChatOutcome samplingRef plannedRounds action =
    fmap (fmap stripHistoryItemIdsFromOutcome) $
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            . runRecordedMockRake samplingRef plannedRounds
            $ action

runHistoryRecordedMockChatOutcome
    :: IORef.IORef [[HistoryItem]]
    -> [ProviderRound]
    -> Eff '[Rake, RakeMediaStorage, Error RakeError, IOE] ChatOutcome
    -> IO (Either RakeError ChatOutcome)
runHistoryRecordedMockChatOutcome historyRef plannedRounds action =
    fmap (fmap stripHistoryItemIdsFromOutcome) $
        runEff
            . runErrorNoCallStack
            . runRakeMediaStorageInMemory
            . runHistoryRecordedMockRake historyRef plannedRounds
            $ action

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
        GetLlmResponseStream{} ->
            pure (failedProviderRound (FailureContract "Unexpected streaming provider round request in non-streaming test") [])

runMockStreamingRake
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => [MockStreamingRound]
    -> Eff (Rake ': es) a
    -> Eff es a
runMockStreamingRake plannedRounds eff = do
    responsesRef <- liftIO (IORef.newIORef plannedRounds)
    interpretWith eff \localEnv -> \case
        GetLlmResponse{} ->
            pure (failedProviderRound (FailureContract "Unexpected non-streaming provider round request in streaming test") [])
        GetLlmResponseStream onAssistantTextDelta onAssistantRefusalDelta _ _ _ _ -> do
            localSeqUnlift localEnv \unlift -> do
                remainingResponses <- liftIO (IORef.readIORef responsesRef)
                case remainingResponses of
                    MockStreamingRound{streamDeltas, streamRound} : rest -> do
                        liftIO (IORef.writeIORef responsesRef rest)
                        let streamCallbacks =
                                protectMockStreamCallbacks
                                    StreamCallbacks
                                        { onAssistantTextDelta = unlift . onAssistantTextDelta
                                        , onAssistantRefusalDelta = unlift . onAssistantRefusalDelta
                                        }
                        traverse_ (emitMockStreamDelta streamCallbacks) streamDeltas
                        pure streamRound
                    [] ->
                        pure (failedProviderRound (FailureContract "Unexpected streaming provider round request in test") [])

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
        GetLlmResponseStream{} ->
            pure (failedProviderRound (FailureContract "Unexpected streaming provider round request in non-streaming test") [])

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
            liftIO $ IORef.modifyIORef' historyRef (<> [stripHistoryItemIds history])
            remainingResponses <- liftIO (IORef.readIORef responsesRef)
            case remainingResponses of
                response : rest -> do
                    liftIO (IORef.writeIORef responsesRef rest)
                    pure response
                [] ->
                    pure (failedProviderRound (FailureContract "Unexpected provider round request in test") [])
        GetLlmResponseStream{} ->
            pure (failedProviderRound (FailureContract "Unexpected streaming provider round request in non-streaming test") [])

emitMockStreamDelta :: StreamCallbacks es -> MockStreamDelta -> Eff es ()
emitMockStreamDelta StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} = \case
    MockTextDelta deltaText ->
        onAssistantTextDelta deltaText
    MockRefusalDelta refusalText ->
        onAssistantRefusalDelta refusalText

protectMockStreamCallbacks
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => StreamCallbacks es
    -> StreamCallbacks es
protectMockStreamCallbacks StreamCallbacks{onAssistantTextDelta, onAssistantRefusalDelta} =
    StreamCallbacks
        { onAssistantTextDelta =
            \deltaText ->
                protectMockStreamingInternalAction
                    OnAssistantTextDeltaFailed
                    (onAssistantTextDelta deltaText)
        , onAssistantRefusalDelta =
            \refusalText ->
                protectMockStreamingInternalAction
                    OnAssistantRefusalDeltaFailed
                    (onAssistantRefusalDelta refusalText)
        }

protectMockStreamingInternalAction
    :: ( IOE :> es
       , Error RakeError :> es
       )
    => (String -> StreamingInternalIssue)
    -> Eff es a
    -> Eff es a
protectMockStreamingInternalAction wrapIssue action = do
    result <-
        withEffToIO SeqUnlift \runInIO ->
            Exception.tryAny $
                runInIO
                    ( (Right <$> action)
                        `catchError` (\_ (rakeError :: RakeError) -> pure (Left rakeError))
                    )
    case result of
        Left err ->
            throwError (StreamingInternalError (wrapIssue (displayException err)))
        Right (Left rakeError) ->
            throwError @RakeError rakeError
        Right (Right value) ->
            pure value

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
         , RakeMediaStorage
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
        . runRakeMediaStorageInMemory
        . runErrorNoCallStack
        . runRakeStorageInMemory
        $ do
            result <- runErrorNoCallStack @RakeError (runMockRake plannedRounds action)
            either (error . show) pure result

runStorageStreamOutcomeTest
    :: [MockStreamingRound]
    -> Eff
        '[ Rake
         , Error RakeError
         , RakeStorage
         , Error ChatStorageError
         , RakeMediaStorage
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either ChatStorageError a)
runStorageStreamOutcomeTest plannedRounds action =
    runEff
        . runConcurrent
        . runTime
        . runRakeMediaStorageInMemory
        . runErrorNoCallStack
        . runRakeStorageInMemory
        $ do
            result <- runErrorNoCallStack @RakeError (runMockStreamingRake plannedRounds action)
            either (error . show) pure result

finalProviderRound :: [HistoryItem] -> ProviderRound
finalProviderRound roundItems =
    ProviderRound
        { roundItems
        , mediaReferences = []
        , action = ProviderRoundDone
        }

toolProviderRound :: [HistoryItem] -> [ToolCall] -> ProviderRound
toolProviderRound roundItems toolCalls =
    ProviderRound
        { roundItems
        , mediaReferences = []
        , action = ProviderRoundNeedsLocalTools toolCalls
        }

pausedProviderRound :: ChatPauseReason -> [HistoryItem] -> ProviderRound
pausedProviderRound pauseReason roundItems =
    ProviderRound
        { roundItems
        , mediaReferences = []
        , action = ProviderRoundPaused pauseReason
        }

failedProviderRound :: ChatFailureReason -> [HistoryItem] -> ProviderRound
failedProviderRound failureReason roundItems =
    ProviderRound
        { roundItems
        , mediaReferences = []
        , action = ProviderRoundFailed failureReason
        }

loopToolCall :: ToolCallId -> ToolCall
loopToolCall toolCallId =
    ToolCall
        { toolCallId
        , toolName = "loop_tool"
        , toolArgs = mempty
        , continuationAttachments = []
        }

responsesAssistantItem :: ItemLifecycle -> Text -> HistoryItem
responsesAssistantItem lifecycle textValue =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = lifecycle
        , genericItem = GenericMessage{role = GenericAssistant, parts = [PartText textValue]}
        , providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderOpenAIResponses
                    , exchangeId = Just "response-openai"
                    , nativeItemId = Just ("message-" <> lifecycleSuffix lifecycle)
                    , payload =
                        object
                            [ "id" .= ("native-message-" <> lifecycleSuffix lifecycle)
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
responsesToolCallItem lifecycle ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs, continuationAttachments} =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = lifecycle
        , genericItem =
            GenericToolCall
                { toolCall =
                    ToolCall
                        { toolCallId = ToolCallId toolCallId
                        , toolName
                        , toolArgs
                        , continuationAttachments
                        }
                }
        , providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderOpenAIResponses
                    , exchangeId = Just "response-openai"
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
    nonPortableHistoryItem
        ItemPending
        ProviderItem
            { apiFamily = ProviderGeminiInteractions
            , exchangeId = Just "interaction-gemini"
            , nativeItemId = Just "thought-1"
            , payload = object ["type" .= ("thought" :: Text)]
            }

replayBarrierItem :: Text -> HistoryItem
replayBarrierItem reason =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemCompleted
        , genericItem = GenericReplayBarrier{reason}
        , providerItem = Nothing
        }

duplicateToolCallFailureMessage :: ToolCallId -> Text
duplicateToolCallFailureMessage (ToolCallId toolCallIdText) =
    "Active conversation contains multiple unresolved tool calls with id "
        <> toolCallIdText
        <> ". Append resetTo to rewind before continuing."

toolCallProjectionMismatchFailureMessage :: Text
toolCallProjectionMismatchFailureMessage =
    "Provider round toolCalls did not match the unresolved tool calls implied by appended round history"

stripHistoryItemIds :: [HistoryItem] -> [HistoryItem]
stripHistoryItemIds =
    map (setHistoryItemId Nothing)

stripHistoryItemIdsFromOutcome :: ChatOutcome -> ChatOutcome
stripHistoryItemIdsFromOutcome = \case
    ChatFinished{appendedItems} ->
        ChatFinished{appendedItems = stripHistoryItemIds appendedItems}
    ChatPaused{appendedItems, pauseReason} ->
        ChatPaused{appendedItems = stripHistoryItemIds appendedItems, pauseReason}
    ChatFailed{appendedItems, failureReason} ->
        ChatFailed{appendedItems = stripHistoryItemIds appendedItems, failureReason}

fixedHistoryItemId :: Word32 -> HistoryItemId
fixedHistoryItemId suffix =
    HistoryItemId (UUID.fromWords 0 0 0 suffix)
