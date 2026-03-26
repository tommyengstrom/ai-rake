{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rake.ReplaySpec where

import Data.Aeson
import Data.IORef qualified as IORef
import Data.UUID qualified as UUID
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static
import Effectful.Time (Time, runTime)
import Rake
import Rake.ChatSpec qualified as Chat
import Rake.Provider.ResponsesRenderSpec qualified as Render
import Rake.Storage.InMemory (runRakeStorageInMemory)
import Relude
import Test.Hspec

spec :: Spec
spec = describe "Rake.Replay" $ do
    describe "conversationReplayState" $ do
        it "tracks the active logical branch across resets" $ do
            let start = withHistoryId 1 (user "start")
                stale = withHistoryId 2 (assistantText "stale")
                again = withHistoryId 3 (user "again")
                history =
                    [ start
                    , stale
                    , resetTo (itemIdOf start)
                    , again
                    ]
                ReplayState{activeHistory, replayHistory, resumableToolCalls, pendingArtifacts, blocked} =
                    conversationReplayState history

            activeHistory `shouldBe` [start, again]
            replayHistory `shouldBe` [start, again]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` []
            blocked `shouldBe` Nothing

        it "keeps resolved tool exchanges replayable while suppressing pending assistant artifacts" $ do
            let pendingAssistant = Chat.responsesAssistantItem ItemPending "working on it"
                pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                replayState =
                    conversationReplayState
                        [ user "start"
                        , pendingAssistant
                        , pendingCallItem
                        , toolResult "loop-1" "looped"
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory
                `shouldBe`
                    [ user "start"
                    , pendingCallItem
                    , toolResult "loop-1" "looped"
                    ]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` [pendingAssistant]
            blocked `shouldBe` Nothing

        it "does not let an earlier stray tool result resolve a later pending tool call with the same id" $ do
            let pendingCall = Chat.responsesToolCallItem ItemPending (Chat.loopToolCall "loop-1")
                replayState =
                    conversationReplayState
                        [ user "start"
                        , toolResultText "loop-1" "stale result"
                        , pendingCall
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory `shouldBe` [user "start"]
            resumableToolCalls `shouldBe` [Chat.loopToolCall "loop-1"]
            pendingArtifacts `shouldBe` [pendingCall]
            blocked `shouldBe` Nothing

        it "blocks replay when the active branch contains duplicate unresolved tool call ids" $ do
            let firstPendingCall = Chat.responsesToolCallItem ItemPending (Chat.loopToolCall "loop-1")
                secondPendingCall = Chat.responsesToolCallItem ItemPending (Chat.loopToolCall "loop-1")
                replayState =
                    conversationReplayState
                        [ user "start"
                        , firstPendingCall
                        , secondPendingCall
                        ]
                ReplayState{blocked} = replayState

            blocked
                `shouldBe` Just
                    (ReplayBlocked "Active conversation contains multiple unresolved tool calls with id loop-1. Append resetTo to rewind before continuing.")

        it "keeps stray tool results in the log but excludes them from replay and pending artifacts" $ do
            let strayToolResult = toolResultText "loop-1" "stale result"
                replayState =
                    conversationReplayState
                        [ user "start"
                        , strayToolResult
                        ]
                ReplayState{activeHistory, replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            activeHistory `shouldBe` [user "start", strayToolResult]
            replayHistory `shouldBe` [user "start"]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` []
            blocked `shouldBe` Nothing

        it "replays Gemini thoughts for resolved same-provider tool continuations" $ do
            let pendingThought =
                    Render.nativeHistoryItem
                        ProviderGeminiInteractions
                        ItemPending
                        "interaction-gemini"
                        (Just "thought-1")
                        (Render.geminiThoughtPayload "thought-1")
                pendingCallItem =
                    Render.nativeHistoryItem
                        ProviderGeminiInteractions
                        ItemPending
                        "interaction-gemini"
                        (Just "tool-call-1")
                        (Render.geminiFunctionCallPayload "tool-call-1" "lookup" (object ["name" .= ("John Snow" :: Text)]))
                replayState =
                    conversationReplayState
                        [ pendingThought
                        , pendingCallItem
                        , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory
                `shouldBe`
                    [ pendingThought
                    , pendingCallItem
                    , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                    ]
            resumableToolCalls `shouldBe` []
            pendingArtifacts `shouldBe` []
            blocked `shouldBe` Nothing

        it "surfaces invalid reset checkpoints instead of silently accepting them" $ do
            let start = withHistoryId 1 (user "start")
                missingItemId = historyItemIdAt 99
                history =
                    [ start
                    , resetTo missingItemId
                    ]
                ReplayState{blocked} =
                    conversationReplayState history

            blocked `shouldBe` Just (ReplayInvalidReset (ResetToItem missingItemId))
            latestValidCheckpoint history `shouldBe` Just (ResetToItem (itemIdOf start))
            resetToLatestValidCheckpoint history `shouldBe` Just (resetTo (itemIdOf start))

    describe "reset checkpoints" $ do
        it "reports the stable active prefixes and the latest reset point" $ do
            let start = withHistoryId 1 (user "start")
                done = withHistoryId 2 (assistantText "done")
                pendingAssistant = withHistoryId 3 (Chat.responsesAssistantItem ItemPending "working on it")
                barrierMessage = "Responses response status was failed"
                history =
                    [ start
                    , done
                    , pendingAssistant
                    , HControl (ReplayBarrier barrierMessage)
                    ]

            validResetCheckpoints history
                `shouldBe` [ResetToStart, ResetToItem (itemIdOf start), ResetToItem (itemIdOf done)]
            latestValidCheckpoint history `shouldBe` Just (ResetToItem (itemIdOf done))
            resetToLatestValidCheckpoint history `shouldBe` Just (resetTo (itemIdOf done))

        it "returns Nothing when the current active branch is already a valid checkpoint" $ do
            let start = withHistoryId 1 (user "start")
                done = withHistoryId 2 (assistantText "done")
            resetToLatestValidCheckpoint [start, done]
                `shouldBe` Nothing

        it "does not suggest ResetToStart for stable id-less histories" $ do
            resetToLatestValidCheckpoint [user "start", assistantText "done"]
                `shouldBe` Nothing

    describe "shared restart semantics" $ do
        it "drops unfinished assistant output but keeps resumable tool work when restarting a paused round" $ do
            historyRef <- IORef.newIORef []
            let pendingAssistant = Chat.responsesAssistantItem ItemPending "working on it"
                pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                finalAssistant = Chat.responsesAssistantItem ItemCompleted "done"

            result <-
                runStorageReplayTest historyRef
                    [ Chat.pausedProviderRound
                        (PauseProviderWaiting "Responses response status was in_progress")
                        [pendingAssistant, pendingCallItem]
                    , Chat.finalProviderRound [finalAssistant]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        pure ()

            result `shouldBe` Right ()
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [user "start"]
                    , [ user "start"
                      , pendingCallItem
                      , toolResult "loop-1" "looped"
                      ]
                    ]

        it "requires an explicit reset before retrying a failed round" $ do
            historyRef <- IORef.newIORef []
            let pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                finalAssistant = Chat.responsesAssistantItem ItemCompleted "done"

            result <-
                runStorageReplayTest historyRef
                    [ Chat.failedProviderRound
                        (FailureProvider "Responses response status was failed")
                        [pendingCallItem]
                    , Chat.finalProviderRound [finalAssistant]
                    ]
                    do
                        convId <- createConversation
                        appendItems convId [user "start"]
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        storedHistory <- getConversation convId
                        appendItems convId (maybe [] pure (resetToLatestValidCheckpoint storedHistory))
                        _ <-
                            withStorageBy
                                chatOutcomeAppendedItems
                                ( chatOutcome
                                    defaultChatConfig
                                        { tools = [Chat.loopTool]
                                        }
                                )
                                convId
                        pure ()

            result `shouldBe` Right ()
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [user "start"]
                    , [user "start"]
                    ]

        it "synthesizes a Tool not found result for historical pending tools and continues" $ do
            historyRef <- IORef.newIORef []
            let pendingCall = Chat.loopToolCall "loop-1"
                pendingCallItem = Chat.responsesToolCallItem ItemPending pendingCall
                finalAssistant = Chat.responsesAssistantItem ItemCompleted "done"

            result <-
                runStorageReplayTest historyRef [Chat.finalProviderRound [finalAssistant]] do
                    convId <- createConversation
                    appendItems convId [user "start", pendingCallItem]
                    outcome <-
                        withStorageBy
                            chatOutcomeAppendedItems
                            (chatOutcome defaultChatConfig)
                            convId
                    storedHistory <- getConversation convId
                    pure (outcome, storedHistory)

            fmap
                ( \(outcome, storedHistory) ->
                    ( Chat.stripHistoryItemIdsFromOutcome outcome
                    , Chat.stripHistoryItemIds storedHistory
                    )
                )
                result
                `shouldBe` Right
                    ( Chat.stripHistoryItemIdsFromOutcome
                        (ChatFinished
                            { appendedItems =
                                [ toolResultText "loop-1" "Tool not found: loop_tool"
                                , finalAssistant
                                ]
                            })
                    , Chat.stripHistoryItemIds
                        [ user "start"
                        , pendingCallItem
                        , toolResultText "loop-1" "Tool not found: loop_tool"
                        , finalAssistant
                        ]
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ user "start"
                      , pendingCallItem
                      , toolResultText "loop-1" "Tool not found: loop_tool"
                      ]
                    ]

    describe "provider replay normalization" $ do
        it "turns unresolved pending OpenAI tool calls into Tool not found results before replay" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingOpenAiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingOpenAiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "turns unresolved pending xAI tool calls into Tool not found results before replay" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingXaiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingXaiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "normalizes unresolved pending Responses tool calls before provider switching" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingOpenAiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingOpenAiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "does not replay pending Gemini function_result items as provider input" $ do
            requestBody <-
                Render.captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiFunctionResultItem]

            Render.lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([] :: [Value]))

        it "does not replay Gemini thought artifacts on same-provider restart" $ do
            requestBody <-
                Render.captureGeminiRequestBody
                    defaultChatConfig
                    [pendingGeminiThoughtItem]

            Render.lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON ([] :: [Value]))

        it "does not mark Gemini thoughts replayable when the only matching tool result appears before the pending tool call" $ do
            let replayState =
                    conversationReplayState
                        [ pendingGeminiThoughtItem
                        , toolResultText "tool-call-1" "hello"
                        , pendingGeminiToolCallItem
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory `shouldBe` []
            resumableToolCalls
                `shouldBe`
                    [ ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String "Ada")]
                        }
                    ]
            pendingArtifacts `shouldBe` [pendingGeminiThoughtItem, pendingGeminiToolCallItem]
            blocked `shouldBe` Nothing

        it "normalizes unresolved pending Responses tool calls before switching into Gemini" $ do
            historyRef <- IORef.newIORef []
            result <-
                Chat.runHistoryRecordedMockChatOutcome historyRef [] $
                    chatOutcome defaultChatConfig [pendingOpenAiToolCallItem]

            result
                `shouldBe` Right
                    (ChatFailed
                        { appendedItems =
                            [ toolResultText "tool-call-1" "Tool not found: lookup"
                            , Chat.replayBarrierItem "Unexpected provider round request in test"
                            ]
                        , failureReason = FailureContract "Unexpected provider round request in test"
                        }
                    )
            recordedHistories <- IORef.readIORef historyRef
            recordedHistories
                `shouldBe`
                    [ [ pendingOpenAiToolCallItem
                      , toolResultText "tool-call-1" "Tool not found: lookup"
                      ]
                    ]

        it "keeps resolved tool exchanges replayable after the tool disappears" $ do
            let toolResultItem = toolResultText "tool-call-1" "Contacts:\n- Ada"
                expectedInput =
                    [ object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "name" .= ("lookup" :: Text)
                        , "arguments" .= ("{\"name\":\"Ada\"}" :: Text)
                        ]
                    , object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("tool-call-1" :: Text)
                        , "output" .= ("Contacts:\n- Ada" :: Text)
                        ]
                    ]

            requestBody <-
                Render.captureOpenAIRequestBody
                    defaultChatConfig
                    [pendingOpenAiToolCallItem, toolResultItem]

            Render.lookupPath ["input"] requestBody
                `shouldBe` Just (toJSON (expectedInput :: [Value]))

runStorageReplayTest
    :: IORef.IORef [[HistoryItem]]
    -> [ProviderRound]
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
runStorageReplayTest historyRef plannedRounds action =
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack
        . runRakeStorageInMemory
        $ do
            result <- runErrorNoCallStack @RakeError (Chat.runHistoryRecordedMockRake historyRef plannedRounds action)
            either (error . show) pure result

pendingOpenAiToolCallItem :: HistoryItem
pendingOpenAiToolCallItem =
    Render.nativeHistoryItem
        ProviderOpenAIResponses
        ItemPending
        "response-openai"
        (Just "item-openai-tool")
        ( pendingResponsesToolCallPayload
            "item-openai-tool"
            "tool-call-1"
            "lookup"
        )

pendingXaiToolCallItem :: HistoryItem
pendingXaiToolCallItem =
    Render.nativeHistoryItem
        ProviderXAIResponses
        ItemPending
        "response-xai"
        (Just "item-xai-tool")
        ( pendingResponsesToolCallPayload
            "item-xai-tool"
            "tool-call-1"
            "lookup"
        )

pendingGeminiFunctionResultItem :: HistoryItem
pendingGeminiFunctionResultItem =
    Render.nativeHistoryItem
        ProviderGeminiInteractions
        ItemPending
        "interaction-gemini"
        (Just "tool-call-1")
        (Render.nativeGeminiToolResultPayload "hello")

pendingGeminiThoughtItem :: HistoryItem
pendingGeminiThoughtItem =
    Render.nativeHistoryItem
        ProviderGeminiInteractions
        ItemPending
        "interaction-gemini"
        (Just "thought-1")
        (Render.geminiThoughtPayload "thought-1")

pendingGeminiToolCallItem :: HistoryItem
pendingGeminiToolCallItem =
    Render.nativeHistoryItem
        ProviderGeminiInteractions
        ItemPending
        "interaction-gemini"
        (Just "tool-call-1")
        (Render.geminiFunctionCallPayload "tool-call-1" "lookup" (object ["name" .= ("Ada" :: Text)]))

pendingResponsesToolCallPayload :: Text -> Text -> Text -> Value
pendingResponsesToolCallPayload itemId callId toolName =
    Render.responsesToolCallPayload
        itemId
        callId
        toolName
        (object ["name" .= ("Ada" :: Text)])
        "in_progress"

withHistoryId :: Word32 -> HistoryItem -> HistoryItem
withHistoryId suffix =
    setHistoryItemId (Just (historyItemIdAt suffix))

historyItemIdAt :: Word32 -> HistoryItemId
historyItemIdAt suffix =
    HistoryItemId (UUID.fromWords 0 0 0 suffix)

itemIdOf :: HistoryItem -> HistoryItemId
itemIdOf historyItem =
    fromMaybe
        (error "Expected HistoryItem to have an embedded id in ReplaySpec")
        (historyItemId historyItem)
