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
import Rake.Chat
import Rake.ChatSpec qualified as Chat
import Rake.MediaStorage.InMemory (runRakeMediaStorageInMemory)
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

        it "replays Gemini continuation attachments for resolved same-provider tool continuations" $ do
            let pendingCallItem = pendingGeminiToolCallWithThoughtItem "John Snow"
                replayState =
                    conversationReplayState
                        [ pendingCallItem
                        , toolResultText "tool-call-1" "Contacts:\n- John Snow"
                        ]
                ReplayState{replayHistory, resumableToolCalls, pendingArtifacts, blocked} = replayState

            replayHistory
                `shouldBe`
                    [ pendingCallItem
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

    describe "system snapshots" $ do
        it "uses the latest replayed system snapshot and drops all system messages from chronology" $ do
            let history =
                    [ system "old system"
                    , user "hello"
                    , system "new system"
                    , assistantText "done"
                    ]
                (maybeSystemSnapshot, chronology) = renderableReplayHistory history

            effectiveSystemSnapshot history `shouldBe` Just (system "new system")
            maybeSystemSnapshot `shouldBe` Just (system "new system")
            chronology `shouldBe` [user "hello", assistantText "done"]

        it "takes the latest system snapshot from the active replay branch after resets" $ do
            let staleSystem = withHistoryId 1 (system "stale system")
                freshSystem = withHistoryId 2 (system "fresh system")
                history =
                    [ staleSystem
                    , user "before reset"
                    , resetToStart
                    , freshSystem
                    , user "after reset"
                    ]

            effectiveSystemSnapshot history `shouldBe` Just freshSystem
            snd (renderableReplayHistory history) `shouldBe` [user "after reset"]

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
                    , Chat.replayBarrierItem barrierMessage
                    ]

            validResetCheckpoints history
                `shouldBe` [ResetToStart, ResetToItem (itemIdOf start), ResetToItem (itemIdOf done)]
            latestValidCheckpoint history `shouldBe` Just (ResetToItem (itemIdOf done))
            resetToLatestValidCheckpoint history `shouldBe` Just (resetTo (itemIdOf done))

        it "treats pending non-portable artifacts as unfinished when computing reset checkpoints" $ do
            let start = withHistoryId 1 (user "start")
                done = withHistoryId 2 (assistantText "done")
                pendingThought = withHistoryId 3 pendingGeminiUnusedThoughtItem
                history = [start, done, pendingThought]
                ReplayState{replayHistory, pendingArtifacts, blocked} = conversationReplayState history

            replayHistory `shouldBe` [start, done]
            pendingArtifacts `shouldBe` [pendingThought]
            blocked `shouldBe` Nothing
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
                      , Chat.withAvailableLocalTools [Chat.loopTool] pendingCallItem
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


runStorageReplayTest
    :: IORef.IORef [[HistoryItem]]
    -> [ProviderRound]
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
runStorageReplayTest historyRef plannedRounds action =
    runEff
        . runConcurrent
        . runTime
        . runRakeMediaStorageInMemory
        . runErrorNoCallStack
        . runRakeStorageInMemory
        $ do
            result <- runErrorNoCallStack @RakeError (Chat.runHistoryRecordedMockRake historyRef plannedRounds action)
            either (error . show) pure result

pendingGeminiToolCallWithThoughtItem :: Text -> HistoryItem
pendingGeminiToolCallWithThoughtItem contactName =
    HistoryItem
        { historyItemIdField = Nothing
        , itemLifecycle = ItemPending
        , genericItem =
            GenericToolCall
                { toolCall =
                    ToolCall
                        { toolCallId = "tool-call-1"
                        , toolName = "lookup"
                        , toolArgs = fromList [("name", String contactName)]
                        , continuationAttachments =
                            [ ToolCallContinuation
                                { continuationProviderFamily = ProviderGeminiInteractions
                                , continuationPayload = object ["type" .= ("thought" :: Text), "signature" .= ("thought-1" :: Text)]
                                }
                            ]
                        }
                }
        , providerItem =
            Just
                ProviderItem
                    { apiFamily = ProviderGeminiInteractions
                    , exchangeId = Just "interaction-gemini"
                    , nativeItemId = Just "tool-call-1"
                    , payload = object ["type" .= ("function_call" :: Text), "id" .= ("tool-call-1" :: Text), "name" .= ("lookup" :: Text), "arguments" .= object ["name" .= contactName]]
                    , availableLocalTools = []
                    }
        }

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


pendingGeminiUnusedThoughtItem :: HistoryItem
pendingGeminiUnusedThoughtItem = nonPortableHistoryItem ItemPending ProviderItem
    { apiFamily = ProviderGeminiInteractions
    , exchangeId = Just "interaction-gemini"
    , nativeItemId = Just "thought-1"
    , payload = object ["type" .= ("thought" :: Text), "signature" .= ("thought-1" :: Text)]
    , availableLocalTools = []
    }
