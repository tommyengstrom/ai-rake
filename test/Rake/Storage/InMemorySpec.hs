{-# OPTIONS_GHC -Wno-orphans #-}

module Rake.Storage.InMemorySpec where

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID
import Effectful
import Effectful.Concurrent (Concurrent, forkFinally, runConcurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Error.Static
import Effectful.Time
import Rake.Storage.Effect
import Rake.Storage.InMemory
import Rake.Types
import Relude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary ConversationId where
    arbitrary =
        fmap ConversationId $
            fromWords
                <$> arbitraryBoundedIntegral
                <*> arbitraryBoundedIntegral
                <*> arbitraryBoundedIntegral
                <*> arbitraryBoundedIntegral

runEffStack
    :: Eff
        '[ Error ChatStorageError
         , Time
         , Concurrent
         , IOE
         ]
        a
    -> IO (Either ChatStorageError a)
runEffStack =
    runEff
        . runConcurrent
        . runTime
        . runErrorNoCallStack

data SomeText = SomeText Text
    deriving stock (Show, Generic)

instance Arbitrary SomeText where
    arbitrary = SomeText . T.pack <$> listOf (choose ('a', 'z'))

spec :: Spec
spec =
    describe "runRakeStorageInMemory" $
        specGeneralized runRakeStorageInMemory

specGeneralized
    :: ( forall a es
          . ( Error ChatStorageError :> es
            , Time :> es
            , Concurrent :> es
            , IOE :> es
            )
         => Eff (RakeStorage ': es) a
         -> Eff es a
       )
    -> Spec
specGeneralized runStorage = do
    describe "RakeStorage" $ do
        it "Fails to fetch conversation that does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ getConversation convId
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        it "Creates an empty conversation" $ do
            monadicIO $ do
                Right conv <- run . runEffStack $ runStorage do
                    convId <- createConversation
                    getConversation convId
                liftIO $ conv `shouldBe` []

        it "Lists more conversation after creating one" $ do
            property $ monadicIO $ do
                Right (listBefore, convId, listAfter) <- run
                    . runEffStack
                    $ runStorage do
                        listBefore <- listConversations
                        convId <- createConversation
                        (listBefore, convId,) <$> listConversations
                liftIO $
                    Set.difference (Set.fromList listAfter) (Set.fromList listBefore)
                        `shouldBe` [convId]

        it "AppendItem adds items to the end of the conversation" $
            property \(SomeText systemPrompt) (SomeText userPrompt1) (SomeText userPrompt2) -> monadicIO do
                Right (beforeAppend, afterAppend) <- run $ runEffStack $ runStorage $ do
                    convId <- createConversation
                    conv <- getConversation convId
                    appendItems convId [system systemPrompt, user userPrompt1, user userPrompt2]
                    (conv,) <$> getConversation convId
                liftIO $ length beforeAppend + 3 `shouldBe` length afterAppend
                liftIO $
                    take 2 (reverseUserTexts afterAppend)
                        `shouldBe` [userPrompt2, userPrompt1]

        it "AppendItem errors if the conversation does not exist" $ do
            property $ \(convId :: ConversationId) (SomeText prompt) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ appendItem convId (user prompt)
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        it "ModifyConversationAtomic appends based on the current history" $ do
            property $ \(SomeText firstPrompt) (SomeText secondPrompt) -> monadicIO $ do
                result <- run . runEffStack $ runStorage do
                    convId <- createConversation
                    appendItem convId (user firstPrompt)
                    observedLength <-
                        modifyConversationAtomic convId \history ->
                            (length history, [user secondPrompt])
                    finalHistory <- map (setHistoryItemId Nothing) <$> getConversation convId
                    pure (observedLength, finalHistory)

                liftIO $
                    result `shouldBe`
                        Right
                            ( 1
                            , [user firstPrompt, user secondPrompt]
                            )

        it "ModifyConversationAtomic errors if the conversation does not exist" $ do
            property $ \(convId :: ConversationId) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ modifyConversationAtomic convId (\history -> (length history, [user "ignored"]))
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

        it "ModifyConversationAtomic serializes concurrent writers" $ do
            let workerCount = 20

            Right (threadResults, finalHistory) <- runEffStack $ runStorage do
                convId <- createConversation
                startGate <- MVar.newEmptyMVar
                done <- MVar.newEmptyMVar

                forM_ ([1 .. workerCount] :: [Int]) $ \workerIndex ->
                    void $
                        forkFinally
                            ( do
                                MVar.readMVar startGate
                                catchError @ChatStorageError
                                    (Right <$> modifyConversationAtomic convId (\history -> (length history, [user ("worker-" <> show workerIndex)])))
                                    (\_ err -> pure (Left err))
                            )
                            (MVar.putMVar done)

                MVar.putMVar startGate ()
                threadResults <- replicateM workerCount (MVar.takeMVar done)
                finalHistory <- getConversation convId
                pure (threadResults, finalHistory)

            observedLengths <- forM threadResults $ \case
                Left err ->
                    expectationFailure ("Concurrent in-memory storage worker crashed: " <> show err)
                        >> pure (-1)
                Right (Left err) ->
                    expectationFailure ("Concurrent in-memory storage worker failed: " <> show err)
                        >> pure (-1)
                Right (Right observedLength) ->
                    pure observedLength

            sort observedLengths `shouldBe` [0 .. workerCount - 1]
            length finalHistory `shouldBe` workerCount

        it "DeleteConversation removes the conversation" $ do
            property $ monadicIO $ do
                Right (convId, listAfterDelete, fetchResult) <- run
                    . runEffStack
                    $ runStorage do
                        convId <- createConversation
                        appendItem convId (user "hello")
                        deleteConversation convId
                        listAfterDelete <- listConversations
                        fetchResult <-
                            ( getConversation convId >>= \conversation -> pure (Right conversation)
                            )
                                `catchError` (\_ err -> pure (Left err :: Either ChatStorageError [HistoryItem]))
                        pure (convId, listAfterDelete, fetchResult)

                liftIO $ convId `shouldNotSatisfy` (`elem` listAfterDelete)
                liftIO $ fetchResult `shouldBe` Left (NoSuchConversation convId)

        it "allows the same HistoryItemId in different conversations" $ do
            let sharedItemId = fixedHistoryItemId 1
                sharedUserMessage = setHistoryItemId (Just sharedItemId) (user "hello")
            result <- runEffStack $ runStorage do
                firstConversationId <- createConversation
                secondConversationId <- createConversation
                appendItem firstConversationId sharedUserMessage
                appendItem secondConversationId sharedUserMessage
                (,) <$> getConversation firstConversationId <*> getConversation secondConversationId

            result `shouldBe` Right ([sharedUserMessage], [sharedUserMessage])

        it "rejects duplicate HistoryItemIds within one conversation" $ do
            let duplicateItemId = fixedHistoryItemId 1
                firstMessage = setHistoryItemId (Just duplicateItemId) (user "hello")
                secondMessage = setHistoryItemId (Just duplicateItemId) (assistantText "world")
            result <- runEffStack $ runStorage do
                conversationId <- createConversation
                appendItems conversationId [firstMessage, secondMessage]

            result
                `shouldSatisfy` \case
                    Left (DuplicateHistoryItemId _ itemId) ->
                        itemId == duplicateItemId
                    _ ->
                        False
  where
    reverseUserTexts history =
        [ text
        | HLocal LocalMessage{role = GenericUser, parts = [PartText{text}]} <- reverse history
        ]

fixedHistoryItemId :: Word32 -> HistoryItemId
fixedHistoryItemId suffix =
    HistoryItemId (fromWords 0 0 0 suffix)
