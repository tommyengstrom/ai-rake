{-# OPTIONS_GHC -Wno-orphans #-}

module LlmChat.Storage.InMemorySpec where

import Control.Lens (folded, reversed, taking, (^..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static
import Effectful.Time
import LlmChat.Storage.Effect
import LlmChat.Storage.InMemory
import LlmChat.Types
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
    describe "runLlmChatStorageInMemory" $
        specGeneralized runLlmChatStorageInMemory

specGeneralized
    :: ( forall a es
          . ( Error ChatStorageError :> es
            , Time :> es
            , Concurrent :> es
            , IOE :> es
            )
         => Eff (LlmChatStorage ': es) a
         -> Eff es a
       )
    -> Spec
specGeneralized runStorage = do
    describe "LlmChatStorage" $ do
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
                    afterAppend
                        ^.. reversed . taking 2 folded . _Ctor @"HLocal" . _Ctor @"LocalUser" . typed @Text
                        `shouldBe` [userPrompt2, userPrompt1]

        it "AppendItem errors if the conversation does not exist" $ do
            property $ \(convId :: ConversationId) (SomeText prompt) -> monadicIO $ do
                result <-
                    run
                        . runEffStack
                        . runStorage
                        $ appendItem convId (user prompt)
                liftIO $ result `shouldBe` Left (NoSuchConversation convId)

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
