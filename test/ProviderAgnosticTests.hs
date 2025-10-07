module ProviderAgnosticTests where

import LlmChat
import Control.Lens (folded, (^..))
import Data.Aeson
import Data.Aeson.KeyMap (keys)
import Data.Generics.Sum
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Relude
import Test.Hspec


-- | Common spec that tests basic LlmChat functionality
-- The runner function should handle setting up the specific provider
specWithProvider
    :: forall es
     . ( LlmChatStorage :> es
        , Error LlmChatError :> es
        , LlmChat :> es
        )
     => (forall a. Eff es a -> IO a)
    -> Spec
specWithProvider runEffectStack  = do
    --tvar <- runIO $ newTVarIO (mempty :: Map ConversationId [ChatMsg])

    it "Responds to initial UserMsg" $ do
        (response, conv) <- runEffectStack $ do
            convId <-
                createConversation
                    "Act exactly as a simple calculator. No extra text, just the answer."
            appendUserMessage convId "2 + 2"
            resp <- getLlmResponse [] Unstructured =<< getConversation convId
            appendMessage convId resp
            conv <- getConversation convId
            pure (resp, conv)
        response `shouldSatisfy` \case
            AssistantMsg {content} -> T.elem '4' content -- More lenient check for different providers
            _ -> False
        conv `shouldSatisfy` (== 3) . length -- silly test...

    it "Tool call is correctly triggered" $ do
        response <- runEffectStack  $ do
            convId <-
                createConversation
                    "You are the users assistant. When asked about contacts or phone numbers, use the available tools to find the information."
            appendUserMessage convId "What is my friend John's last name?"
            msgs <- withStorage (respondWithTools [listContacts]) convId
            pure msgs
        response
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )

    it "Resolves multiple tool calls" $ do
        response <- runEffectStack  $ do
            convId <-
                createConversation
                    "You are the users assistant, always trying to help them without first clearifying what they want. When asked about contacts or phone numbers, use the available tools to find the information."
            appendUserMessage convId "What is John's phone number?"
            msgs <-
                withStorage
                    (respondWithTools [listContacts, showPhoneNumber])
                    convId
            pure msgs
        response
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )
        response
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "show_phone_number") toolCalls
                    _ -> False
                )
        response
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{content} -> T.isInfixOf "123-456-7890" content
                    _ -> False
                )
        response `shouldSatisfy` (>= 3) . length -- At least 3 messages
        let assistantToolCalls =
                [calls | AssistantMsg{toolCalls = calls} <- response, not (null calls)]
        assistantToolCalls `shouldSatisfy` (>= 1) . length
        (response ^.. folded . _Ctor @"ToolCallResponseMsg") `shouldSatisfy` (>= 1) . length
        (response ^.. folded . _Ctor @"AssistantMsg") `shouldSatisfy` (>= 1) . length

    describe "Structured Output" $ do
        it "Responds with JSON when requested" $ do
            (_, val) <- runEffectStack $ do
                convId <- createConversation "You are a helpful assistant. Always provide direct answers."
                appendUserMessage convId "What is 2+2? Reply with a JSON object containing the field 'answer' with the numeric result."
                withStorageStructured (respondWithToolsJson []) convId
            val `shouldSatisfy` \v ->
                case v of
                    Object obj -> "answer" `elem` keys obj
                    _ -> False

        it "Responds with structured output matching schema" $ do
            (_, PersonInfo name age) <- runEffectStack  $ do
                convId <-
                    createConversation "You are a helpful assistant. Provide structured data when requested."
                appendUserMessage convId "Tell me about Albert Einstein. Include his name and approximate age at death."
                withStorageStructured (respondWithToolsStructured @PersonInfo []) convId
            name `shouldSatisfy` T.isInfixOf "Einstein"
            age `shouldSatisfy` (> 70)

        it "Combines tools with structured output" $ do
            (msgs, ContactInfo name _) <- runEffectStack $ do
                convId <-
                    createConversation
                        "You are a helpful assistant. Use tools when needed and provide structured responses."
                appendUserMessage convId "Get John's information and return it as structured data."
                withStorageStructured
                    (respondWithToolsStructured @ContactInfo [listContacts, showPhoneNumber])
                    convId
            name `shouldSatisfy` T.isInfixOf "John"
            msgs
                `shouldSatisfy` any
                    ( \case
                        AssistantMsg{toolCalls} -> not (null toolCalls)
                        _ -> False
                    )

-- Test data types for structured output
data PersonInfo = PersonInfo
    { name :: Text
    , age :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data ContactInfo = ContactInfo
    { name :: Text
    , phoneNumber :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- Helper tools for testing
listContacts :: ToolDef es
listContacts =
    defineToolNoArgument
        "list_contact"
        "List all the contacts of the user."
        ( pure . Right . ToolResponse $ "Contacts:\n" <> T.intercalate "\n- " contacts
        )
  where
    contacts :: [Text]
    contacts = ["John Snow", "Arya Stark", "Tyrion Lannister"]

data FullName = FullName
    { fullName :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

showPhoneNumber :: ToolDef es
showPhoneNumber =
    defineToolWithArgument
        "show_phone_number"
        "Show the phone number of a contact. Must use full name for lookup, as given by `list_contact`."
        ( \case
            FullName "John Snow" ->
                pure . Right . ToolResponse $ "Phone number: 123-456-7890"
            FullName n -> pure $ Left $ "No phone number for contact: " <> T.unpack n
        )
