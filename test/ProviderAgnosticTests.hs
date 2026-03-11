module ProviderAgnosticTests where

import Data.Aeson
import Data.Aeson.KeyMap (keys)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import LlmChat
import Relude
import Test.Hspec

specWithProvider
    :: forall es
     . ( LlmChatStorage :> es
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => (forall a. Eff es a -> IO a)
    -> Spec
specWithProvider runEffectStack = do
    it "Responds to an initial system+user history" $ do
        (response, conv) <- runEffectStack $ do
            convId <- createConversation
            appendItems
                convId
                [ system "Act exactly as a simple calculator. No extra text, just the answer."
                , user "2 + 2"
                ]
            resp <- withStorage (chat defaultChatConfig) convId
            conv <- getConversation convId
            pure (resp, conv)

        lastAssistantText response `shouldSatisfy` maybe False (T.elem '4')
        length conv `shouldSatisfy` (>= 3)

    it "Executes a single tool call" $ do
        response <- runEffectStack $ do
            convId <- createConversation
            appendItems
                convId
                [ system "You are the user's assistant. When asked about contacts or phone numbers, use the available tools."
                , user "What is my friend John's last name?"
                ]
            withStorage
                (chat defaultChatConfig{tools = [listContacts]})
                convId

        response
            `shouldSatisfy` any
                ( \case
                    HLocal LocalToolResult{toolResult = ToolResult{toolResponse = ToolResponse{response = toolOutput}}} ->
                        T.isInfixOf "John Snow" toolOutput
                    _ ->
                        False
                )

    it "Executes multiple tool calls and returns the final answer" $ do
        response <- runEffectStack $ do
            convId <- createConversation
            appendItems
                convId
                [ system "You are the user's assistant. Use tools to answer contact and phone number questions."
                , user "What is John's phone number?"
                ]
            withStorage
                (chat defaultChatConfig{tools = [listContacts, showPhoneNumber]})
                convId

        response
            `shouldSatisfy` any
                ( \case
                    HLocal LocalToolResult{toolResult = ToolResult{toolResponse = ToolResponse{response = toolOutput}}} ->
                        T.isInfixOf "123-456-7890" toolOutput
                    _ ->
                        False
                )
        lastAssistantText response `shouldSatisfy` maybe False (T.isInfixOf "123-456-7890")
        response `shouldSatisfy` (>= 3) . length
        length [() | HLocal LocalToolResult{} <- response] `shouldSatisfy` (>= 1)

    describe "Structured Output" $ do
        it "Responds with JSON when requested" $ do
            msgs <- runEffectStack $ do
                convId <- createConversation
                appendItems
                    convId
                    [ system "You are a helpful assistant. Always provide direct answers."
                    , user "What is 2+2? Reply with a JSON object containing the field 'answer' with the numeric result."
                    ]
                withStorage
                    (chat defaultChatConfig{responseFormat = JsonValue})
                    convId

            val <- case decodeLastAssistant @Value msgs of
                Left err -> expectationFailure (show err) >> fail "unreachable"
                Right parsed -> pure parsed

            val `shouldSatisfy` \case
                Object obj -> "answer" `elem` keys obj
                _ -> False

        it "Responds with structured output matching schema" $ do
            PersonInfo name age <- runEffectStack $ do
                convId <- createConversation
                appendItems
                    convId
                    [ system "You are a helpful assistant. Provide structured data when requested."
                    , user "Tell me about Albert Einstein. Include his name and approximate age at death."
                    ]
                msgs <-
                    withStorage
                        (chat defaultChatConfig{responseFormat = jsonSchemaFormat @PersonInfo})
                        convId
                either throwError pure (decodeLastAssistant msgs)

            name `shouldSatisfy` T.isInfixOf "Einstein"
            age `shouldSatisfy` (> 70)

        it "Combines tools with structured output" $ do
            (msgs, ContactInfo name _) <- runEffectStack $ do
                convId <- createConversation
                appendItems
                    convId
                    [ system "You are a helpful assistant. Use tools when needed and provide structured responses."
                    , user "Get John's information and return it as structured data."
                    ]
                msgs <-
                    withStorage
                        ( chat
                            defaultChatConfig
                                { responseFormat = jsonSchemaFormat @ContactInfo
                                , tools = [listContacts, showPhoneNumber]
                                }
                        )
                        convId
                decoded <- either throwError pure (decodeLastAssistant msgs)
                pure (msgs, decoded)

            name `shouldSatisfy` T.isInfixOf "John"
            msgs
                `shouldSatisfy` any
                    ( \case
                        HLocal LocalToolResult{} -> True
                        _ -> False
                    )

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

listContacts :: ToolDef es
listContacts =
    defineToolNoArgument
        "list_contact"
        "List all the contacts of the user."
        (pure . Right . ToolResponse $ "Contacts:\n" <> T.intercalate "\n- " contacts)
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
            FullName n ->
                pure $ Left $ "No phone number for contact: " <> T.unpack n
        )
