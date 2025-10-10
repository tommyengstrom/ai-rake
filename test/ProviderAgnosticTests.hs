module ProviderAgnosticTests where

import Control.Lens (folded, (^..))
import Data.Aeson
import Data.Aeson.KeyMap (keys)
import Data.Generics.Sum
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import LlmChat
import Relude
import Test.Hspec

sysPrompt :: ChatMsg
sysPrompt =
    SystemMsg
        "You are the users assistant, always trying to help them without first clearifying what they want. When asked about contacts or phone numbers, use the available tools to find the information. When you do a tool call, always include a short message describing what you are doing."

data PhoneNumber = PhoneNumber
    { phoneNumer :: Text
    } deriving stock (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Common spec that tests basic LlmChat functionality
-- The runner function should handle setting up the specific provider
specWithProvider
    :: forall es
     . ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => (forall a. Eff es a -> IO a)
    -> Spec
specWithProvider runEffectStack = do
    -- tvar <- runIO $ newTVarIO (mempty :: Map ConversationId [ChatMsg])

    it "Responds to initial UserMsg" $ do
        response <-
            runEffectStack $
                getLlmResponse
                    []
                    Unstructured
                    [sysPrompt, UserMsg "2 + 2"]
        response `shouldSatisfy` \case
            AssistantMsg{content} -> T.elem '4' content -- More lenient check for different providers
            _ -> False
    it "Tool call is correctly triggered" $ do
        response <-
            runEffectStack $
                respondWithTools
                    [listContacts]
                    [ sysPrompt
                    , UserMsg "What is my friend John's last name?"
                    ]
        response
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )

    it "Resolves multiple tool calls" $ do
        msgs <- runEffectStack $ respondWithTools
                    [listContacts, showPhoneNumber]
                    [ sysPrompt
                    , UserMsg "What is John's phone number?"
                    ]
        msgs
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )
        msgs
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "show_phone_number") toolCalls
                    _ -> False
                )
        msgs
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{content} -> T.isInfixOf "123-456-7890" content
                    _ -> False
                )
        msgs `shouldSatisfy` (>= 3) . length -- At least 3 messages
        let assistantToolCalls =
                [calls | AssistantMsg{toolCalls = calls} <- msgs, not (null calls)]
        assistantToolCalls `shouldSatisfy` (>= 1) . length
        (msgs ^.. folded . _Ctor @"ToolResponseMsg") `shouldSatisfy` (>= 1) . length
        (msgs ^.. folded . _Ctor @"AssistantMsg") `shouldSatisfy` (>= 1) . length

    it "Resolves multiple tool calls with structured response" $ do
        -- this this case the assistant will send empty messsages as our response format
        -- it not giving it a reasonable way to tell us what it is doing.
        (msgs, answer) <- runEffectStack $ do
                respondWithToolsStructured
                    [listContacts, showPhoneNumber]
                    [ sysPrompt
                    , UserMsg "What is John's phone number?"
                    ]
        msgs
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "list_contact") toolCalls
                    _ -> False
                )
        msgs
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{toolCalls} -> any (\ToolCall{toolName} -> toolName == "show_phone_number") toolCalls
                    _ -> False
                )
        msgs
            `shouldSatisfy` any
                ( \case
                    AssistantMsg{content} -> T.isInfixOf "123-456-7890" content
                    _ -> False
                )
        answer `shouldBe` PhoneNumber "123-456-7890"
        msgs `shouldSatisfy` (>= 3) . length -- At least 3 messages
        let assistantToolCalls =
                [calls | AssistantMsg{toolCalls = calls} <- msgs, not (null calls)]
        assistantToolCalls `shouldSatisfy` (>= 1) . length
        (msgs ^.. folded . _Ctor @"ToolResponseMsg") `shouldSatisfy` (>= 1) . length
        (msgs ^.. folded . _Ctor @"AssistantMsg") `shouldSatisfy` (>= 1) . length

    describe "Structured Output" $ do
        it "Responds with JSON when requested" $ do
            (_, val) <-
                runEffectStack $
                    respondWithToolsJson
                        []
                        [ sysPrompt
                        , UserMsg
                            "What is 2+2? Reply with a JSON object containing the field 'answer' with the numeric result."
                        ]
            val `shouldSatisfy` \v ->
                case v of
                    Object obj -> "answer" `elem` keys obj
                    _ -> False

        it "Responds with structured output matching schema" $ do
            (_, PersonInfo name age) <-
                runEffectStack $
                    respondWithToolsStructured @PersonInfo
                        []
                        [ sysPrompt
                        , UserMsg "Tell me about Albert Einstein. Include his name and approximate age at death."
                        ]

            name `shouldSatisfy` T.isInfixOf "Einstein"
            age `shouldSatisfy` (> 70)

        it "Combines tools with structured output" $ do
            (msgs, ContactInfo name _) <-
                runEffectStack $
                    respondWithToolsStructured @ContactInfo
                        [listContacts, showPhoneNumber]
                        [sysPrompt, UserMsg "Get John's information and return it as structured data."]
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
            FullName n -> pure $ Left $ "No phone number for contact: " <> T.unpack n
        )
