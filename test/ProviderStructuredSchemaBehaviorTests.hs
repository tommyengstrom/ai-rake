module ProviderStructuredSchemaBehaviorTests
    ( SchemaBehavior (..)
    , ProviderStructuredSchemaExpectations (..)
    , allStructuredSchemasAccepted
    , specWithStructuredSchemaProvider
    ) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Rake
import Relude
import StructuredSchemaTestTypes
import Test.Hspec

data SchemaBehavior
    = SchemaAccepted
    | SchemaRejected Text
    deriving stock (Show, Eq)

data ProviderStructuredSchemaExpectations = ProviderStructuredSchemaExpectations
    { nonNullarySumResponse :: SchemaBehavior
    , maybeRecordResponse :: SchemaBehavior
    , nullaryEnumResponse :: SchemaBehavior
    , maybeToolArguments :: SchemaBehavior
    }
    deriving stock (Show, Eq)

allStructuredSchemasAccepted :: ProviderStructuredSchemaExpectations
allStructuredSchemasAccepted =
    ProviderStructuredSchemaExpectations
        { nonNullarySumResponse = SchemaAccepted
        , maybeRecordResponse = SchemaAccepted
        , nullaryEnumResponse = SchemaAccepted
        , maybeToolArguments = SchemaAccepted
        }

specWithStructuredSchemaProvider
    :: forall es
     . ( IOE :> es
       , RakeStorage :> es
       , RakeMediaStorage :> es
       , Error RakeError :> es
       , Rake :> es
       )
    => ProviderStructuredSchemaExpectations
    -> (forall a. Eff es a -> IO (Either RakeError a))
    -> Spec
specWithStructuredSchemaProvider
    ProviderStructuredSchemaExpectations
        { nonNullarySumResponse = expectedNonNullarySumResponse
        , maybeRecordResponse = expectedMaybeRecordResponse
        , nullaryEnumResponse = expectedNullaryEnumResponse
        , maybeToolArguments = expectedMaybeToolArguments
        }
    runEffectStack =
    describe "Structured schema behavior" $ do
        it "tests the real provider reaction to non-nullary sum structured output" $ do
            result <-
                runEffectStack $
                    runConversation
                        defaultChatConfig{responseFormat = jsonSchemaFormat @NonNullarySum}
                        [ user "Return a JSON object for the SumText variant with tag=\"SumText\" and value=\"hello\". Return nothing else."
                        ]
            assertStructuredOutput expectedNonNullarySumResponse result (SumText "hello")

        it "tests the real provider reaction to Maybe-bearing structured output" $ do
            result <-
                runEffectStack $
                    runConversation
                        defaultChatConfig{responseFormat = jsonSchemaFormat @RecordWithMaybe}
                        [ user "Return a JSON object with response=\"ok\", translated_response=\"ok\", and toolCall=null. Return nothing else."
                        ]
            assertStructuredOutput expectedMaybeRecordResponse result (RecordWithMaybe "ok" "ok" Nothing)

        it "tests the real provider reaction to nullary enum structured output" $ do
            result <-
                runEffectStack $
                    runConversation
                        defaultChatConfig{responseFormat = jsonSchemaFormat @NullaryEnum}
                        [ user "Return exactly the JSON string \"Alpha\" and nothing else."
                        ]
            assertStructuredOutput expectedNullaryEnumResponse result Alpha

        it "tests the real provider reaction to Maybe-bearing typed tool schemas" $ do
            result <-
                runEffectStack $
                    runConversation
                        defaultChatConfig{tools = [maybeTool]}
                        [ system "Reply directly unless a tool is explicitly required."
                        , user "Reply with the exact text ok."
                        ]
            assertToolSchemaOutcome expectedMaybeToolArguments result

runConversation
    :: ( RakeStorage :> es
       , RakeMediaStorage :> es
       , IOE :> es
       , Error RakeError :> es
       , Rake :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es ChatOutcome
runConversation chatConfig history = do
    convId <- createConversation
    appendItems convId history
    withResumableChat chatConfig convId

assertStructuredOutput
    :: forall a
     . (Eq a, FromJSON a, Show a)
    => SchemaBehavior
    -> Either RakeError ChatOutcome
    -> a
    -> Expectation
assertStructuredOutput schemaBehavior result expectedValue =
    case schemaBehavior of
        SchemaAccepted -> do
            outcome <- expectAcceptedProviderResult result
            let response = expectFinished outcome
            case decodeLastAssistant @a response of
                Left err ->
                    expectationFailure ("Expected structured output to decode: " <> show err)
                Right decodedValue ->
                    decodedValue `shouldBe` expectedValue
        SchemaRejected expectedFragment ->
            assertRejectedProviderResult expectedFragment result

assertToolSchemaOutcome
    :: SchemaBehavior
    -> Either RakeError ChatOutcome
    -> Expectation
assertToolSchemaOutcome schemaBehavior result =
    case schemaBehavior of
        SchemaAccepted -> do
            outcome <- expectAcceptedProviderResult result
            case outcome of
                ChatFinished{appendedItems} ->
                    appendedItems `shouldSatisfy` (not . null)
                ChatPaused{appendedItems} ->
                    appendedItems `shouldSatisfy` (not . null)
                ChatFailed{failureReason} ->
                    expectationFailure ("Expected request acceptance, got ChatFailed: " <> show failureReason)
        SchemaRejected expectedFragment ->
            assertRejectedProviderResult expectedFragment result

expectAcceptedProviderResult :: Either RakeError ChatOutcome -> IO ChatOutcome
expectAcceptedProviderResult = \case
    Left rakeError ->
        expectationFailure ("Expected provider to accept the schema, got: " <> toString (renderRakeError rakeError))
            >> fail "unreachable"
    Right outcome ->
        pure outcome

assertRejectedProviderResult :: Text -> Either RakeError ChatOutcome -> Expectation
assertRejectedProviderResult expectedFragment = \case
    Left rakeError ->
        renderRakeError rakeError `shouldSatisfy` T.isInfixOf expectedFragment
    Right outcome ->
        expectationFailure ("Expected provider rejection, got successful outcome: " <> show outcome)

expectFinished :: ChatOutcome -> [HistoryItem]
expectFinished = \case
    ChatFinished{appendedItems} ->
        appendedItems
    other ->
        error ("Expected ChatFinished, got: " <> show other)
