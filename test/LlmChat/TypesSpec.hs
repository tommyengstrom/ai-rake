module LlmChat.TypesSpec where

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.OpenApi
import LlmChat
import Relude
import Test.Hspec

data ClosedRecord = ClosedRecord
    { name :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data OpenMapPayload = OpenMapPayload
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ExampleToolCall = ExampleToolCall
    { tool :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data RecordWithMaybe = RecordWithMaybe
    { response :: Text
    , translated_response :: Text
    , toolCall :: Maybe ExampleToolCall
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data NestedChild = NestedChild
    { maybeText :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data NestedRecord = NestedRecord
    { label :: Text
    , child :: Maybe NestedChild
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToSchema OpenMapPayload where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "OpenMapPayload") $
                mempty
                    & type_ ?~ OpenApiObject
                    & additionalProperties ?~ AdditionalPropertiesAllowed True

spec :: Spec
spec = describe "LlmChat.Types" $ do
    describe "jsonSchemaFormat" $ do
        it "adds additionalProperties false when the typed schema leaves it absent" $ do
            case jsonSchemaFormat @ClosedRecord of
                JsonSchema schemaValue ->
                    lookupField "additionalProperties" schemaValue `shouldBe` Just (Bool False)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "preserves explicit additionalProperties from typed schemas" $ do
            case jsonSchemaFormat @OpenMapPayload of
                JsonSchema schemaValue ->
                    lookupField "additionalProperties" schemaValue `shouldBe` Just (Bool True)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "makes optional typed fields required and nullable" $ do
            case jsonSchemaFormat @RecordWithMaybe of
                JsonSchema schemaValue -> do
                    sort (fromMaybe [] (requiredFieldNames schemaValue))
                        `shouldBe` sort ["response", "translated_response", "toolCall"]
                    lookupPath ["properties", "toolCall"] schemaValue
                        `shouldBe` Just (nullableSchema exampleToolCallSchema)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

        it "normalizes nested optional object fields" $ do
            case jsonSchemaFormat @NestedRecord of
                JsonSchema schemaValue -> do
                    sort (fromMaybe [] (requiredFieldNames schemaValue))
                        `shouldBe` sort ["label", "child"]
                    lookupPath ["properties", "child"] schemaValue
                        `shouldBe` Just (nullableSchema nestedChildSchema)
                _ ->
                    expectationFailure "Expected JsonSchema from jsonSchemaFormat"

    describe "defineToolWithArgument" $ do
        it "preserves explicit additionalProperties from typed tool schemas" $ do
            let tool = defineToolWithArgument @OpenMapPayload "open_map" "open map tool" (\_ -> pure (Right "ok"))
                ToolDef{parameterSchema = parameterSchemaValue} = tool
            (parameterSchemaValue >>= lookupField "additionalProperties") `shouldBe` Just (Bool True)

        it "applies the same normalization to typed tool argument schemas" $ do
            let tool = defineToolWithArgument @RecordWithMaybe "lookup" "lookup tool" (\_ -> pure (Right "ok"))
                ToolDef{parameterSchema = parameterSchemaValue} = tool
            sort (maybe [] id (parameterSchemaValue >>= requiredFieldNames))
                `shouldBe` sort ["response", "translated_response", "toolCall"]
            (parameterSchemaValue >>= lookupPath ["properties", "toolCall"])
                `shouldBe` Just (nullableSchema exampleToolCallSchema)

    describe "HistoryItem JSON envelope" $ do
        it "encodes without schemaVersion" $ do
            let encoded = toJSON (user "hello")
            lookupField "schemaVersion" encoded `shouldBe` Nothing
            lookupField "apiFamily" encoded `shouldBe` Just (String "local")

        it "decodes the new envelope format" $ do
            let encoded =
                    object
                        [ "apiFamily" .= ("local" :: Text)
                        , "payload" .= LocalMessage{role = GenericUser, parts = [PartText "hello"]}
                        ]
            fromJSON encoded `shouldBe` Success (user "hello")

        it "decodes the old envelope format for schemaVersion 1" $ do
            let encoded =
                    object
                        [ "apiFamily" .= ("local" :: Text)
                        , "schemaVersion" .= (1 :: Int)
                        , "payload" .= LocalMessage{role = GenericUser, parts = [PartText "hello"]}
                        ]
            fromJSON encoded `shouldBe` Success (user "hello")

        it "rejects unknown historical schema versions" $ do
            let encoded =
                    object
                        [ "apiFamily" .= ("local" :: Text)
                        , "schemaVersion" .= (2 :: Int)
                        , "payload" .= LocalMessage{role = GenericUser, parts = [PartText "hello"]}
                        ]
            case fromJSON encoded :: Result HistoryItem of
                Error{} ->
                    pure ()
                Success decoded ->
                    expectationFailure ("Unexpectedly decoded: " <> show decoded)

        it "round-trips multipart text messages" $ do
            let item = userParts [textPart "hello", textPart " world"]
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips JSON tool responses" $ do
            forM_
                ( [ String "ok"
                  , Number 123
                  , Bool True
                  , Null
                  , toJSON ([1 :: Int, 2 :: Int] :: [Int])
                  , object ["answer" .= (4 :: Int)]
                  ]
                    :: [Value]
                )
                $ \jsonValue -> do
                    let item = toolResultJson "tool-call-1" jsonValue
                    fromJSON (toJSON item) `shouldBe` Success item
  where
    lookupField :: Text -> Value -> Maybe Value
    lookupField fieldName = \case
        Object objectValue ->
            KM.lookup (Key.fromText fieldName) objectValue
        _ ->
            Nothing

    lookupPath :: [Text] -> Value -> Maybe Value
    lookupPath [] currentValue = Just currentValue
    lookupPath (fieldName : rest) currentValue = case currentValue of
        Object currentObject ->
            KM.lookup (Key.fromText fieldName) currentObject >>= lookupPath rest
        _ ->
            Nothing

    requiredFieldNames :: Value -> Maybe [Text]
    requiredFieldNames schemaValue = do
        Array requiredFields <- lookupField "required" schemaValue
        traverse requiredFieldName (toList requiredFields)

    requiredFieldName :: Value -> Maybe Text
    requiredFieldName = \case
        String fieldName ->
            Just fieldName
        _ ->
            Nothing

    nullableSchema :: Value -> Value
    nullableSchema schemaValue =
        object
            [ "anyOf" .= ([schemaValue, nullSchema] :: [Value])
            ]

    nullSchema :: Value
    nullSchema =
        object
            [ "type" .= ("null" :: Text)
            ]

    exampleToolCallSchema :: Value
    exampleToolCallSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "tool" .= object ["type" .= ("string" :: Text)]
                    ]
            , "required" .= (["tool" :: Text] :: [Text])
            , "additionalProperties" .= False
            ]

    nestedChildSchema :: Value
    nestedChildSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "maybeText" .= nullableSchema (object ["type" .= ("string" :: Text)])
                    ]
            , "required" .= (["maybeText" :: Text] :: [Text])
            , "additionalProperties" .= False
            ]
