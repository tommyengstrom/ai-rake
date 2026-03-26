module Rake.TypesSpec where

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.OpenApi
import Data.UUID qualified as UUID
import Rake
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
spec = describe "Rake.Types" $ do
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
        it "encodes schemaVersion 4 and completed lifecycle for local items" $ do
            let encoded = toJSON (user "hello")
            lookupField "schemaVersion" encoded `shouldBe` Just (toJSON (4 :: Int))
            lookupField "apiFamily" encoded `shouldBe` Just (String "local")
            lookupField "itemLifecycle" encoded `shouldBe` Just (toJSON ItemCompleted)

        it "round-trips embedded history item ids" $ do
            let itemId = historyItemIdAt 7
                item = setHistoryItemId (Just itemId) (user "hello")
                encoded = toJSON item
            lookupField "historyItemId" encoded `shouldBe` Just (toJSON itemId)
            fromJSON encoded `shouldBe` Success item

        it "rejects envelopes without a schemaVersion" $ do
            let encoded =
                    object
                        [ "apiFamily" .= ("local" :: Text)
                        , "payload" .= LocalMessage{role = GenericUser, parts = [PartText "hello"]}
                        ]
            case fromJSON encoded :: Result HistoryItem of
                Error{} ->
                    pure ()
                Success decoded ->
                    expectationFailure ("Unexpectedly decoded: " <> show decoded)

        it "rejects legacy schemaVersion 2 envelopes" $ do
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

        it "rejects legacy control item tags" $ do
            let encoded =
                    object
                        [ "schemaVersion" .= (4 :: Int)
                        , "apiFamily" .= ("control" :: Text)
                        , "itemLifecycle" .= ItemCompleted
                        , "payload"
                            .= object
                                [ "tag" .= ("ReplayBarrier" :: Text)
                                , "contents" .= ("bad round" :: Text)
                                ]
                        ]
            case fromJSON encoded :: Result HistoryItem of
                Error{} ->
                    pure ()
                Success decoded ->
                    expectationFailure ("Unexpectedly decoded: " <> show decoded)

        it "round-trips pending Gemini native envelopes" $ do
            let item =
                    HProvider
                        ProviderHistoryItem
                            { apiFamily = ProviderGeminiInteractions
                            , itemLifecycle = ItemPending
                            , nativeItem =
                                NativeProviderItem
                                    { exchangeId = Nothing
                                    , nativeItemId = Just "native-thought"
                                    , payload = object ["type" .= ("thought" :: Text)]
                                    }
                            }
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips reset control envelopes" $ do
            let item = resetTo (historyItemIdAt 2)
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips replay barrier control envelopes" $ do
            let item = HControl (ReplayBarrier "Responses response status was failed")
            fromJSON (toJSON item) `shouldBe` Success item

        it "round-trips reset-to-start control envelopes" $ do
            let item = HControl (ResetTo ResetToStart)
            fromJSON (toJSON item) `shouldBe` Success item

        it "rejects unknown historical schema versions" $ do
            let encoded =
                    object
                        [ "apiFamily" .= ("local" :: Text)
                        , "schemaVersion" .= (5 :: Int)
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

    historyItemIdAt :: Word32 -> HistoryItemId
    historyItemIdAt suffix =
        HistoryItemId (UUID.fromWords 0 0 0 suffix)

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
