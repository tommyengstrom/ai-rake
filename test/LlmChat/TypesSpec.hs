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

    describe "defineToolWithArgument" $ do
        it "preserves explicit additionalProperties from typed tool schemas" $ do
            let tool = defineToolWithArgument @OpenMapPayload "open_map" "open map tool" (\_ -> pure (Right "ok"))
                ToolDef{parameterSchema = parameterSchemaValue} = tool
            (parameterSchemaValue >>= lookupField "additionalProperties") `shouldBe` Just (Bool True)

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
