module LlmChat.Internal.Schema
    ( normalizeStructuredOutputSchema
    , closeOpenObjectSchemas
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Relude

normalizeStructuredOutputSchema :: Value -> Value
normalizeStructuredOutputSchema = normalizeSchema

closeOpenObjectSchemas :: Value -> Value
closeOpenObjectSchemas = normalizeStructuredOutputSchema

normalizeSchema :: Value -> Value
normalizeSchema = \case
    Object objectValue ->
        normalizeNullableKeyword $
            if isObjectSchema normalizedObject
                then normalizeObjectSchema normalizedObject
                else Object normalizedObject
      where
        normalizedObject = KM.mapWithKey normalizeSchemaField objectValue
    Array values ->
        Array (normalizeSchema <$> values)
    other ->
        other

normalizeSchemaField :: Key.Key -> Value -> Value
normalizeSchemaField fieldName fieldValue
    | fieldName `elem` schemaMapFields =
        normalizeSchemaMap fieldValue
    | otherwise =
        normalizeSchema fieldValue

normalizeSchemaMap :: Value -> Value
normalizeSchemaMap = \case
    Object objectValue ->
        Object (KM.map normalizeSchema objectValue)
    other ->
        normalizeSchema other

normalizeObjectSchema :: KM.KeyMap Value -> Value
normalizeObjectSchema objectValue =
    Object $
        addAdditionalProperties $
            KM.insert "required" requiredProperties $
                updatePropertiesField objectValue normalizedProperties
  where
    propertiesObject = objectProperties objectValue
    originallyRequired = requiredPropertyNames objectValue
    normalizedProperties =
        KM.mapWithKey normalizeProperty propertiesObject
    normalizeProperty propertyName schemaValue
        | Key.toText propertyName `elem` originallyRequired = schemaValue
        | otherwise = makeSchemaNullable schemaValue
    requiredProperties =
        toJSON (Key.toText <$> KM.keys normalizedProperties)

updatePropertiesField :: KM.KeyMap Value -> KM.KeyMap Value -> KM.KeyMap Value
updatePropertiesField objectValue normalizedProperties = case KM.lookup "properties" objectValue of
    Just Object{} ->
        KM.insert "properties" (Object normalizedProperties) objectValue
    _ ->
        objectValue

addAdditionalProperties :: KM.KeyMap Value -> KM.KeyMap Value
addAdditionalProperties objectValue
    | KM.member "additionalProperties" objectValue = objectValue
    | otherwise = KM.insert "additionalProperties" (Bool False) objectValue

normalizeNullableKeyword :: Value -> Value
normalizeNullableKeyword = \case
    Object objectValue -> case KM.lookup "nullable" objectValue of
        Just (Bool True) ->
            makeSchemaNullable (Object (KM.delete "nullable" objectValue))
        Just _ ->
            Object (KM.delete "nullable" objectValue)
        Nothing ->
            Object objectValue
    other ->
        other

makeSchemaNullable :: Value -> Value
makeSchemaNullable schemaValue
    | schemaAllowsNull schemaValue = schemaValue
    | otherwise = object ["anyOf" .= ([schemaValue, nullSchema] :: [Value])]

schemaAllowsNull :: Value -> Bool
schemaAllowsNull = \case
    Bool booleanValue ->
        booleanValue
    Object objectValue ->
        hasNullType objectValue
            || hasNullTypeArrayMember objectValue
            || hasNullConst objectValue
            || hasNullEnumMember objectValue
            || anySchemaAllowsNull "anyOf" objectValue
            || anySchemaAllowsNull "oneOf" objectValue
            || allSchemaAllowNull "allOf" objectValue
    _ ->
        False

hasNullType :: KM.KeyMap Value -> Bool
hasNullType objectValue =
    KM.lookup "type" objectValue == Just (String "null")

hasNullTypeArrayMember :: KM.KeyMap Value -> Bool
hasNullTypeArrayMember objectValue = case KM.lookup "type" objectValue of
    Just (Array values) ->
        any (== String "null") values
    _ ->
        False

hasNullConst :: KM.KeyMap Value -> Bool
hasNullConst objectValue =
    KM.lookup "const" objectValue == Just Null

hasNullEnumMember :: KM.KeyMap Value -> Bool
hasNullEnumMember objectValue = case KM.lookup "enum" objectValue of
    Just (Array values) ->
        any (== Null) values
    _ ->
        False

anySchemaAllowsNull :: Key.Key -> KM.KeyMap Value -> Bool
anySchemaAllowsNull fieldName objectValue = case KM.lookup fieldName objectValue of
    Just (Array values) ->
        any schemaAllowsNull values
    _ ->
        False

allSchemaAllowNull :: Key.Key -> KM.KeyMap Value -> Bool
allSchemaAllowNull fieldName objectValue = case KM.lookup fieldName objectValue of
    Just (Array values) ->
        not (null values) && all schemaAllowsNull values
    _ ->
        False

requiredPropertyNames :: KM.KeyMap Value -> [Text]
requiredPropertyNames objectValue = case KM.lookup "required" objectValue of
    Just (Array values) ->
        mapMaybe requiredPropertyName (toList values)
    _ ->
        []

requiredPropertyName :: Value -> Maybe Text
requiredPropertyName = \case
    String fieldName ->
        Just fieldName
    _ ->
        Nothing

objectProperties :: KM.KeyMap Value -> KM.KeyMap Value
objectProperties objectValue = case KM.lookup "properties" objectValue of
    Just (Object propertiesObject) ->
        propertiesObject
    _ ->
        mempty

isObjectSchema :: KM.KeyMap Value -> Bool
isObjectSchema objectValue =
    KM.lookup "type" objectValue == Just (String "object")
        || KM.member "properties" objectValue

nullSchema :: Value
nullSchema = object ["type" .= ("null" :: Text)]

schemaMapFields :: [Key.Key]
schemaMapFields =
    [ "$defs"
    , "definitions"
    , "dependentSchemas"
    , "patternProperties"
    , "properties"
    ]
