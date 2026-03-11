{-# LANGUAGE RecordWildCards #-}

module LlmChat.Providers.Responses.Internal
    ( ProviderTag (..)
    , ResponsesProviderConfig (..)
    , historyItemToGenericItems
    , runResponsesProvider
    ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as Vector
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import LlmChat.Effect
import LlmChat.Internal.Schema (closeOpenObjectSchemas)
import LlmChat.Types
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.API (Header, JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client

data ProviderTag
    = ProviderOpenAI
    | ProviderXAI
    deriving stock (Show, Eq)

data ResponsesProviderConfig es = ResponsesProviderConfig
    { providerTag :: ProviderTag
    , apiKey :: Text
    , baseUrl :: Text
    , model :: Text
    , organizationId :: Maybe Text
    , projectId :: Maybe Text
    , requestLogger :: NativeMsgFormat -> Eff es ()
    }

type ResponsesAPI =
    "v1"
        Servant.:> "responses"
        Servant.:> Header "Authorization" Text
        Servant.:> Header "OpenAI-Organization" Text
        Servant.:> Header "OpenAI-Project" Text
        Servant.:> ReqBody '[JSON] Value
        Servant.:> Post '[JSON] Value

responsesApi :: Proxy ResponsesAPI
responsesApi = Proxy

runResponsesProvider
    :: forall es a
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => ResponsesProviderConfig es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runResponsesProvider config@ResponsesProviderConfig{..} eff = do
    manager <- liftIO newTlsManager
    parsedBaseUrl <- either (throwError . invalidBaseUrl) pure $ parseBaseUrl (toString baseUrl)
    let clientEnv = mkClientEnv manager parsedBaseUrl
        postResponse =
            client responsesApi

    interpretWith eff \_ -> \case
        GetLlmResponse tools responseFormat history -> do
            requestBody <- buildRequestBody config tools responseFormat history
            requestLogger (NativeMsgOut requestBody)
            responseValue <-
                liftIO
                    ( runClientM
                        ( postResponse
                            (Just ("Bearer " <> apiKey))
                            organizationId
                            projectId
                            requestBody
                        )
                        clientEnv
                    )
                    >>= \case
                        Left err -> do
                            requestLogger (NativeRequestFailure err)
                            throwError (LlmClientError err)
                        Right response -> pure response
            requestLogger (NativeMsgIn responseValue)
            either throwError pure $ decodeResponse providerTag responseValue
  where
    invalidBaseUrl err =
        LlmExpectationError ("Invalid base URL: " <> show err)

buildRequestBody
    :: ResponsesProviderConfig es
    -> [ToolDeclaration]
    -> ResponseFormat
    -> [HistoryItem]
    -> Eff es Value
buildRequestBody ResponsesProviderConfig{..} tools responseFormat history = do
    input <- fmap concat $ traverse (renderHistoryItem providerTag requestLogger) history
    pure $
        object $
            [ "model" .= model
            , "input" .= input
            , "store" .= False
            ]
                <> toolFields
                <> responseFormatFields
  where
    toolFields
        | null tools = []
        | otherwise = ["tools" .= fmap toolDeclarationToValue tools]

    responseFormatFields =
        maybe [] (\formatValue -> ["text" .= object ["format" .= formatValue]]) $
            responseFormatToValue responseFormat

responseFormatToValue :: ResponseFormat -> Maybe Value
responseFormatToValue = \case
    Unstructured ->
        Nothing
    JsonValue ->
        Just $ object ["type" .= ("json_object" :: Text)]
    JsonSchema schema ->
        Just $
            object
                [ "type" .= ("json_schema" :: Text)
                , "name" .= ("response_format" :: Text)
                , "schema" .= schema
                , "strict" .= True
                ]

toolDeclarationToValue :: ToolDeclaration -> Value
toolDeclarationToValue ToolDeclaration{name, description, parameterSchema} =
    object $
        [ "type" .= ("function" :: Text)
        , "name" .= name
        , "description" .= description
        , "parameters" .= fromMaybe emptyToolParametersSchema parameterSchema
        ]

emptyToolParametersSchema :: Value
emptyToolParametersSchema =
    closeOpenObjectSchemas $
        object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]

renderHistoryItem
    :: ProviderTag
    -> (NativeMsgFormat -> Eff es ())
    -> HistoryItem
    -> Eff es [Value]
renderHistoryItem providerTag requestLogger = \case
    HLocal localItem ->
        pure (renderLocalItem localItem)
    HOpenAIResponses (OpenAIResponsesItem NativeResponseItem{payload})
        | providerTag == ProviderOpenAI ->
            pure [payload]
        | otherwise ->
            projectForeignItem payload
    HXAIResponses (XAIResponsesItem NativeResponseItem{payload})
        | providerTag == ProviderXAI ->
            pure [payload]
        | otherwise ->
            projectForeignItem payload
  where
    projectForeignItem payload = do
        let (notes, genericItems) = nativePayloadToGenericItems payload
        traverse_ (requestLogger . NativeConversionNote) notes
        pure (genericItemToValue <$> genericItems)

renderLocalItem :: LocalItem -> [Value]
renderLocalItem = \case
    LocalSystem{content} ->
        [messageValue "system" content]
    LocalDeveloper{content} ->
        [messageValue "developer" content]
    LocalUser{content} ->
        [messageValue "user" content]
    LocalAssistantText{content} ->
        [messageValue "assistant" content]
    LocalToolCall{toolCall = localToolCall} ->
        [toolCallValue localToolCall]
    LocalToolResult{toolResult = localToolResult} ->
        [toolResultValue localToolResult]

messageValue :: Text -> Text -> Value
messageValue role content =
    object
        [ "role" .= role
        , "content" .= content
        ]

toolCallValue :: ToolCall -> Value
toolCallValue ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs} =
    object
        [ "type" .= ("function_call" :: Text)
        , "call_id" .= toolCallId
        , "name" .= toolName
        , "arguments" .= encodeObjectText toolArgs
        ]

toolResultValue :: ToolResult -> Value
toolResultValue ToolResult{toolCallId = ToolCallId toolCallId, toolResponse = ToolResponse{response}} =
    object
        [ "type" .= ("function_call_output" :: Text)
        , "call_id" .= toolCallId
        , "output" .= response
        ]

genericItemToValue :: GenericItem -> Value
genericItemToValue = \case
    GenericMessage{role, content} ->
        messageValue (genericRoleToText role) content
    GenericToolCall{toolCall = genericToolCall'} ->
        toolCallValue genericToolCall'
    GenericToolResult{toolResult = genericToolResult'} ->
        toolResultValue genericToolResult'

genericRoleToText :: GenericRole -> Text
genericRoleToText = \case
    GenericSystem -> "system"
    GenericDeveloper -> "developer"
    GenericUser -> "user"
    GenericAssistant -> "assistant"

decodeResponse :: ProviderTag -> Value -> Either LlmChatError [HistoryItem]
decodeResponse providerTag responseValue = do
    responseObject <- expectObject "response" responseValue
    let responseId = lookupText "id" responseObject
    outputValue <-
        maybe
            (Left (LlmExpectationError "Responses API returned no output array"))
            Right
            (KM.lookup "output" responseObject)
    outputItems <- expectArray "response.output" outputValue
    forM (Vector.toList outputItems) $ \payload -> do
        payloadObject <- expectObject "response.output item" payload
        let nativeItemId = lookupText "id" payloadObject
            nativeItem =
                NativeResponseItem
                    { responseId
                    , nativeItemId
                    , payload
                    }
        pure $ case providerTag of
            ProviderOpenAI ->
                HOpenAIResponses (OpenAIResponsesItem nativeItem)
            ProviderXAI ->
                HXAIResponses (XAIResponsesItem nativeItem)

historyItemToGenericItems :: HistoryItem -> ([Text], [GenericItem])
historyItemToGenericItems = \case
    HLocal localItem ->
        ([], [localItemToGenericItem localItem])
    HOpenAIResponses (OpenAIResponsesItem NativeResponseItem{payload}) ->
        nativePayloadToGenericItems payload
    HXAIResponses (XAIResponsesItem NativeResponseItem{payload}) ->
        nativePayloadToGenericItems payload

localItemToGenericItem :: LocalItem -> GenericItem
localItemToGenericItem = \case
    LocalSystem{content} ->
        GenericMessage GenericSystem content
    LocalDeveloper{content} ->
        GenericMessage GenericDeveloper content
    LocalUser{content} ->
        GenericMessage GenericUser content
    LocalAssistantText{content} ->
        GenericMessage GenericAssistant content
    LocalToolCall{toolCall = localToolCall} ->
        GenericToolCall localToolCall
    LocalToolResult{toolResult = localToolResult} ->
        GenericToolResult localToolResult

nativePayloadToGenericItems :: Value -> ([Text], [GenericItem])
nativePayloadToGenericItems payload =
    case payload of
        Object itemObject ->
            case (lookupText "type" itemObject, lookupText "role" itemObject) of
                (Just "function_call", _) ->
                    case parseToolCall itemObject of
                        Nothing ->
                            (["Dropped malformed function_call item"], [])
                        Just genericToolCall' ->
                            ([], [GenericToolCall genericToolCall'])
                (Just "function_call_output", _) ->
                    case parseToolResult itemObject of
                        Nothing ->
                            (["Dropped malformed function_call_output item"], [])
                        Just genericToolResult' ->
                            ([], [GenericToolResult genericToolResult'])
                (Just "message", _) ->
                    messageObjectToGeneric itemObject
                (Nothing, Just _) ->
                    messageObjectToGeneric itemObject
                (Just unsupportedType, _) ->
                    ([unsupportedItemTypeNote unsupportedType], [])
                _ ->
                    (["Dropped malformed native response item"], [])
        _ ->
            (["Dropped non-object native response item"], [])
  where
    unsupportedItemTypeNote itemType =
        "Dropped unsupported native response item type: " <> itemType

messageObjectToGeneric :: Object -> ([Text], [GenericItem])
messageObjectToGeneric itemObject =
    case roleTextToGenericRole =<< lookupText "role" itemObject of
        Nothing ->
            (["Dropped message item with unsupported role"], [])
        Just role ->
            case extractContentText (KM.lookup "content" itemObject) of
                (notes, Nothing) ->
                    (notes <> ["Dropped message item without text content"], [])
                (notes, Just content) ->
                    (notes, [GenericMessage role content])

extractContentText :: Maybe Value -> ([Text], Maybe Text)
extractContentText = \case
    Nothing ->
        (["Missing content field"], Nothing)
    Just (String content) ->
        ([], Just content)
    Just (Array parts) ->
        let (notes, texts) = unzip (map partToText (Vector.toList parts))
            combinedText = mconcat (catMaybes texts)
         in (mconcat notes, if combinedText == "" then Nothing else Just combinedText)
    Just _ ->
        (["Unsupported message content format"], Nothing)
  where
    partToText :: Value -> ([Text], Maybe Text)
    partToText = \case
        String content ->
            ([], Just content)
        Object partObject ->
            case lookupText "text" partObject of
                Just text ->
                    ([], Just text)
                Nothing ->
                    let partType = fromMaybe "unknown" (lookupText "type" partObject)
                     in (["Dropped unsupported message part type: " <> partType], Nothing)
        _ ->
            (["Dropped unsupported message part"], Nothing)

parseToolCall :: Object -> Maybe ToolCall
parseToolCall itemObject = do
    ToolCallId toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    toolName <- lookupText "name" itemObject
    toolArgs <- parseToolArgs =<< KM.lookup "arguments" itemObject
    pure ToolCall{toolCallId = ToolCallId toolCallId, toolName, toolArgs}

parseToolResult :: Object -> Maybe ToolResult
parseToolResult itemObject = do
    toolCallId <- ToolCallId <$> lookupText "call_id" itemObject
    outputValue <- KM.lookup "output" itemObject
    pure $
        ToolResult
            { toolCallId
            , toolResponse = ToolResponse (valueToCompactText outputValue)
            }

parseToolArgs :: Value -> Maybe (Map Text Value)
parseToolArgs = \case
    String argumentsText ->
        case eitherDecodeStrictText argumentsText of
            Right (Object objectValue) ->
                Just (Map.mapKeys Key.toText (KM.toMap objectValue))
            _ ->
                Nothing
    Object objectValue ->
        Just (Map.mapKeys Key.toText (KM.toMap objectValue))
    _ ->
        Nothing

valueToCompactText :: Value -> Text
valueToCompactText = \case
    String text ->
        text
    other ->
        TL.toStrict (encodeToLazyText other)

encodeObjectText :: Map Text Value -> Text
encodeObjectText args =
    TL.toStrict $
        encodeToLazyText $
            Object (KM.fromMap (Map.mapKeys Key.fromText args))

roleTextToGenericRole :: Text -> Maybe GenericRole
roleTextToGenericRole = \case
    "system" -> Just GenericSystem
    "developer" -> Just GenericDeveloper
    "user" -> Just GenericUser
    "assistant" -> Just GenericAssistant
    _ -> Nothing

lookupText :: Key -> Object -> Maybe Text
lookupText key objectValue = KM.lookup key objectValue >>= \case
    String text -> Just text
    _ -> Nothing

expectObject :: Text -> Value -> Either LlmChatError Object
expectObject label = \case
    Object objectValue ->
        Right objectValue
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an object"))

expectArray :: Text -> Value -> Either LlmChatError (Vector.Vector Value)
expectArray label = \case
    Array values ->
        Right values
    _ ->
        Left (LlmExpectationError ("Expected " <> toString label <> " to be an array"))
