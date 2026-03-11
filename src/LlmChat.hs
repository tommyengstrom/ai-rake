module LlmChat
    ( module X
    , chat
    , itemText
    , lastAssistantText
    , decodeLastAssistant
    , withStorage
    , withStorageBy
    ) where

import Data.Aeson (FromJSON, Value (Object), eitherDecodeStrictText)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.List qualified as List
import Data.Map qualified as Map
import Effectful
import Effectful.Error.Static
import LlmChat.Effect as X
import LlmChat.Providers.Responses.Internal (historyItemToGenericItems)
import LlmChat.Storage.Effect as X
import LlmChat.Tool as X
import LlmChat.Types as X
import Relude

chat
    :: ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es [HistoryItem]
chat ChatConfig{tools, responseFormat, onItem} conversation =
    handleToolLoop tools responseFormat onItem conversation []

itemText :: HistoryItem -> Maybe Text
itemText = \case
    HLocal LocalSystem{content} -> Just content
    HLocal LocalDeveloper{content} -> Just content
    HLocal LocalUser{content} -> Just content
    HLocal LocalAssistantText{content} -> Just content
    HLocal LocalToolResult{toolResult = ToolResult{toolResponse = ToolResponse{response}}} ->
        Just response
    HLocal LocalToolCall{} ->
        Nothing
    historyItem ->
        viaNonEmpty head
            [ text
            | GenericMessage{content = text} <- projected historyItem
            ]
      where
        projected item = snd (historyItemToGenericItems item)

lastAssistantText :: [HistoryItem] -> Maybe Text
lastAssistantText history =
    listToMaybe
        [ text
        | historyItem <- List.reverse history
        , GenericMessage{role = GenericAssistant, content = text} <- snd (historyItemToGenericItems historyItem)
        ]

decodeLastAssistant
    :: forall a
     . FromJSON a
    => [HistoryItem]
    -> Either LlmChatError a
decodeLastAssistant history = do
    assistantReply <-
        maybe
            (Left (LlmExpectationError "Assistant returned no text"))
            Right
            (lastAssistantText history)
    first LlmExpectationError (eitherDecodeStrictText assistantReply)

executeToolCalls :: [ToolDef es] -> [ToolCall] -> Eff es [HistoryItem]
executeToolCalls tools toolCalls =
    forM toolCalls $ \ToolCall{toolCallId, toolName = requestedToolName, toolArgs} -> do
        toolResponse <- case find (matchesTool requestedToolName) tools of
            Nothing ->
                pure . ToolResponse $ "Tool not found: " <> requestedToolName
            Just ToolDef{executeFunction} -> do
                let args = Object (KM.fromMap (Map.mapKeys fromText toolArgs))
                result <- executeFunction args
                pure $
                    either
                        (ToolResponse . ("Tool error: " <>) . toText)
                        identity
                        result
        pure (toolResult toolCallId toolResponse)
      where
        matchesTool requestedToolName ToolDef{name} = name == requestedToolName

collectToolCalls :: [HistoryItem] -> [ToolCall]
collectToolCalls historyItems =
    [ genericToolCall'
    | historyItem <- historyItems
    , GenericToolCall{toolCall = genericToolCall'} <- snd (historyItemToGenericItems historyItem)
    ]

handleToolLoop
    :: ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es]
    -> ResponseFormat
    -> (HistoryItem -> Eff es ())
    -> [HistoryItem]
    -> [HistoryItem]
    -> Eff es [HistoryItem]
handleToolLoop tools responseFormat onItem conversation accumulated = do
    response <- getLlmResponse (toToolDeclaration <$> tools) responseFormat (conversation <> accumulated)
    traverse_ onItem response

    let toolCalls = collectToolCalls response
    if null toolCalls
        then pure (accumulated <> response)
        else do
            toolCallResults <- executeToolCalls tools toolCalls
            traverse_ onItem toolCallResults
            handleToolLoop
                tools
                responseFormat
                onItem
                conversation
                (accumulated <> response <> toolCallResults)
  where
    toToolDeclaration ToolDef{name, description, parameterSchema} =
        ToolDeclaration{name, description, parameterSchema}

withStorage
    :: ( LlmChatStorage :> es
       )
    => ([HistoryItem] -> Eff es [HistoryItem])
    -> ConversationId
    -> Eff es [HistoryItem]
withStorage = withStorageBy identity

withStorageBy
    :: ( LlmChatStorage :> es
       )
    => (a -> [HistoryItem])
    -> ([HistoryItem] -> Eff es a)
    -> ConversationId
    -> Eff es a
withStorageBy extract action convId = do
    conversation <- getConversation convId
    result <- action conversation
    appendItems convId (extract result)
    pure result
