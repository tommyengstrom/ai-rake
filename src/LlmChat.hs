module LlmChat
    ( module X
    , module LlmChat
    ) where

import Control.Lens ((^.))
import Control.Monad.Morph
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.List qualified as L
import Data.Map qualified as Map
import Data.OpenApi (ToSchema, toInlinedSchema)
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static
import LlmChat.Effect as X
import LlmChat.Storage.Effect as X
import LlmChat.Tool as X
import LlmChat.Types as X
import Relude
import Servant.API (SourceIO)
import Servant.Types.SourceT

-- | Send a user message and handle any tool calls automatically
respondWithTools
    :: ( HasCallStack
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> [ChatMsg]
    -> Eff es [ChatMsg] -- Returns all new messages (assistant responses and tool calls)
respondWithTools tools conversation =
    fst <$> respondWithTools' Unstructured tools conversation

-- | Send a user message and handle any tool calls automatically
respondWithToolsStructured
    :: forall a es
     . ( HasCallStack
       , ToSchema a
       , FromJSON a
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> [ChatMsg]
    -> Eff es ([ChatMsg], a)
respondWithToolsStructured tools conversation = do
    (msgs, lastMsgContent) <-
        respondWithTools'
            (JsonSchema . toJSON . toInlinedSchema $ Proxy @a)
            tools
            conversation
    a <-
        either (throwError . LlmExpectationError) pure $
            eitherDecodeStrictText lastMsgContent
    pure (msgs, a)

respondWithToolsJson
    :: forall es
     . ( HasCallStack
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => [ToolDef es] -- Tools available for this conversation
    -> [ChatMsg]
    -> Eff es ([ChatMsg], Value)
respondWithToolsJson tools conversation = do
    (msgs, lastMsgContent) <- respondWithTools' JsonValue tools conversation
    a <-
        either (throwError . LlmExpectationError) pure $
            eitherDecodeStrictText lastMsgContent
    pure (msgs, a)

-- | Send a user message and handle any tool calls automatically
respondWithTools'
    :: ( HasCallStack
       , Error LlmChatError :> es
       , LlmChat :> es
       )
    => ResponseFormat
    -> [ToolDef es]
    -- ^ Tools available for these calls
    -> [ChatMsg]
    -> Eff es ([ChatMsg], Text)
    -- ^ Returns all new messages (assistant responses and tool calls)
respondWithTools' responseFormat tools conversation = do
    msgs <-
        either (throwError . LlmExpectationError) pure
            =<< runExceptT
                ( runStepT $
                    respondWithToolsStepT responseFormat tools conversation
                )

    case L.reverse msgs of
        AssistantMsg{content} : _ -> pure (msgs, content)
        msg : _ ->
            throwError
                . LlmExpectationError
                $ "Expected the last message to be an assitant message but got: "
                    <> show msg
        [] -> throwError $ LlmExpectationError "Assistant returned no messages"

-- | Execute tool calls and return the responses
executeToolCalls
    :: [ToolDef es]
    -> [ToolCall] -- FIXME: Just do a single tool call and traverse outside!
    -> Eff es [ChatMsg]
executeToolCalls tools toolCalls = do
    forM toolCalls $ \tc -> do
        response <- case find (\t -> t ^. #name == tc ^. #toolName) tools of
            Nothing ->
                pure . ToolResponse $ "Tool not found: " <> tc ^. #toolName
            Just tool -> do
                let args = Object (KM.fromMap (Map.mapKeys fromText (tc ^. #toolArgs)))
                result <- tool ^. #executeFunction $ args
                case result of
                    Right resp -> pure resp
                    Left err ->
                        pure . ToolResponse $ "Tool error: " <> Text.pack err
        pure $
            ToolCallResponseMsg
                { toolCallId = tc ^. #toolCallId
                , toolResponse = response
                }

respondWithToolsSourceIO
    :: forall es
     . ( Error LlmChatError :> es
       , LlmChat :> es
       , IOE :> es
       )
    => ResponseFormat
    -> [ToolDef es]
    -> [ChatMsg]
    -> Eff es (SourceIO ChatMsg)
respondWithToolsSourceIO responseFormat tools conversation =
    withUnliftStrategy SeqForkUnlift $
        withRunInIO $ \runInIO ->
            pure
                . hoist runInIO
                . fromStepT
                $ respondWithToolsStepT responseFormat tools conversation

-- | Tool loop implemented as a Servant stream of ChatMsgs
respondWithToolsStepT
    :: forall es
     . ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => ResponseFormat
    -> [ToolDef es]
    -> [ChatMsg]
    -> StepT (Eff es) ChatMsg
respondWithToolsStepT responseFormat tools conversation = Effect do
    response <- getLlmResponse (toToolDeclaration <$> tools) responseFormat conversation
    pure $ Yield response $ Effect do
        case response of
            -- Tool calls - execute them and continue
            AssistantMsg{toolCalls} | not (null toolCalls) -> do
                toolCallResponseMsgs <- executeToolCalls tools toolCalls
                let nextLlmCall :: StepT (Eff es) ChatMsg
                    nextLlmCall =
                        respondWithToolsStepT
                            responseFormat
                            tools
                            (conversation <> [response] <> toolCallResponseMsgs)
                pure $ L.foldr Yield nextLlmCall toolCallResponseMsgs
            _ -> pure Stop
  where
    toToolDeclaration tool =
        ToolDeclaration
            { name = tool ^. #name
            , description = tool ^. #description
            , parameterSchema = tool ^. #parameterSchema
            }

withStorage
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => ([ChatMsg] -> Eff es [ChatMsg])
    -> ConversationId
    -> Eff es [ChatMsg]
withStorage = withStorageBy id

withStorageStructured
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => ([ChatMsg] -> Eff es ([ChatMsg], a))
    -> ConversationId
    -> Eff es ([ChatMsg], a)
withStorageStructured = withStorageBy fst

withStorageBy
    :: ( HasCallStack
       , LlmChatStorage :> es
       )
    => (a -> [ChatMsg])
    -> ([ChatMsg] -> Eff es a)
    -> ConversationId
    -> Eff es a
withStorageBy extract action convId = do
    conversation <- getConversation convId
    result <- action conversation
    traverse_ (appendMessage convId) (extract result)
    pure result
