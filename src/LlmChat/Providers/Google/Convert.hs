{-# LANGUAGE RecordWildCards #-}

module LlmChat.Providers.Google.Convert where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lens (unpacked)
import Data.Vector qualified as V
import Effectful
import Effectful.Error.Static
import LlmChat.Providers.Google.Types
import LlmChat.Types
import LlmChat.Error
import Relude

-- | Clean JSON response by removing markdown code blocks if present.
cleanMarkdownJson :: Text -> Text
cleanMarkdownJson t =
    let stripped = T.strip t
        withoutPrefix
            | "```json" `T.isPrefixOf` stripped = T.drop 7 stripped
            | "```" `T.isPrefixOf` stripped = T.drop 3 stripped
            | otherwise = stripped
        withoutSuffix =
            if "```" `T.isSuffixOf` T.strip withoutPrefix
                then T.dropEnd 3 (T.strip withoutPrefix)
                else withoutPrefix
     in T.strip withoutSuffix

-- | Convert internal chat messages to Gemini content payloads.
toGeminiContent :: ChatMsg -> Maybe GeminiContent
toGeminiContent msg = case msg of
    SystemMsg{} -> Nothing -- handled separately
    UserMsg{content} ->
        Just
            $ GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "user"
                }
    AssistantMsg{content, toolCalls} ->
        let textParts
                | T.null content = []
                | otherwise = [GeminiTextPart content]
            toolCallParts = translateToolCall <$> toolCalls
         in Just
                $ GeminiContent
                    { parts = V.fromList $ textParts <> toolCallParts
                    , role = "model"
                    }
    ToolCallResponseMsg{toolCallId, toolResponse} ->
        Just
            $ GeminiContent
                { parts =
                    V.singleton
                        $ GeminiFunctionResponsePart
                        $ GeminiFunctionResponse
                            { name = toolCallId ^. typed @Text
                            , response = object ["result" .= (toolResponse ^. #response)]
                            }
                , role = "user"
                }
  where
    translateToolCall :: ToolCall -> GeminiPart
    translateToolCall tc =
        GeminiFunctionCallPart
            $ GeminiFunctionCall
                { name = tc ^. #toolName
                , args = Object (KM.fromMap (Map.mapKeys fromText (tc ^. #toolArgs)))
                }

-- | Convert tool declarations to Gemini function declarations.
mkToolFromDeclaration :: ToolDeclaration -> GeminiFunctionDeclaration
mkToolFromDeclaration t =
    GeminiFunctionDeclaration
        { name = t ^. #name
        , description = t ^. #description
        , parameters = case t ^. #parameterSchema of
            Nothing -> Just $ object ["type" .= ("object" :: Text), "properties" .= object []]
            Just schema -> Just $ removeUnsupportedSchemaProps schema
        }
  where
    removeUnsupportedSchemaProps :: Value -> Value
    removeUnsupportedSchemaProps = \case
        Object obj -> Object $ KM.delete "additionalProperties" $ KM.map removeUnsupportedSchemaProps obj
        Array arr -> Array $ fmap removeUnsupportedSchemaProps arr
        v -> v

-- | Peel off a system instruction from the message list when present.
extractSystemMessage :: [ChatMsg] -> (Maybe GeminiContent, [ChatMsg])
extractSystemMessage msgs = case msgs of
    (SystemMsg{content} : rest) ->
        ( Just
            $ GeminiContent
                { parts = V.singleton $ GeminiTextPart content
                , role = "user" -- Gemini expects the system prompt under this role
                }
        , rest
        )
    _ -> (Nothing, msgs)

-- | Convert Gemini content back into the internal chat message representation.
fromGeminiContent
    :: Error LlmChatError :> es => GeminiContent -> Eff es ChatMsg
fromGeminiContent content = case content ^. #role of
    "model" -> do
        let partsList = V.toList (content ^. #parts)
            textParts = [text | GeminiTextPart text <- partsList]
            combinedText = T.intercalate "" textParts
            cleanedText = cleanMarkdownJson combinedText
            functionCalls = [fc | GeminiFunctionCallPart fc <- partsList]
            assistantToolCalls = zipWith convertFunctionCallWithIndex [1 ..] functionCalls
        if null textParts && null functionCalls
            then throwError $ LlmExpectationError "Unexpected model content structure: no text or function calls"
            else
                pure
                    $ AssistantMsg
                        { content = if null textParts then "" else cleanedText
                        , toolCalls = assistantToolCalls
                        }
    _ ->
        throwError
            $ LlmExpectationError
            $ "Unexpected role in response: "
            <> content
            ^. #role
            . unpacked
  where
    convertFunctionCallWithIndex :: Int -> GeminiFunctionCall -> ToolCall
    convertFunctionCallWithIndex idx fc =
        ToolCall
            { toolCallId = ToolCallId $ fc ^. #name <> "_" <> T.pack (show idx)
            , toolName = fc ^. #name
            , toolArgs = case fc ^. #args of
                Object km -> Map.mapKeys Key.toText (KM.toMap km)
                _ -> mempty
            }
