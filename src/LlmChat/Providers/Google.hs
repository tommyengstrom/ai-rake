{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module LlmChat.Providers.Google where

import LlmChat.Effect
import LlmChat.Providers.Google.API
import LlmChat.Providers.Google.Convert
import LlmChat.Providers.Google.Types
import LlmChat.Types
import Control.Lens hiding ((.=))
import Data.Aeson (toJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.Client

-- | Default settings for Google provider
defaultGoogleSettings :: GoogleApiKey -> GoogleSettings es
defaultGoogleSettings apiKey =
    GoogleSettings
        { apiKey = apiKey
        , model = "gemini-2.5-flash"
        , baseUrl = "https://generativelanguage.googleapis.com"
        , requestLogger = \_ -> pure ()
        }

-- | Run the LlmChat effect using Google's Gemini API
runLlmChatGoogle
    :: forall es a
     . ( IOE :> es
       , Error LlmChatError :> es
       )
    => GoogleSettings es
    -> Eff (LlmChat ': es) a
    -> Eff es a
runLlmChatGoogle settings es = do
    manager <- liftIO newTlsManager
    let baseUrl' = parseBaseUrl $ T.unpack (settings ^. #baseUrl)
    case baseUrl' of
        Left err -> throwError $ LlmExpectationError $ "Invalid base URL: " <> show err
        Right url -> do
            let clientEnv = mkClientEnv manager url
            let generateContent = client geminiAPI
            runLlmChat generateContent clientEnv es
  where
    runLlmChat
        :: ([Text] -> Text -> GeminiChatRequest -> ClientM GeminiChatResponse)
        -> ClientEnv
        -> Eff (LlmChat ': es) a
        -> Eff es a
    runLlmChat generateContent clientEnv = interpret \_ -> \case
        GetLlmResponse tools' responseFormat messages -> do
            let hasTools = not (null tools')
            let tools =
                    if hasTools
                        then Just $ V.singleton $ GeminiTool $ V.fromList $ fmap mkToolFromDeclaration tools'
                        else Nothing

            let (originalSystemInstruction, otherMessages) = extractSystemMessage messages

            -- When we have both tools and structured output, use prompt instructions
            -- instead of generationConfig (which Google doesn't support with tools)
            let systemInstruction = case (hasTools, responseFormat, originalSystemInstruction) of
                    (True, JsonValue, Just sysContent) ->
                        Just $
                            sysContent
                                { parts =
                                    (sysContent ^. #parts)
                                        <> V.singleton
                                            ( GeminiTextPart
                                                "\n\nCRITICAL REQUIREMENT: You MUST respond with valid JSON only. Do not include any text outside the JSON object. Do not wrap in markdown code blocks."
                                            )
                                }
                    (True, JsonValue, Nothing) ->
                        Just $
                            GeminiContent
                                { role = "system"
                                , parts =
                                    V.singleton
                                        ( GeminiTextPart
                                            "CRITICAL REQUIREMENT: You MUST respond with valid JSON only. Do not include any text outside the JSON object. Do not wrap in markdown code blocks."
                                        )
                                }
                    (True, JsonSchema schema, Just sysContent) ->
                        Just $
                            sysContent
                                { parts =
                                    (sysContent ^. #parts)
                                        <> V.singleton
                                            ( GeminiTextPart $
                                                "\n\nCRITICAL REQUIREMENT: You MUST respond with valid JSON matching this schema. Do not include any text outside the JSON. Do not wrap in markdown code blocks.\nSchema:\n"
                                                    <> TL.toStrict (encodeToLazyText schema)
                                            )
                                }
                    (True, JsonSchema schema, Nothing) ->
                        Just $
                            GeminiContent
                                { role = "system"
                                , parts =
                                    V.singleton
                                        ( GeminiTextPart $
                                            "CRITICAL REQUIREMENT: You MUST respond with valid JSON matching this schema. Do not include any text outside the JSON. Do not wrap in markdown code blocks.\nSchema:\n"
                                                <> TL.toStrict (encodeToLazyText schema)
                                        )
                                }
                    _ -> originalSystemInstruction

            let contents = V.fromList $ mapMaybe toGeminiContent otherMessages
            let modelName = settings ^. #model
            let modelPath = [modelName <> ":generateContent"]

            -- Only use generationConfig when we don't have tools
            -- (Google doesn't support both simultaneously)
            let generationConfig =
                    if hasTools
                        then Nothing -- Can't use with tools, rely on prompt instructions instead
                        else case responseFormat of
                            Unstructured -> Nothing
                            JsonValue ->
                                Just $
                                    GeminiGenerationConfig
                                        { responseMimeType = Just "application/json"
                                        , responseSchema = Nothing
                                        }
                            JsonSchema schema ->
                                Just $
                                    GeminiGenerationConfig
                                        { responseMimeType = Just "application/json"
                                        , responseSchema = Just schema
                                        }
                req :: GeminiChatRequest
                req =
                    GeminiChatRequest
                        { contents = contents
                        , tools = tools
                        , systemInstruction = systemInstruction
                        , generationConfig = generationConfig
                        }
            response <-
                adapt clientEnv $
                    generateContent
                        modelPath
                        (settings ^. #apiKey . typed @Text)
                        req

            (settings ^. #requestLogger) (NativeMsgIn $ toJSON response)

            case response ^? #candidates . taking 1 folded . #content of
                Nothing -> throwError $ LlmExpectationError "No content in Gemini response"
                Just geminiContent -> do
                    fromGeminiContent  geminiContent

    adapt :: ClientEnv -> ClientM x -> Eff es x
    adapt env m = do
        result <- liftIO $ runClientM m env
        case result of
            Left err -> throwError . LlmClientError $ err
            Right x -> pure x
