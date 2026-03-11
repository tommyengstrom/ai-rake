{-# LANGUAGE DataKinds #-}

module Main where

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import LlmChat
import LlmChat.Providers.OpenAI.Responses
import Relude hiding (lookupEnv)
import System.Environment (getEnv)

main :: IO ()
main = do
    apiKey <- toText <$> getEnv "OPENAI_API_KEY"
    runEffectStack apiKey do
        reply <-
            chat
                defaultChatConfig
                [ system "You are a helpful assistant."
                , user "Hello! What's 2 + 2?"
                ]
        putTextLn "Assistant reply:"
        print reply

runEffectStack
    :: Text
    -> Eff '[LlmChat, Error LlmChatError, Concurrent, IOE] a
    -> IO a
runEffectStack apiKey =
    runEff
        . runConcurrent
        . runErrorNoCallStackWith @LlmChatError (error . show)
        . runLlmChatOpenAIResponses (defaultOpenAIResponsesSettings apiKey)
