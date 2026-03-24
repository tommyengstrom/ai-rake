{-# LANGUAGE DataKinds #-}

module Main where

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Rake
import Rake.Providers.OpenAI.Chat
import Relude hiding (lookupEnv)
import System.Environment (getEnv)

main :: IO ()
main = do
    apiKey <- toText <$> getEnv "OPENAI_API_KEY"
    runEffectStack apiKey do
        newItems <-
            chat
                defaultChatConfig{maxToolRounds = 8}
                [ system "You are a helpful assistant."
                , user "Hello! What's 2 + 2?"
                ]
        putTextLn "Assistant reply:"
        print (lastAssistantTextsStrict newItems)

runEffectStack
    :: Text
    -> Eff '[Rake, Error RakeError, Concurrent, IOE] a
    -> IO a
runEffectStack apiKey =
    runEff
        . runConcurrent
        . runErrorNoCallStackWith @RakeError (error . show)
        . runRakeOpenAIChat (defaultOpenAIChatSettings apiKey)
