# llmchat-effectful

A Haskell library for provider-agnostic LLM chat with local tool execution, Effectful integration, and storage backends.

## Basic Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import LlmChat
import LlmChat.Providers.OpenAI.Responses

main :: IO ()
main = do
  apiKey <- getEnvText "OPENAI_API_KEY"
  runEff
    . runConcurrent
    . runErrorNoCallStackWith @LlmChatError (error . show)
    $ runLlmChatOpenAIResponses (defaultOpenAIResponsesSettings apiKey) do
        newItems <-
          chat
            defaultChatConfig
            [ system "You are a helpful assistant."
            , user "What is 2 + 2?"
            ]

        print (lastAssistantText newItems)
```

## Current Scope

- Canonical conversations are `[HistoryItem]`
- Generic chat entrypoint is `chat`
- Supported providers are OpenAI Responses and xAI Responses
- Local storage backends support in-memory and PostgreSQL persistence

The old `ChatMsg` API, Gemini integration, and the `openai` package dependency have been removed.
