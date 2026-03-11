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
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- toText <$> getEnv "OPENAI_API_KEY"
  runEff
    . runConcurrent
    . runErrorNoCallStackWith @LlmChatError (error . show)
    $ runLlmChatOpenAIResponses (defaultOpenAIResponsesSettings apiKey) do
        newItems <-
          chat
            defaultChatConfig{maxToolRounds = 8}
            [ system "You are a helpful assistant."
            , user "What is 2 + 2?"
            ]

        print (lastAssistantTextsStrict newItems)
```

## Current Scope

- Canonical conversations are `[HistoryItem]`
- Generic chat entrypoint is `chat`
- Supported providers are OpenAI Responses and xAI Responses
- Shared/local history items cover text messages, tool calls, and tool results
- Local storage backends support in-memory and PostgreSQL persistence

The old `ChatMsg` API, Gemini integration, and the `openai` package dependency have been removed.

Notes:

- `lastAssistantTexts` and `decodeLastAssistant` are best-effort helpers.
- `lastAssistantTextsStrict` and `decodeLastAssistantStrict` only look at the latest contiguous assistant tail.
- Shared/local multipart content is text-only for now; richer media remains provider-native until both adapters support the same generic representation.
- Provider-switch conversion warnings are logged to stderr by default when native items lose information during projection.
