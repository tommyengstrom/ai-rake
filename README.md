# llmchat-effectful

A Haskell library for provider-agnostic LLM chat with local tool execution, Effectful integration, and storage backends.

## Basic Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import LlmChat
import LlmChat.Providers.OpenAI.Images
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
- OpenAI image generation and xAI Grok Imagine image/video generation are available through provider-specific helper modules instead of the shared `chat` API.

## Media Generation

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Error.Static
import LlmChat
import LlmChat.Providers.OpenAI.Images
import LlmChat.Providers.XAI.Imagine
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  openAiKey <- toText <$> getEnv "OPENAI_API_KEY"
  xaiKey <- toText <$> getEnv "XAI_API_KEY"
  runEff
    . runErrorNoCallStackWith @LlmChatError (error . show)
    $ do
        openAiImage <-
          generateOpenAIImage
            (defaultOpenAIImagesSettings openAiKey)
            (defaultOpenAIImageRequest "A clean product photo of a ceramic mug on linen")

        xaiVideo <-
          generateXAIVideo
            (defaultXAIImagineSettings xaiKey)
            ( defaultXAIImagineVideoRequest
                "Animate this still into a gentle dusk timelapse"
            ){imageUrl = Just "https://example.com/still.png", duration = Just 8}

        print openAiImage
        print xaiVideo
```

## CLIs

Build and run the image CLI with:

```bash
cabal run gen-image -- grok "a man riding a horse on the moon"
```

Build and run the video CLI with:

```bash
cabal run gen-video -- grok "She walk away" --image girl.jpg
cabal run gen-video -- grok --extend clip.mp4 "continue the scene for 5 more seconds"
```

Defaults:

- `gen-image openai ...` uses OpenAI `gpt-image-1.5`
- `gen-image grok ...` uses xAI Grok Imagine image generation
- `gen-video grok ...` uses xAI Grok Imagine video generation
- No `--output` writes to `./generated/<timestamp>-<slug>.png` for images
- No `--output` writes to `./generated/<timestamp>-<slug>.mp4` for videos
- `gen-video --extend ...` performs a true append by extracting the last frame locally, generating a continuation from that frame, and concatenating the clips; it requires local `ffmpeg` and `ffprobe`

The CLIs read `OPENAI_API_KEY` for OpenAI image generation and `XAI_API_KEY` for Grok Imagine image/video generation.

Use provider-specific help to see all available controls:

```bash
cabal run gen-image -- --help
cabal run gen-image -- openai --help
cabal run gen-image -- grok --help
cabal run gen-video -- --help
cabal run gen-video -- grok --help
```
