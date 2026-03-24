# ai-rake

A Haskell library for provider-agnostic LLM chat with local tool execution, Effectful integration, and storage backends.

## Basic Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Rake
import Rake.Providers.OpenAI.Chat
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- toText <$> getEnv "OPENAI_API_KEY"
  runEff
    . runConcurrent
    . runErrorNoCallStackWith @RakeError (error . show)
    $ runRakeOpenAIChat (defaultOpenAIChatSettings apiKey) do
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
- `chatOutcome` is the low-level variant for agent loops that need explicit pause/failure state
- Supported chat providers are OpenAI Chat, xAI Chat, and Gemini Chat
- Shared/local history items cover text messages, tool calls, and tool results
- Provider-native history is preserved for OpenAI Responses, xAI Responses, and Gemini Interactions items
- Local storage backends support in-memory and PostgreSQL persistence

The old `ChatMsg` API and the `openai` package dependency have been removed.

Notes:

- `lastAssistantTexts` and `decodeLastAssistant` are best-effort helpers.
- `lastAssistantTextsStrict` and `decodeLastAssistantStrict` only look at the latest contiguous assistant tail.
- Canonical conversations are append-only agent logs. Provider-native items can be marked `ItemPending` or `ItemCompleted` inside persisted history.
- `chat` is strict in control flow, not transcript shape: it throws on paused and failed rounds, but successful runs can still include earlier pending tool calls or assistant output in the returned canonical suffix.
- `chatOutcome` returns append-only canonical history plus explicit pause/failure state.
- Stored unresolved tool calls are resumed locally before the next provider request instead of being replayed back at the model.
- `withStorage` and `withStorageBy` are snapshot-based convenience wrappers around `chat`; they load the current history, run the action, and append the returned canonical suffix.
- `withStorageBy chatOutcomeItems (chatOutcome config)` is the safe low-level storage pattern for resumable agent loops.
- `modifyConversationAtomic` is the short-lived mutation helper to use when concurrent writers matter.
- Shared/local multipart content is text-only for now; richer media remains provider-native until both adapters support the same generic representation.
- Provider-switch conversion warnings are logged to stderr by default when native items lose information during projection.
- OpenAI and xAI chat adapters target the Responses API; Gemini chat targets the Interactions API.
- Gemini-specific built-in Interactions tools are available through `GeminiChatSettings.providerTools`, while local function tools still flow through the shared `chat` loop.
- Gemini image generation, OpenAI image generation, and xAI Grok Imagine image/video generation are available through provider-specific helper modules instead of the shared `chat` API.

## Persistence

PostgreSQL storage now uses validated identifiers instead of raw table-name `Text`:

```haskell
{-# LANGUAGE DataKinds #-}

import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Effectful
import Effectful.PostgreSQL.Connection.Pool
import Rake
import Rake.Storage.Postgres
import Relude
import UnliftIO.Pool (mkDefaultPoolConfig, newPool)

main :: IO ()
main = do
  prefix <- either (error . toString) pure (mkPgIdentifier "ai_rake")
  tables <- either (error . toString) pure (conversationTablesFromPrefix prefix)
  config <- mkDefaultPoolConfig (connectPostgreSQL "dbname=chatcompletion-test") close 60 5
  pool <- newPool config

  runEff
    . runWithConnectionPool pool
    $ setupConversationTables tables
```

Use `withStorage (chat config) conversationId` for strict load-run-append chat flows, `withStorageBy chatOutcomeItems (chatOutcome config) conversationId` for resumable append-only agent logs, and `modifyConversationAtomic` for short-lived read-modify-write mutations that must serialize correctly under concurrent access.

Minimal resumable loop:

```haskell
outcome <-
  withStorageBy
    chatOutcomeItems
    (chatOutcome defaultChatConfig{tools = [myTool]})
    conversationId

case outcome of
  ChatFinished{historyItems} ->
    print (lastAssistantTextsStrict historyItems)

  ChatPaused{pauseReason} ->
    print pauseReason

  ChatFailed{failureReason} ->
    print failureReason
```

## Media Generation

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Error.Static
import Rake
import Rake.Providers.Gemini.Images
import Rake.Providers.OpenAI.Images
import Rake.Providers.XAI.Imagine
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  geminiKey <- toText <$> getEnv "GEMINI_API_KEY"
  openAiKey <- toText <$> getEnv "OPENAI_API_KEY"
  xaiKey <- toText <$> getEnv "XAI_API_KEY"
  runEff
    . runErrorNoCallStackWith @RakeError (error . show)
    $ do
        geminiImage <-
          generateGeminiImage
            (defaultGeminiImagesSettings geminiKey)
            (defaultGeminiImageRequest "A tiny watercolor postcard of a lighthouse")

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

        print geminiImage
        print openAiImage
        print xaiVideo
```

Validation notes:

- OpenAI `mask` and `inputFidelity` require at least one input image.
- xAI `videoUrl` requests are edit operations and cannot be combined with `duration`, `aspectRatio`, or `resolution`.
- xAI video requests may set `imageUrl` or `videoUrl`, but not both.

## CLIs

Build and run the image CLI with:

```bash
cabal run gen-image -- imagine "a man riding a horse on the moon"
```

Build and run the video CLI with:

```bash
cabal run gen-video -- grok "She walk away" --image girl.jpg
cabal run gen-video -- grok --extend clip.mp4 "continue the scene for 5 more seconds"
```

Defaults:

- `gen-image gptimage ...` uses OpenAI `gpt-image-1.5`
- `gen-image imagine ...` uses xAI Grok Imagine image generation
- `gen-image banana2 ...` uses Gemini `gemini-2.5-flash-image`
- `gen-video grok ...` uses xAI Grok Imagine video generation
- No `--output` writes to `./generated/<timestamp>-<slug>.png` for images
- No `--output` writes to `./generated/<timestamp>-<slug>.mp4` for videos
- `gen-video --extend ...` performs a true append by extracting the last frame locally, generating a continuation from that frame, and concatenating the clips; it requires local `ffmpeg` and `ffprobe`

The CLIs read `OPENAI_API_KEY` for `gptimage`, `XAI_API_KEY` for `imagine` and `gen-video`, and `GEMINI_API_KEY` for `banana2`.

Use provider-specific help to see all available controls:

```bash
cabal run gen-image -- --help
cabal run gen-image -- gptimage --help
cabal run gen-image -- imagine --help
cabal run gen-image -- banana2 --help
cabal run gen-video -- --help
cabal run gen-video -- grok --help
```
