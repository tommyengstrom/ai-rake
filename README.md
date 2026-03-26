# ai-rake

A Haskell library for provider-agnostic LLM chat with local tool execution, Effectful integration, and storage backends.

## Basic Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Rake
import Rake.Error (renderRakeError)
import Rake.Providers.OpenAI.Chat
import Relude
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- toText <$> getEnv "OPENAI_API_KEY"
  runEff
    . runConcurrent
    . runErrorNoCallStackWith @RakeError (error . toString . renderRakeError)
    $ runRakeOpenAIChat (defaultOpenAIChatSettings apiKey) do
        outcome <-
          chatOutcome
            defaultChatConfig{maxToolRounds = 8}
            [ system "You are a helpful assistant."
            , user "What is 2 + 2?"
            ]

        case outcome of
          ChatFinished{appendedItems} ->
            print (lastAssistantTextsStrict appendedItems)
          other ->
            print other
```

## Current Scope

- Canonical conversations are `[HistoryItem]`
- Generic chat entrypoint is `chatOutcome`
- `withResumableChat` is the recommended durable wrapper for agent loops that need persistence
- Supported chat providers are OpenAI Chat, xAI Chat, and Gemini Chat
- Shared/local history items cover text messages, tool calls, and tool results
- Provider-native history is preserved for OpenAI Responses, xAI Responses, and Gemini Interactions items
- Local storage backends support in-memory and PostgreSQL persistence

The old `ChatMsg` API and the `openai` package dependency have been removed.

Notes:

- `lastAssistantTexts` and `decodeLastAssistant` are best-effort helpers.
- `lastAssistantTextsStrict` and `decodeLastAssistantStrict` only look at the latest contiguous assistant tail.
- Canonical conversations are append-only agent logs. Provider-native items can be marked `ItemPending` or `ItemCompleted` inside persisted history.
- Anything named `appendedItems` is the newly returned append-only suffix, not the full conversation history.
- History items get stable `HistoryItemId`s before replay and persistence. When storage is used, the embedded item id matches the storage row id.
- `chatOutcome` and `withResumableChat` require `IOE` because the library assigns `HistoryItemId`s before replay and persistence.
- `chatOutcome` returns an append-only canonical suffix plus explicit pause/failure state. Failed outcomes append a durable `ReplayBarrier` control item into the returned suffix.
- `withResumableChat` is the recommended durable wrapper for append-only loops. It persists paused and failed suffixes through `chatOutcome`.
- Stored unresolved tool calls are resumed locally before the next provider request instead of being replayed back at the model.
- Historical unresolved tool calls whose local tool no longer exists are resumed into a synthetic `"Tool not found"` result and the loop continues.
- `chatOutcome` throws `ConversationBlocked` for blocked histories and includes the latest valid reset checkpoint when the supplied history already has stable item ids.
- `validResetCheckpoints`, `latestValidCheckpoint`, and `resetToLatestValidCheckpoint` work with `HistoryItemId` checkpoints, not raw log offsets. Use `resetTo` for item checkpoints and `resetToStart` to rewind to the beginning.
- `withStorage` and `withStorageBy` are snapshot-based helpers; they load the current history, run the action, and append the returned suffix.
- `withStorageBy chatOutcomeAppendedItems (chatOutcome config)` is the advanced low-level storage pattern for resumable agent loops, but `withResumableChat` is the preferred public entrypoint.
- `modifyConversationAtomic` is the short-lived mutation helper to use when concurrent writers matter.
- `renderRakeError` gives user-facing text for blocked conversations, including when no concrete reset checkpoint can be suggested for id-less direct histories.
- Shared/local multipart content is text-only for now; richer media remains provider-native until both adapters support the same generic representation.
- Provider-switch conversion warnings are logged to stderr by default when native items lose information during projection.
- OpenAI and xAI chat adapters target the Responses API; Gemini chat targets the Interactions API.
- Gemini-specific built-in Interactions tools are available through `GeminiChatSettings.providerTools`, while local function tools still flow through the shared chat loop.
- Switching away from Gemini keeps generic unresolved tool state portable. Gemini-only pending `thought` metadata is reused only for same-provider Gemini continuation and is dropped when replaying into OpenAI/xAI.
- Gemini image generation, OpenAI image generation, and xAI Grok Imagine image/video generation are available through provider-specific helper modules instead of the shared chat API.

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

Use `withResumableChat config conversationId` for resumable append-only agent logs, `withStorageBy chatOutcomeAppendedItems (chatOutcome config)` for advanced manual suffix persistence, and `modifyConversationAtomic` for short-lived read-modify-write mutations that must serialize correctly under concurrent access.

`chatOutcome` is the durable path for failed rounds. If a run returns `ChatFailed`, the returned `appendedItems` suffix includes a `ReplayBarrier`, and later `chatOutcome`/`withResumableChat` calls will refuse to continue until you append a reset control item that rewinds before the blocked suffix.

PostgreSQL item ids are unique per conversation, not globally. `setupConversationTables` also performs a best-effort migration away from the library's older global `item_id` uniqueness constraint; if an older installation renamed that legacy constraint manually, drop it yourself and rerun setup.

Minimal resumable loop:

```haskell
outcome <-
  withResumableChat
    defaultChatConfig{tools = [myTool]}
    conversationId

case outcome of
  ChatFinished{appendedItems} ->
    print (lastAssistantTextsStrict appendedItems)

  ChatPaused{pauseReason} ->
    print pauseReason

  ChatFailed{failureReason} ->
    print failureReason
```

Recover from a blocked append-only history:

```haskell
history <- getConversation conversationId

case resetToLatestValidCheckpoint history of
  Nothing ->
    pure ()
  Just rewindItem ->
    appendItems conversationId [rewindItem]
```

If you need to present all reset options to a user, inspect `validResetCheckpoints history`. The checkpoints target `HistoryItemId`s from the active conversation branch, not append-log offsets.
Compute those checkpoints from the full stored conversation, not from `appendedItems`.

## Media Generation

```haskell
{-# LANGUAGE DataKinds #-}

import Effectful
import Effectful.Error.Static
import Rake
import Rake.Error (renderRakeError)
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
    . runErrorNoCallStackWith @RakeError (error . toString . renderRakeError)
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
