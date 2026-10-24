# airake

Haskell AI clients with typed model settings, automatic local tool calls, streaming,
and durable conversations. Applications normally depend on `airake` and use
OpenRouter; other backends remain independent packages.

| Package | Purpose | Depends on |
| --- | --- | --- |
| `airake` | Application facade, OpenRouter defaults, media CLIs | core, chat, OpenRouter |
| `airake-core` | Neutral history, effects, errors, media, backend interpreter API | No backend or database |
| `airake-chat` | Tool loop, replay, resumable conversations, storage interfaces and in-memory/directory storage | core |
| `airake-openrouter` | Chat, images, speech and video jobs through OpenRouter | core |
| `airake-openai` | Direct OpenAI chat, image and speech APIs | core |
| `airake-jev` | Independent `TypeSafe.Jev` API and `rake-decide` CLI | No Rake packages |
| `airake-postgres` | PostgreSQL conversation storage and response logger | core, chat |

`airake` reexports the public modules of core, chat and OpenRouter. Depending on it
is enough to import `Rake`, `Rake.Providers.OpenRouter.Images`, and the other
OpenRouter modules. It does not pull in PostgreSQL, direct OpenAI or Jev.

## Chat

```haskell
{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Rake
import Rake.MediaStorage.InMemory

main :: IO ()
main = do
    result <- runEff . runConcurrent . runErrorNoCallStack @RakeError
        . runRakeMediaStorageInMemory
        . runRakeOpenRouterChat (defaultOpenRouterSettings "your-key") defaultGpt41Mini
        $ chatOutcome defaultChatConfig [user "What is 2 + 2?"]
    print result
```

Use `CustomChatModel "author/model" settings` to select any supported chat model.
The adapter uses stateless `/responses`, including tools and SSE, and submits
history on every round. See [chat and persistence](docs/chat.md) for tool loops,
recovery, streaming, and storage semantics. The executable
[minimal example](packages/airake/app/MinimalExample.hs) reads `OPENROUTER_API_KEY`.

## Typed models and custom JSON

Each operation has its own model class: `OpenRouterChatModel`,
`OpenRouterImageModel`, `OpenRouterVideoModel`, and `OpenRouterSpeechModel`.
Model records hold model-specific settings; credentials, routing, logging and
transport timeouts live in `OpenRouterSettings`. Inputs live in request records.
Use `withOpenRouterManager` to reuse connections across standalone operations;
chat interpreters and video polling reuse connections within their workflows.

```haskell
import Rake.Providers.OpenRouter.Images
import Rake.Providers.OpenRouter.Models.GptImage25Flare qualified as Flare

flare :: GptImage25Flare
flare = defaultGptImage25Flare
    { Flare.quality = GptQualityHigh
    , Flare.dimensions = Just (ImageAspectRatio GptRatio16x9)
    }

-- Inside an Eff stack with IOE and Error RakeError:
-- generateOpenRouterImage settings flare (defaultImageRequest "A lighthouse")
```

`CustomImageModel`, `CustomVideoModel`, `CustomSpeechModel`, and `CustomChatModel`
accept a full model ID and an `Aeson.Object`. This supports newly released models
and options without a library release. Custom settings bypass the known-model
validation but cannot overwrite fields owned by the operation, such as `model`,
`prompt`, `input`, or tool/history fields. Client routing and nested provider
options merge recursively; conflicting explicit values fail rather than silently
winning. Model IDs and availability are controlled by OpenRouter.

The protocols differ: images return image data, speech returns binary audio,
and video creates a job which must be polled and downloaded. The library exposes
those lifecycles directly. `generateOpenRouterVideo` is a bounded polling convenience;
`startOpenRouterVideo` and `getOpenRouterVideo` support application-managed jobs.
Video downloads use the authenticated content endpoint on the configured base URL.

## Command-line tools

All media CLIs use `OPENROUTER_API_KEY`. Options accept `--flag VALUE` and
`--flag=VALUE`; unknown full `author/model` IDs are accepted.

```sh
rake-image --quality medium "A lighthouse"
rake-image -m openai/gpt-image-2.5-flare --quality high --output-format webp "A lighthouse"
rake-image -m google/gemini-nano-banana-2.1 --resolution 2K "A lighthouse"
rake-image -m author/new-model --model-options '{"new_option":true}' "A lighthouse"
rake-video -m google/veo-3.1 --duration 8 --resolution 1080p "Waves around a lighthouse"
rake-tts --voice eve -o greeting.mp3 "Hello there"
```

Image defaults remain Grok Imagine Image 2.0 with **low** quality. The typed
OpenAI default is **GPT Image 2.5 Flare**; Sunburst is also available. Gemini image
options include Nano Banana 2.1, Flash Lite Image 3.1, and Pro Image 3. Video defaults
to Grok Imagine Video 1.5, with Lite and Veo 3.1 options. Speech defaults to Grok
Voice TTS 1.0, Eve, MP3.

`--model-options` replaces known-model defaults with the supplied object and
allows new settings on existing models too. Explicit flags may supplement it;
duplicate keys are rejected. `--image` accepts local files, HTTPS URLs and data
URLs. `rake-video --extend` extracts the last frame and appends a generated
continuation locally using ffmpeg; it is not a native video extension request.
`rake-tts` plays audio locally unless `--output` is supplied; raw PCM requires an
output path. See each CLI's `--help` for supported options.

Direct-provider features absent from OpenRouter's normalized API are explicit
migration gaps, including masks/file IDs and some image safety/thinking controls,
native video edits, and some provider-specific speech codecs. The CLIs reject
unsupported native flags. Direct OpenAI APIs remain available in `airake-openai`.

Jev is unchanged in `airake-jev`; `rake-decide` still uses `TYPESAFE_API_KEY`.
See [Jev documentation](docs/jev.md).

## Backend authors and migration

Implement the `Rake` effect using the public `Rake.Backend.runChatProvider` helper
and return `ProviderRound`s. Depend on `airake-core` only. The
[independent backend fixture](fixtures/backend/Main.hs) compiles and tests that
boundary without importing chat, a concrete backend, or a database. Frontend
applications choose a backend interpreter around `chatOutcome`.

The package dependency changes are intentional for version 0.2. Existing `Rake.*`
module names remain where practical; `Rake.Chat` exposes the chat frontend on its
own. Stored history JSON, provider-family identifiers, item IDs and PostgreSQL
schemas are unchanged. See [migration notes](docs/migration-0.2.md).

## Development

Start `nix develop -c process-compose up` from the repository root. The server
uses port 6599. Use the managed jobs instead of invoking builds/tests separately:

```sh
process-compose process restart backend-3-cabal-build-all
process-compose process restart backend-4-cabal-test-all
process-compose process logs backend-4-cabal-test-all
```

The default suite is offline and includes local HTTP fixtures. `ghcid.log` contains
compiler feedback. `AIRAKE_REPL_TARGET` selects watched components; the default watches all seven
libraries. Use `AIRAKE_REPL_PROJECT=cabal.integration.project` for optional
integration components. The HLS cradle uses that project so optional test modules
can be edited without running their tests.

The following jobs are disabled until explicitly started:

- `backend-6-build-integration`: compile the database and direct OpenAI live suites.
- `backend-7-test-postgres`: create a disposable PostgreSQL cluster, run storage/logger tests, then remove it.
- `backend-8-test-live-openai`: paid live tests, requiring `AIRAKE_LIVE_TESTS=1` and `OPENAI_API_KEY` in the manager environment.
- `backend-9-sdist`: build all seven packages and their offline tests from extracted source distributions.

Each package is independently distributable. Nix exposes all seven packages and
uses `airake` as its default output.
