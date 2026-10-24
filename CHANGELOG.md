# Changelog for `ai-rake`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Split the workspace into `airake`, `airake-core`, `airake-chat`, `airake-openrouter`, `airake-openai`, `airake-jev`, and `airake-postgres` (0.2.0.0).
- Made OpenRouter the facade/CLI default with typed per-model settings and custom JSON for chat, images, speech and video jobs. Retired direct xAI/Gemini clients; direct OpenAI and Jev remain opt-in.
- Added a public backend interpreter API, neutral provider errors, offline HTTP contract tests, an independent backend fixture, and explicit PostgreSQL/live test jobs.
- Preserved stored history JSON, IDs and database schemas. See `docs/migration-0.2.md` for source API and CLI migration details.

### Earlier changes in this development cycle


- Updated OpenAI image defaults to `gpt-image-2.5-flare` and xAI defaults to `grok-imagine-image-2.0` with explicit low quality. The image CLI also supports Sunburst and both 2.5 dated snapshots, retains legacy models, and accepts `--quality VALUE` or `--quality=VALUE` for both providers.
- Added `quality` to `XAIImagineImageRequest` and `safetyIdentifier` to `OpenAIImageRequest`, with a corresponding `--safety-identifier` CLI option. Legacy Grok requires quality to be omitted. Transparent OpenAI output now accepts PNG/WebP, including the `gpt-image-2` preview capability.
- Added optional `model`, `outputFormat`, `size`, `quality`, `background`, and `usage` fields to `ImageGenerationResponse`, plus `mimeType` to `GeneratedImage`. Usage preserves provider-specific JSON, including xAI cost ticks; Gemini image MIME and response metadata are retained. Callers constructing these public records directly must supply the added fields; old response JSON remains readable.
- Added typed OpenAI and xAI Responses reasoning-effort settings, omitted by default. OpenAI supports `none`, `low`, `medium`, `high`, `xhigh`, and `max`; xAI supports `none`, `low`, `medium`, and `high`.
- Migrated Gemini Interactions requests, responses, structured output, and streaming to the current step-based API schema.
- Changed canonical conversation storage to an append-only agent log. Provider-native items now carry `ItemPending` or `ItemCompleted`, and unresolved tool calls or incomplete assistant output are persisted as ordinary history instead of being hidden in transient loop state.
- Replaced `chatOutcomeFinalizedItems` with `chatOutcomeItems` as the low-level storage extractor for resumable `chatOutcome` flows.
- Added embedded `HistoryItemId`s plus `ResetCheckpoint`-based recovery helpers for append-only replay, including `validResetCheckpoints`, `latestValidCheckpoint`, `resetToLatestValidCheckpoint`, `resetTo`, and `resetToStart`.
- Provider-backed history items now store the local `ToolDeclaration`s that were available to the model for the request that produced the provider item.
- `chat` and `chatOutcome` now require `IOE` so the library can assign `HistoryItemId`s before replay and persistence.
- Added durable replay barriers for failed `chatOutcome` runs, and a typed `ConversationBlocked` error for strict `chat` callers.
- Historical unresolved tool calls now resume into a synthetic `"Tool not found"` result when the local tool is no longer configured, instead of being silently dropped.
- Added broader shared loop tests plus deterministic OpenAI/xAI/Gemini decoder tests for completed, tool-handoff, paused, failed, and thought-only rounds.
- Added provider-specific media generation helpers for OpenAI Images (`gpt-image-1.5` by default) and xAI Grok Imagine image/video generation.
- Added a `rake-image` CLI for OpenAI and Grok image generation, a separate `rake-video` CLI for Grok video generation from an image, video edits, and local append-style `--extend` continuations, and a `rake-tts` CLI for OpenAI/xAI speech generation with local playback by default and optional `--output` saving.
- Moved PostgreSQL response logging onto the same effectful `WithConnection` abstraction as the storage backend.
- Replaced raw PostgreSQL table-name `Text` with validated `PgIdentifier` and `ConversationTables` configuration.
- Added batched appends plus `modifyConversationAtomic` for short-lived concurrent storage mutations.
- Made OpenAI/xAI media helpers fail fast on unsupported option combinations instead of silently dropping request fields.
- Split the large shared Responses and xAI Imagine implementation modules into smaller internal units.

## 0.1.0.0 - YYYY-MM-DD
