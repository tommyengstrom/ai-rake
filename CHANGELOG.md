# Changelog for `ai-rake`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Changed canonical conversation storage to an append-only agent log. Provider-native items now carry `ItemPending` or `ItemCompleted`, and unresolved tool calls or incomplete assistant output are persisted as ordinary history instead of being hidden in transient loop state.
- Replaced `chatOutcomeFinalizedItems` with `chatOutcomeItems` as the low-level storage extractor for resumable `chatOutcome` flows.
- Added broader shared loop tests plus deterministic OpenAI/xAI/Gemini decoder tests for completed, tool-handoff, paused, failed, and thought-only rounds.
- Added provider-specific media generation helpers for OpenAI Images (`gpt-image-1.5` by default) and xAI Grok Imagine image/video generation.
- Added a `gen-image` CLI for OpenAI and Grok image generation, plus a separate `gen-video` CLI for Grok video generation from an image, video edits, and local append-style `--extend` continuations.
- Moved PostgreSQL response logging onto the same effectful `WithConnection` abstraction as the storage backend.
- Replaced raw PostgreSQL table-name `Text` with validated `PgIdentifier` and `ConversationTables` configuration.
- Added batched appends plus `modifyConversationAtomic` for short-lived concurrent storage mutations.
- Made OpenAI/xAI media helpers fail fast on unsupported option combinations instead of silently dropping request fields.
- Split the large shared Responses and xAI Imagine implementation modules into smaller internal units.

## 0.1.0.0 - YYYY-MM-DD
