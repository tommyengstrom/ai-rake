# Message Streaming

Status: not implemented in the shared API yet.

## Current Behavior

- `chat` returns append-only `[HistoryItem]` suffixes.
- Provider adapters normalize finished responses after the HTTP request completes.
- Tool handling also operates on stable history items, but those items may be marked pending before the overall loop reaches a completed boundary.
- Media helper modules return decoded final responses instead of streaming events.

This is deliberate. The current generic model is strong for append-only text messages, tool calls, and tool results, but it is not yet rich enough to represent every provider-specific streaming delta without losing information.

## Why There Is No Shared Streaming Layer Yet

- OpenAI Responses and xAI Responses expose different native event shapes.
- Some provider-native content remains richer than the current generic `HistoryItem` representation.
- Persisting transport-level streaming deltas would blur the boundary between stable history items and ephemeral wire events.

## Likely Future Seam

If streaming is added, it should preserve provider-native detail until an event can be promoted into a stable `HistoryItem`.

The intended shape is:

- provider-specific streaming events stay native while the stream is live
- promotable message/tool items are emitted as `HistoryItem` with explicit pending/completed lifecycle
- storage persists append-only history items, not raw transport deltas

That keeps the existing `chat`/`withStorage` contract stable while leaving room for a future streaming API that can be more explicit about partial deltas, tool-call starts, tool-call argument chunks, and completion boundaries.
