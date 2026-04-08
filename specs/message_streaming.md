# Message Streaming

Status: implemented in the shared API as a text-first ephemeral layer.

## Current Behavior

- `chatOutcome` returns append-only `[HistoryItem]` suffixes.
- `streamChatOutcome` and `withResumableStreamingChat` emit live assistant text/refusal deltas through `StreamCallbacks`.
- Provider adapters still normalize only finished responses into stable `HistoryItem` values.
- Tool handling also operates on stable history items, but those items may be marked pending before the overall loop reaches a completed boundary.
- Media helper modules return decoded final responses instead of streaming events.
- Storage persists only finalized history items. Mid-stream crashes or disconnects do not write partial output.

This remains deliberate. The current generic model is strong for append-only text messages, tool calls, and tool results, but it is still not rich enough to represent every provider-specific streaming delta without losing information.

## Shared Streaming Boundary

- OpenAI Responses, xAI Responses, and Gemini Interactions expose different native event shapes.
- Some provider-native content remains richer than the current generic `HistoryItem` representation.
- Persisting transport-level streaming deltas would blur the boundary between stable history items and ephemeral wire events.

## Current Shape

- provider-specific streaming events stay native while the stream is live
- the shared API exposes only assistant text/refusal deltas
- finalized provider rounds are decoded with the same code path as non-streaming requests
- storage persists append-only history items, not raw transport deltas

That keeps the existing `chatOutcome`/`withStorage` contract stable while leaving room for a future streaming API that can be more explicit about provider-native deltas, tool-call starts, tool-call argument chunks, and completion boundaries.
