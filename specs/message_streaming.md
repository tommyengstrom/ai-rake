# Message Streaming

Status: not implemented in the shared API yet.

## Current Behavior

- `chat` returns completed `[HistoryItem]` values.
- Provider adapters normalize finished responses after the HTTP request completes.
- Tool handling also operates on completed history items, not partial deltas.
- Media helper modules return decoded final responses instead of streaming events.

This is deliberate. The current generic model is strong for completed text messages, tool calls, and tool results, but it is not yet rich enough to represent every provider-specific streaming delta without losing information.

## Why There Is No Shared Streaming Layer Yet

- OpenAI Responses and xAI Responses expose different native event shapes.
- Some provider-native content remains richer than the current generic `HistoryItem` representation.
- Persisting partial streaming state would blur the boundary between finalized history and transient transport events.

## Likely Future Seam

If streaming is added, it should preserve provider-native detail until an event can be promoted into a stable `HistoryItem`.

The intended shape is:

- provider-specific streaming events stay native while the stream is live
- finalized message/tool items are emitted as `HistoryItem`
- storage persists only finalized items

That keeps the existing `chat`/`withStorage` contract stable while leaving room for a future streaming API that can be more explicit about partial deltas, tool-call starts, tool-call argument chunks, and completion boundaries.
