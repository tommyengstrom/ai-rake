# Stateful Interface

Status: implemented.

## Canonical Conversation Type

The library now treats `[HistoryItem]` as the canonical conversation representation.

```haskell
chat
    :: ( Error LlmChatError :> es
       , LlmChat :> es
       )
    => ChatConfig es
    -> [HistoryItem]
    -> Eff es [HistoryItem]
```

`chat` is storage-agnostic. It only consumes a conversation snapshot and returns the newly produced history items from the current run.

## Storage Wrappers

Storage is opt-in and lives at the boundary:

```haskell
withStorage
    :: LlmChatStorage :> es
    => ([HistoryItem] -> Eff es [HistoryItem])
    -> ConversationId
    -> Eff es [HistoryItem]

withStorageBy
    :: LlmChatStorage :> es
    => (a -> [HistoryItem])
    -> ([HistoryItem] -> Eff es a)
    -> ConversationId
    -> Eff es a
```

Semantics:

- `withStorage` loads the current conversation snapshot.
- It runs the provided action with that snapshot.
- It appends only the returned suffix back to storage.
- It does not hold a database transaction or storage lock open during the LLM call.

That last point is intentional. Long-running network calls should not keep storage resources pinned.

## Concurrent Mutation Helper

When a caller needs a short-lived read-modify-write mutation that must serialize correctly, use `modifyConversationAtomic` instead of `withStorage`.

```haskell
modifyConversationAtomic
    :: LlmChatStorage :> es
    => ConversationId
    -> ([HistoryItem] -> (a, [HistoryItem]))
    -> Eff es a
```

Semantics:

- The callback receives the latest committed history for the conversation.
- It returns a result plus the new suffix to append.
- The in-memory backend serializes the whole mutation in one critical section.
- The PostgreSQL backend runs the mutation in one transaction pinned to one connection and locks the conversation row with `FOR UPDATE`.

`appendItems` is the shared batched append primitive used by both backends.

## PostgreSQL Configuration

PostgreSQL storage no longer accepts raw table-name `Text`.

Use validated identifiers instead:

```haskell
mkPgIdentifier :: Text -> Either Text PgIdentifier
conversationTablesFromPrefix :: PgIdentifier -> Either Text ConversationTables
setupConversationTables :: ConversationTables -> Eff es ()
```

This keeps table/index naming explicit, validated, and pool-safe.
