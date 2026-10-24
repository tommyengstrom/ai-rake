ai-rake is a library for supplier agnostic llms calls with automatic tool call handling.

## Workflow

- `process-compose` is the expected local build/test/ghcid interface.
- Assume `process-compose` is already running outside of the agent process; use
  the configured `process-compose process ...` commands against that manager.
- Do not run `cabal build` / `cabal test` directly; use the configured
  `process-compose` processes instead.
- The output from ghcid is continuously written to `ghcid.log`; read it to get
  compiler feedback.

Start the process manager from the repo root with:

```bash
nix develop -c process-compose up
```

The Nix dev shell sets `PC_PORT_NUM=6599`, so process-compose should use port
6599 for its server.

Available processes:

- `backend-1-cabal-update` - update Cabal package indexes
- `backend-2-cabal-build-all-deps` - build dependencies only, after update
- `backend-3-cabal-build-all` - full build, after dependencies
- `backend-4-cabal-test-all` - full test run, after full build
- `backend-5-ghcid` - ghcid via `run-ghcid.sh`, after full build

Useful commands:

```bash
process-compose process logs backend-5-ghcid
process-compose process logs backend-4-cabal-test-all
process-compose process restart backend-5-ghcid
process-compose process restart backend-4-cabal-test-all
process-compose process list
```

## Workspace

The application facade is `packages/airake`. Core types and backend authoring
helpers live in `packages/airake-core`; the chat/tool/replay frontend is
`packages/airake-chat`. OpenRouter, direct OpenAI, Jev, and PostgreSQL each have
separate packages. The independent backend fixture is in `fixtures/backend`.

The default test job is offline. Additional jobs are disabled until explicitly
started:

- `backend-6-build-integration` compiles optional integration suites.
- `backend-7-test-postgres` runs against a disposable local PostgreSQL cluster.
- `backend-8-test-live-openai` requires `AIRAKE_LIVE_TESTS=1` and credentials;
  running it makes paid provider calls.
- `backend-9-sdist` builds extracted distributions without workspace sources.

Use `cabal.integration.project` for editor/repl integration components. It enables
the integration build flags without running tests.
