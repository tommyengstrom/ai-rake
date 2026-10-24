#!/usr/bin/env bash
set -euo pipefail

project="${AIRAKE_REPL_PROJECT:-cabal.project}"
restart_args=(--restart cabal.project --restart "$project")
targets=()
for manifest in packages/*/*.cabal; do
    restart_args+=(--restart "$manifest")
    package_name="$(basename "$manifest" .cabal)"
    targets+=("$package_name:lib:$package_name")
done
if [[ -n "${AIRAKE_REPL_TARGET:-}" ]]; then
    read -r -a targets <<< "$AIRAKE_REPL_TARGET"
fi
printf -v repl_command '%q ' cabal repl --enable-multi-repl -O0 --project-file "$project" "${targets[@]}"
exec ghcid --command "$repl_command" "${restart_args[@]}" -o ghcid.log
