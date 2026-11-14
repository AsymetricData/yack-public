#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-threads-test"
rm -rf "$OUT"
mkdir -p "$OUT"

for mode in heap stack; do
    ./yack build --memory="$mode" test/thread_spawn.yack "$OUT/thread-$mode" >/dev/null
    output=$("$OUT/thread-$mode" 2>&1)
    if [ "$output" = "thread 7" ]; then
        echo "ok threads_$mode"
    else
        echo "FAIL threads_$mode: output [$output]" >&2
        exit 1
    fi
done



./yack build test/thread_limit.yack "$OUT/thread-limit" >/dev/null
if output=$(bash -c "ulimit -u 1; exec '$OUT/thread-limit'" 2>&1); then
    echo "FAIL thread_create_limit: exited 0" >&2
    exit 1
fi
case "$output" in
    *"cannot create thread"*) echo "ok thread_create_limit" ;;
    *)
        echo "FAIL thread_create_limit: [$output]" >&2
        exit 1
        ;;
esac

echo "all thread checks passed"
