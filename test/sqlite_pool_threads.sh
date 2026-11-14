#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-sqlite-pool-threads-test"
DB="/tmp/yack-sqlite-pool-threads.db"
rm -rf "$OUT"
mkdir -p "$OUT"
rm -f "$DB" "$DB-shm" "$DB-wal"

for mode in heap stack; do
    rm -f "$DB" "$DB-shm" "$DB-wal"
    ./yack build --memory="$mode" --arena-reserve=64M \
        bench/sqlite_pool/main.yack "$OUT/pool-$mode" >/dev/null
    output=$("$OUT/pool-$mode" 2>&1)
    readers=$(printf '%s\n' "$output" | grep -c '^reader 1000$' || true)
    if [ "$readers" -eq 10 ] && printf '%s\n' "$output" | grep -qx 'done'; then
        echo "ok sqlite_pool_threads_$mode"
    else
        echo "FAIL sqlite_pool_threads_$mode" >&2
        printf '%s\n' "$output" >&2
        exit 1
    fi
done

echo "all sqlite pool thread checks passed"
