#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-sql-test"
rm -rf "$OUT"
mkdir -p "$OUT"
./yack build bench/sql/smoke.yack "$OUT/smoke" >/dev/null

actual=$("$OUT/smoke")
expected=$'id=1\nname=alice\nid=2\nname=bob'
if [ "$actual" != "$expected" ]; then
    echo "sql: unexpected output" >&2
    printf 'expected:\n%s\nactual:\n%s\n' "$expected" "$actual" >&2
    exit 1
fi





expect_error() {
    local name=$1
    local code=$2
    if ./yack check "test/sql/$name.yack" >"$OUT/$name.log" 2>&1; then
        echo "sql: $name was accepted" >&2
        exit 1
    fi
    if ! grep -q "$code" "$OUT/$name.log"; then
        echo "sql: $name: missing $code diagnostic" >&2
        cat "$OUT/$name.log" >&2
        exit 1
    fi
}
expect_error forgot_finalize E0403
expect_error close_twice E0402
expect_error use_after_close E0402
expect_error forgot_close E0403
expect_error close_with_open_stmt E0501

./yack build test/sql/prepare_finalize_close.yack "$OUT/prepare_finalize_close" >/dev/null

actual=$("$OUT/prepare_finalize_close")
if [ "$actual" != "0" ]; then
    echo "sql: finalize then close did not succeed" >&2
    printf 'actual:\n%s\n' "$actual" >&2
    exit 1
fi

./yack build test/sql/open_failure.yack "$OUT/open_failure" >/dev/null

actual=$("$OUT/open_failure")
expected="open failed"
if [ "$actual" != "$expected" ]; then
    echo "sql: invalid open was not reported" >&2
    printf 'expected:\n%s\nactual:\n%s\n' "$expected" "$actual" >&2
    exit 1
fi

rm -f /tmp/yack-sql-pool-readers.db /tmp/yack-sql-pool-readers.db-shm \
    /tmp/yack-sql-pool-readers.db-wal
./yack build test/sql/pool_readers.yack "$OUT/pool_readers" >/dev/null

actual=$("$OUT/pool_readers")
expected=$'fallback 3\nthird none\nreader 3\nreaders 2'
if [ "$actual" != "$expected" ]; then
    echo "sql: pool readers did not go out and back in" >&2
    printf 'expected:\n%s\nactual:\n%s\n' "$expected" "$actual" >&2
    exit 1
fi

echo "ok sql"
