#!/usr/bin/env bash




set -u
cd "$(dirname "$0")/.."

YACK=./yack
OUT=$(mktemp -d)
trap 'rm -rf "$OUT"' EXIT
DB=/tmp/yack-sqlite-pool-threads.db
failed=0

fail() {
    echo "FAIL $1" >&2
    shift
    printf '%s\n' "$@" >&2
    failed=1
}

compile() {
    local name=$1 source=$2 memory=$3 sanitizer=$4 link=$5
    shift 5
    if ! "$YACK" llvm --memory="$memory" --arena-reserve=64M \
         "$@" "$source" > "$OUT/$name.ll" 2> "$OUT/$name.build"; then
        fail "$name llvm" "$(head -5 "$OUT/$name.build")"
        return 1
    fi
    if ! clang -O1 -g -fsanitize="$sanitizer" -Wno-override-module \
         $link -o "$OUT/$name" "$OUT/$name.ll" > "$OUT/$name.cc" 2>&1; then
        fail "$name clang" "$(head -5 "$OUT/$name.cc")"
        return 1
    fi
}

sanitizer_hit() {
    grep -Eq 'WARNING: ThreadSanitizer|ERROR: AddressSanitizer|ThreadSanitizer:DEADLYSIGNAL' "$1"
}

run_pool() {
    local name=$1 log="$OUT/$1.log"
    rm -f "$DB" "$DB-shm" "$DB-wal"
    timeout 60 "$OUT/$name" > "$log" 2>&1
    local status=$?
    local readers
    readers=$(grep -c '^reader 1000$' "$log" || true)
    if [ "$status" -ne 0 ] || [ "$readers" -ne 10 ] || ! grep -qx 'done' "$log"; then
        fail "$name run" "status $status, $readers readers"
        tail -5 "$log"
        return
    fi
    if sanitizer_hit "$log"; then
        fail "$name sanitizer" "$(head -10 "$log")"
    else
        echo "ok   $name"
    fi
}

for sanitizer in thread address; do
    quarantine=""
    [ "$sanitizer" = address ] && quarantine="--arena-quarantine=strict"

    compile "pool-$sanitizer" bench/sqlite_pool/main.yack heap \
        "$sanitizer" "-lsqlite3" $quarantine && run_pool "pool-$sanitizer"
done

if [ "$failed" -ne 0 ]; then
    echo "thread sanitizer checks failed"
    exit 1
fi
echo "threads: no data race, no memory error"
