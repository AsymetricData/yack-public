#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../.."

export LC_ALL=C

OUT="${TMPDIR:-/tmp}/yack-cli-size"
YACK="${YACK:-./yack}"
HERE="$(cd "$(dirname "$0")" && pwd)"

rm -rf "$OUT"
mkdir -p "$OUT"

echo "== build =="
"$YACK" build "$HERE/main.yack" "$OUT/yack"
"$YACK" build --trim "$HERE/main.yack" "$OUT/yack-trim"

if command -v go >/dev/null 2>&1; then
    (cd "$HERE" && go build -o "$OUT/go" main.go)
    (cd "$HERE" && go build -trimpath -ldflags="-s -w" -o "$OUT/go-strip" main.go)
else
    echo "go: not installed, skipped" >&2
fi

if command -v cargo >/dev/null 2>&1; then
    if ! (cd "$HERE" && cargo build --release --offline -q 2>/dev/null); then
        (cd "$HERE" && cargo build --release -q)
    fi
    cp "$HERE/target/release/cli_size" "$OUT/rust"
    cp "$OUT/rust" "$OUT/rust-strip"
    strip "$OUT/rust-strip"
else
    echo "cargo: not installed, skipped" >&2
fi

echo "== check =="
for binary in yack yack-trim go go-strip rust rust-strip; do
    [ -x "$OUT/$binary" ] || continue
    for expected in "Ada:--name Ada" "Yack:"; do
        want="Hello from ${expected%%:*} !"
        flag="${expected#*:}"
        if [ -n "$flag" ]; then
            got=$("$OUT/$binary" $flag)
        else
            got=$("$OUT/$binary")
        fi
        if [ "$got" != "$want" ]; then
            echo "$binary: expected [$want], got [$got]" >&2
            exit 1
        fi
    done
done
if command -v node >/dev/null 2>&1; then
    got=$(node "$HERE/main.ts" --name Ada)
    [ "$got" = "Hello from Ada !" ] || { echo "ts: unexpected output [$got]" >&2; exit 1; }
fi
echo "all implementations print the same greeting"

echo "== size (bytes) =="
printf '%-14s %10s\n' "binary" "bytes"
for binary in yack yack-trim go go-strip rust rust-strip; do
    [ -f "$OUT/$binary" ] || continue
    printf '%-14s %10s\n' "$binary" "$(wc -c < "$OUT/$binary")"
done

echo "== source lines (non-blank, non-comment) =="
printf '%-14s %10s\n' "language" "lines"
count() { grep -v -E '^\s*$|^\s*(//|#|\(\*)' "$1" | wc -l; }
printf '%-14s %10s\n' "yack" "$(count "$HERE/main.yack")"
printf '%-14s %10s\n' "go" "$(count "$HERE/main.go")"
printf '%-14s %10s\n' "typescript" "$(count "$HERE/main.ts")"
printf '%-14s %10s\n' "rust" "$(count "$HERE/main.rs")"
