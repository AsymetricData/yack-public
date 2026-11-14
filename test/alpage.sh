#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-alpage-test"
rm -rf "$OUT"
mkdir -p "$OUT"

PORT=18092
YACK="${YACK:-./yack}"

if curl -s -m 0.2 "http://127.0.0.1:$PORT/" >/dev/null 2>&1; then
    echo "alpage: port $PORT already in use" >&2
    exit 1
fi

"$YACK" build examples/alpage/src/test.yack "$OUT/test" >/dev/null
TEST_OUTPUT=$("$OUT/test")
printf '%s\n' "$TEST_OUTPUT"
if printf '%s\n' "$TEST_OUTPUT" | grep -q 'FAIL'; then
    echo "alpage: in-memory tests failed" >&2
    exit 1
fi


for mode in heap stack; do
    "$YACK" build --memory=$mode examples/alpage_tutorial/src/test.yack \
        "$OUT/tutorial-$mode" >/dev/null
    TUTORIAL_OUTPUT=$("$OUT/tutorial-$mode" | grep -E '^(ok|FAIL)')
    printf '%s\n' "$TUTORIAL_OUTPUT" | sed "s/^/tutorial $mode: /"
    if printf '%s\n' "$TUTORIAL_OUTPUT" | grep -q 'FAIL'; then
        echo "alpage: tutorial tests failed ($mode)" >&2
        exit 1
    fi
done
"$YACK" build examples/alpage_tutorial/src/main.yack "$OUT/tutorial" >/dev/null

if [ -d site ]; then node test/site_snippets.mjs; fi

"$YACK" build examples/alpage/src/main.yack "$OUT/server" >/dev/null
"$OUT/server" >"$OUT/server.log" 2>&1 &
pid=$!
trap 'kill "$pid" 2>/dev/null || true; wait "$pid" 2>/dev/null || true' EXIT

ready=""
for _ in $(seq 1 100); do
    if curl -s -m 0.2 "http://127.0.0.1:$PORT/" >/dev/null 2>&1; then
        ready=1
        break
    fi
    sleep 0.02
done

if [ -z "$ready" ]; then
    echo "alpage: server did not answer" >&2
    cat "$OUT/server.log" >&2
    exit 1
fi

node test/alpage_socket.mjs "$PORT"

body=$(curl -s -m 2 "http://127.0.0.1:$PORT/")
[ "$body" = "hello" ] || { echo "alpage: GET body '$body'" >&2; exit 1; }

status=$(curl -s -m 2 -o /dev/null -w '%{http_code}' "http://127.0.0.1:$PORT/missing")
[ "$status" = "404" ] || { echo "alpage: 404 status '$status'" >&2; exit 1; }

if ! kill -0 "$pid" 2>/dev/null; then
    echo "alpage: server died" >&2
    cat "$OUT/server.log" >&2
    exit 1
fi

kill "$pid" 2>/dev/null || true
wait "$pid" 2>/dev/null || true



"$YACK" build --memory=stack --arena-reserve=64M \
    examples/alpage/src/main.yack "$OUT/server-stack" >/dev/null
"$OUT/server-stack" >"$OUT/server-stack.log" 2>&1 &
pid=$!

ready=""
for _ in $(seq 1 100); do
    if curl -s -m 0.2 "http://127.0.0.1:$PORT/" >/dev/null 2>&1; then
        ready=1
        break
    fi
    sleep 0.02
done

if [ -z "$ready" ]; then
    echo "alpage: stack server did not answer" >&2
    cat "$OUT/server-stack.log" >&2
    exit 1
fi

if command -v wrk >/dev/null 2>&1; then
    if ! wrk -t2 -c10 -d2s "http://127.0.0.1:$PORT/" \
         >"$OUT/stack.wrk" 2>&1; then
        echo "alpage: stack wrk failed" >&2
        cat "$OUT/stack.wrk" >&2
        exit 1
    fi
    count=$(awk '/requests in/ { print $1 }' "$OUT/stack.wrk")
    if [ "${count:-0}" -lt 10000 ]; then
        echo "alpage: stack only ${count:-0} requests" >&2
        cat "$OUT/stack.wrk" >&2
        exit 1
    fi
fi

if ! kill -0 "$pid" 2>/dev/null; then
    echo "alpage: stack server died" >&2
    cat "$OUT/server-stack.log" >&2
    exit 1
fi

echo "ok alpage stack"
echo "ok alpage"
