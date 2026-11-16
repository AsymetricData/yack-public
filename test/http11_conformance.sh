#!/usr/bin/env bash



set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-http11-conformance"
rm -rf "$OUT"
mkdir -p "$OUT"

YACK="${YACK:-./yack}"
BASE_PORT="${PORT:-18140}"

pid=""

cleanup() {
    if [ -n "$pid" ]; then
        kill "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
        pid=""
    fi
}
trap cleanup EXIT

fail() {
    echo "FAIL $1: $2"
    exit 1
}


pick_port() {
    local candidate=$BASE_PORT attempt
    for attempt in $(seq 1 40); do
        if ! (exec 3<>"/dev/tcp/127.0.0.1/$candidate") 2>/dev/null; then
            echo "$candidate"
            return 0
        fi
        candidate=$((candidate + 1))
    done
    return 1
}

wait_port() {
    local i
    for i in $(seq 1 200); do
        if (exec 3<>"/dev/tcp/127.0.0.1/$PORT") 2>/dev/null; then
            return 0
        fi
        sleep 0.02
    done
    return 1
}

start_server() {
    local name=$1
    shift
    env "$@" "$OUT/app" >"$OUT/$name.log" 2>&1 &
    pid=$!
    if ! wait_port; then
        cat "$OUT/$name.log" >&2
        fail "$name" "server did not listen on port $PORT"
    fi
}

stop_server() {
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    pid=""
    sleep 0.05
}

run_case() {
    local name=$1
    shift
    start_server "$name" YACK_HTTP_PORT="$PORT" "$@"
    if ! node test/http11_conformance.mjs "$PORT" "$name" "$pid"; then
        stop_server
        cat "$OUT/$name.log" >&2
        fail "$name" "scenario failed"
    fi
    stop_server
}

PORT="$(pick_port)" || fail port "no free port near $BASE_PORT"

"$YACK" build test/http_server_app/src/main.yack "$OUT/app" >/dev/null


run_case conformance


run_case big-chunked YACK_HTTP_MAX_REQUEST_BYTES=2096128



run_case slow-large YACK_HTTP_MAX_CONNECTIONS=64 YACK_HTTP_MAX_RESPONSE_BYTES=8388608

echo "ok http11 conformance"
