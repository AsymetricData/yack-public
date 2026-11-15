#!/usr/bin/env bash



set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-http-limits-test"
rm -rf "$OUT"
mkdir -p "$OUT"

YACK="${YACK:-./yack}"
BASE_PORT="${PORT:-18120}"

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
    if ! node test/http_server_limits.mjs "$PORT" "$name"; then
        stop_server
        cat "$OUT/$name.log" >&2
        fail "$name" "scenario failed"
    fi
    stop_server
}

check_invalid_config() {
    local name=invalid-request-bytes
    local log="$OUT/$name.log"
    local status=0
    YACK_HTTP_PORT="$PORT" YACK_HTTP_MAX_REQUEST_BYTES=100 \
        timeout 10 "$OUT/app" >"$log" 2>&1 || status=$?
    if [ "$status" -eq 0 ]; then
        fail "$name" "exited 0 instead of failing"
    fi
    if [ "$status" -eq 124 ]; then
        fail "$name" "did not exit within 10 s"
    fi
    if [ ! -s "$log" ]; then
        fail "$name" "printed nothing"
    fi
    if ! head -n 1 "$log" | grep -q '^Http.run:'; then
        fail "$name" "first line does not start with Http.run: ($(head -n 1 "$log"))"
    fi
    echo "ok $name"
}

PORT="$(pick_port)" || fail port "no free port near $BASE_PORT"

"$YACK" build test/http_server_app/src/main.yack "$OUT/app" >/dev/null


run_case headers-small


run_case headers-large


run_case body-large


run_case headers-raised YACK_HTTP_MAX_REQUEST_BYTES=65536


run_case big-default


run_case big-limit YACK_HTTP_MAX_RESPONSE_BYTES=4096


run_case survives-431


check_invalid_config

echo "ok http_server limits"
