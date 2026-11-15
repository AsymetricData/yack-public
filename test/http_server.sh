#!/usr/bin/env bash




set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-http-server-test"
rm -rf "$OUT"
mkdir -p "$OUT"

PORT=18092
DURATION="${DURATION:-5}"
YACK="${YACK:-./yack}"

if curl -s -m 0.2 "http://127.0.0.1:$PORT/" >/dev/null 2>&1; then
    echo "http_server: port $PORT already in use" >&2
    exit 1
fi

cleanup() {
    if [ -n "${pid:-}" ]; then
        kill "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
    fi
    local guard
    for guard in ${guard_pids:-}; do
        kill "$guard" 2>/dev/null || true
        wait "$guard" 2>/dev/null || true
    done
}
trap cleanup EXIT

build() {
    local name=$1
    shift
    "$YACK" build "$@" test/http_server_app/src/main.yack "$OUT/$name" >/dev/null
}

wait_port() {
    local i
    for i in $(seq 1 100); do
        if curl -s -m 0.2 -o /dev/null "http://127.0.0.1:$PORT/" 2>/dev/null; then
            return 0
        fi
        sleep 0.02
    done
    echo "http_server: server did not answer" >&2
    cat "$OUT/$1.log" >&2
    return 1
}

run_slots() {
    node test/http_server_slots.mjs "$PORT"
}

endurance() {
    local name=$1 conns=$2 threads=$3
    local log="$OUT/$name-c$conns.wrk"
    local count
    if [ "$conns" = "1" ]; then
        threads=1
    fi
    if ! wrk -t"$threads" -c"$conns" -d"${DURATION}s" \
         "http://127.0.0.1:$PORT/" >"$log" 2>&1; then
        echo "http_server: wrk failed for $name c=$conns" >&2
        cat "$log" >&2
        return 1
    fi
    count=$(awk '/requests in/ { print $1 }' "$log")
    count=${count:-0}
    if ! kill -0 "$pid" 2>/dev/null; then
        echo "http_server: $name died at c=$conns after $count requests" >&2
        cat "$OUT/$name.log" >&2
        return 1
    fi
    if [ "$conns" = "1" ] && [ "$count" -lt 10000 ]; then
        echo "http_server: $name c=1 only $count requests" >&2
        return 1
    fi
    if [ "$conns" != "1" ] && [ "$count" -lt 50000 ]; then
        echo "http_server: $name c=$conns only $count requests" >&2
        return 1
    fi
    echo "ok http_server endurance $name c=$conns ($count requests)"
}

run_variant() {
    local name=$1
    fuser -k "$PORT/tcp" 2>/dev/null || true
    sleep 0.2
    "$OUT/$name" >"$OUT/$name.log" 2>&1 &
    pid=$!
    wait_port "$name"
    run_slots
    endurance "$name" 1 1
    endurance "$name" 10 2
    endurance "$name" 100 2
    kill -TERM "$pid" 2>/dev/null || true
    if ! wait "$pid"; then
        echo "http_server: $name exited non-zero after SIGTERM" >&2
        cat "$OUT/$name.log" >&2
        pid=""
        return 1
    fi
    pid=""
    sleep 0.2
}

build heap
build stack --memory=stack --arena-reserve=64M



bind_guard() {
    local name=heap
    if curl -s -m 0.2 "http://127.0.0.1:$PORT/" >/dev/null 2>&1; then
        echo "http_server: port $PORT already in use" >&2
        return 1
    fi
    "$OUT/$name" >"$OUT/$name-guard.log" 2>&1 &
    pid=$!
    wait_port "$name"

    local second_log="$OUT/$name-second.log"
    local status=0
    timeout 5 "$OUT/$name" >"$second_log" 2>&1 || status=$?
    if [ "$status" -eq 0 ]; then
        echo "http_server: second instance bound port $PORT" >&2
        cat "$second_log" >&2
        return 1
    fi
    if [ "$status" -eq 124 ]; then
        echo "http_server: second instance did not fail" >&2
        cat "$second_log" >&2
        return 1
    fi
    if ! grep -q "errno 98" "$second_log"; then
        echo "http_server: second instance did not report errno 98" >&2
        cat "$second_log" >&2
        return 1
    fi
    local code
    code=$(curl -s -m 2 -o /dev/null -w '%{http_code}' \
        "http://127.0.0.1:$PORT/")
    if [ "$code" != "200" ]; then
        echo "http_server: first instance answered '$code' after failed bind" >&2
        return 1
    fi

    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    pid=""
    sleep 0.2


    YACK_HTTP_PORT="$PORT" YACK_HTTP_REUSE_PORT=1 "$OUT/$name" \
        >"$OUT/$name-shared-a.log" 2>&1 &
    local first=$!
    guard_pids="$first"
    local i
    for i in $(seq 1 100); do
        if curl -s -m 0.2 -o /dev/null "http://127.0.0.1:$PORT/" 2>/dev/null; then
            break
        fi
        sleep 0.02
    done
    YACK_HTTP_PORT="$PORT" YACK_HTTP_REUSE_PORT=1 "$OUT/$name" \
        >"$OUT/$name-shared-b.log" 2>&1 &
    local second=$!
    guard_pids="$first $second"
    sleep 0.5
    if ! kill -0 "$first" 2>/dev/null; then
        echo "http_server: first reuse_port instance died" >&2
        cat "$OUT/$name-shared-a.log" >&2
        return 1
    fi
    if ! kill -0 "$second" 2>/dev/null; then
        echo "http_server: second reuse_port instance did not start" >&2
        cat "$OUT/$name-shared-b.log" >&2
        return 1
    fi
    code=$(curl -s -m 2 -o /dev/null -w '%{http_code}' \
        "http://127.0.0.1:$PORT/")
    if [ "$code" != "200" ]; then
        echo "http_server: shared port answered '$code'" >&2
        return 1
    fi
    kill -TERM "$first" "$second" 2>/dev/null || true
    wait "$first" 2>/dev/null || true
    wait "$second" 2>/dev/null || true
    guard_pids=""
    echo "ok http_server reuse_port"
}

run_variant heap
run_variant stack
bind_guard

echo "ok http_server"
