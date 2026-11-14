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
run_variant heap
run_variant stack

echo "ok http_server"
