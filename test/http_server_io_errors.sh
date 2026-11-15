#!/usr/bin/env bash



set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${TMPDIR:-/tmp}/yack-http-io-errors-test"
rm -rf "$OUT"
mkdir -p "$OUT"

YACK="${YACK:-./yack}"
BASE_PORT="${PORT:-18140}"
PORT=""
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

start_plain() {
    YACK_HTTP_PORT="$PORT" "$OUT/app" >"$OUT/plain.out" 2>"$OUT/plain.err" &
    pid=$!
    if ! wait_port; then
        cat "$OUT/plain.err" >&2
        fail plain "server did not listen on port $PORT"
    fi
}



start_limited() {
    ( ulimit -n 64; exec env YACK_HTTP_PORT="$PORT" "$OUT/app" ) \
        >"$OUT/limited.out" 2>"$OUT/limited.err" &
    pid=$!
    if ! wait_port; then
        cat "$OUT/limited.err" >&2
        fail limited "server did not listen on port $PORT"
    fi
}

stop_server() {
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    pid=""
    sleep 0.05
}

count_fds() {
    ls "/proc/$pid/fd" | wc -l
}

PORT="$(pick_port)" || fail port "no free port near $BASE_PORT"

"$YACK" build test/http_server_app/src/main.yack "$OUT/app" >/dev/null



start_plain
sleep 0.2
before=$(count_fds)
if ! node test/http_server_io_errors.mjs "$PORT" peers; then
    cat "$OUT/plain.err" >&2
    fail peers "scenario failed"
fi
settled=0
for _ in $(seq 1 40); do
    after=$(count_fds)
    if [ "$after" -eq "$before" ]; then
        settled=1
        break
    fi
    sleep 0.05
done
after=$(count_fds)
[ "$settled" -eq 1 ] || fail peers "fd count stayed at $after, started at $before"
if grep -q '^Http.run: ' "$OUT/plain.err"; then
    cat "$OUT/plain.err" >&2
    fail peers "peer resets were logged"
fi
stop_server
echo "ok io_errors peers"



start_limited
started=$(date +%s%N)
if ! node test/http_server_io_errors.mjs "$PORT" emfile; then
    cat "$OUT/limited.err" >&2
    fail emfile "scenario failed"
fi
stop_server
finished=$(date +%s%N)
elapsed=$(( (finished - started + 999999999) / 1000000000 ))

accept_lines=$(grep -c '^Http.run: accept failed ' "$OUT/limited.err" || true)
if [ "$accept_lines" -lt 1 ]; then
    cat "$OUT/limited.err" >&2
    fail emfile "no accept failure was reported"
fi
if ! grep -q '^Http.run: accept failed .* (last errno 24)$' "$OUT/limited.err"; then
    cat "$OUT/limited.err" >&2
    fail emfile "EMFILE was not reported as errno 24"
fi
limit=$((elapsed + 2))
if [ "$accept_lines" -gt "$limit" ]; then
    cat "$OUT/limited.err" >&2
    fail emfile "$accept_lines accept lines in ${elapsed}s, limit $limit"
fi
echo "ok io_errors emfile"

echo "ok http_server io errors"
