#!/usr/bin/env bash
















set -euo pipefail
cd "$(dirname "$0")/.."


PORT=18092
PROFILE="${PROFILE:-quick}"
SEED="${SEED:-1}"
MODE="${MODE:-heap}"
ASAN="${ASAN:-0}"
YACK="${YACK:-./yack}"
OUT="${TMPDIR:-/tmp}/yack-http-fuzz"

rm -rf "$OUT"
mkdir -p "$OUT"

port_open() {
    (exec 3<>"/dev/tcp/127.0.0.1/$PORT") 2>/dev/null
}

if port_open; then
    echo "http_fuzz: port $PORT already in use; refusing to start (not killing it)" >&2
    exit 1
fi

if [ ! -x "$YACK" ]; then
    echo "http_fuzz: compiler not found: $YACK (run make yack)" >&2
    exit 1
fi

case "$PROFILE" in
    quick|aggressive) ;;
    *) echo "http_fuzz: unknown PROFILE=$PROFILE (use quick or aggressive)" >&2; exit 2 ;;
esac

if [ "$ASAN" = "1" ]; then


    echo "http_fuzz: building ASan server"
    if ! bash -c 'ulimit -v 4000000; exec taskset -c 4-7 "$@"' _ \
         "$YACK" llvm --memory=heap --arena=noop examples/alpage/src/main.yack \
         >"$OUT/server.ll" 2>"$OUT/build.log"; then
        echo "http_fuzz: yack llvm failed" >&2
        cat "$OUT/build.log" >&2
        exit 1
    fi
    if ! bash -c 'ulimit -v 4000000; exec taskset -c 4-7 "$@"' _ \
         clang -O1 -g -fsanitize=address -Wno-override-module \
         -o "$OUT/server" "$OUT/server.ll" 2>"$OUT/build.log"; then
        echo "http_fuzz: clang ASan link failed" >&2
        cat "$OUT/build.log" >&2
        exit 1
    fi
else
    case "$MODE" in
        heap|stack) ;;
        *) echo "http_fuzz: unknown MODE=$MODE (use heap or stack)" >&2; exit 2 ;;
    esac
    if [ "$MODE" = "stack" ]; then
        if ! bash -c 'ulimit -v 4000000; exec taskset -c 4-7 "$@"' _ \
             "$YACK" build --memory=stack --arena-reserve=64M \
             examples/alpage/src/main.yack "$OUT/server" \
             >"$OUT/build.log" 2>&1; then
            echo "http_fuzz: build failed (MODE=stack)" >&2
            cat "$OUT/build.log" >&2
            exit 1
        fi
    else
        if ! bash -c 'ulimit -v 4000000; exec taskset -c 4-7 "$@"' _ \
             "$YACK" build --memory=heap examples/alpage/src/main.yack "$OUT/server" \
             >"$OUT/build.log" 2>&1; then
            echo "http_fuzz: build failed (MODE=heap)" >&2
            cat "$OUT/build.log" >&2
            exit 1
        fi
    fi
fi

pid=""
stop_server() {
    if [ -z "$pid" ]; then
        return 0
    fi
    if kill -0 "$pid" 2>/dev/null; then
        kill -TERM "$pid" 2>/dev/null || true
        for _ in $(seq 1 50); do
            kill -0 "$pid" 2>/dev/null || break
            sleep 0.1
        done
        if kill -0 "$pid" 2>/dev/null; then
            kill -KILL "$pid" 2>/dev/null || true
        fi
    fi
    wait "$pid" 2>/dev/null || true
    pid=""
}
trap stop_server EXIT

if [ "$ASAN" = "1" ]; then
    ASAN_OPTIONS="${ASAN_OPTIONS:-detect_leaks=0:abort_on_error=0}" \
        taskset -c 4-5 "$OUT/server" >"$OUT/server.log" 2>&1 &
else
    bash -c 'ulimit -v 4000000; exec taskset -c 4-5 "$@"' _ \
        "$OUT/server" >"$OUT/server.log" 2>&1 &
fi
pid=$!

ready=""
for _ in $(seq 1 200); do
    if ! kill -0 "$pid" 2>/dev/null; then
        echo "http_fuzz: server died during startup" >&2
        cat "$OUT/server.log" >&2
        wait "$pid" 2>/dev/null || true
        pid=""
        exit 1
    fi
    if port_open; then
        ready=1
        break
    fi
    sleep 0.05
done

if [ -z "$ready" ]; then
    echo "http_fuzz: server did not listen on port $PORT" >&2
    cat "$OUT/server.log" >&2
    exit 1
fi

fuzz_command=(
    node test/http_fuzz.mjs
    --port "$PORT"
    --pid "$pid"
    --seed "$SEED"
    --profile "$PROFILE"
    --json "$OUT/fuzz.json"
)
fuzz_command+=("$@")

set +e
bash -c 'ulimit -n 65535 2>/dev/null || ulimit -n 4096; ulimit -v 4000000; exec taskset -c 6-7 "$@"' _ "${fuzz_command[@]}"
status=$?
set -e

if ! kill -0 "$pid" 2>/dev/null; then
    echo "http_fuzz: server is no longer running" >&2
    if [ -s "$OUT/server.log" ]; then
        echo "http_fuzz: server stderr:" >&2
        cat "$OUT/server.log" >&2
    fi
    status=1
fi

if [ "$status" -ne 0 ]; then
    echo "http_fuzz: fuzzer exit $status (report: $OUT/fuzz.json, server log: $OUT/server.log)" >&2
    if [ -s "$OUT/server.log" ]; then
        echo "http_fuzz: server stderr (tail):" >&2
        tail -40 "$OUT/server.log" >&2
    fi
else
    echo "http_fuzz: report $OUT/fuzz.json"
fi

exit "$status"
