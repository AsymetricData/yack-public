#!/usr/bin/env bash






set -euo pipefail
cd "$(dirname "$0")/.."

YACK="${YACK:-./yack}"
OUT="${TMPDIR:-/tmp}/yack-nll-test"
rm -rf "$OUT"
mkdir -p "$OUT"

failed=0

pass() {
    printf 'ok   %s\n' "$1"
}

fail() {
    printf 'FAIL %s\n' "$1"
    shift
    [ "$#" -eq 0 ] || printf '     %s\n' "$@"
    failed=1
}

run_accept() {
    local name=$1
    local file="test/nll/accept/$name.yack"
    local expected_file="test/nll/accept/$name.out"
    local expected
    expected=$(cat "$expected_file")
    local mode binary output

    if ! "$YACK" check "$file" >"$OUT/$name.check" 2>&1; then
        fail "nll_${name}_check" "$(tail -n 3 "$OUT/$name.check")"
        return
    fi

    for mode in heap stack; do
        binary="$OUT/$name-$mode"
        if ! "$YACK" build "--memory=$mode" "$file" "$binary" \
             >"$OUT/$name-$mode.log" 2>&1; then
            fail "nll_${name}_${mode}_build" "$(tail -n 3 "$OUT/$name-$mode.log")"
            return
        fi
        output=$("$binary" 2>&1) || {
            fail "nll_${name}_${mode}_run" "$output"
            return
        }
        if [ "$output" != "$expected" ]; then
            fail "nll_${name}_${mode}_output" \
              "expected [$expected] got [$output]"
            return
        fi
    done





    binary="$OUT/$name-poison-stack"
    if ! "$YACK" build --memory=stack --arena-debug=poison \
         --arena-quarantine=strict "$file" "$binary" \
         >"$OUT/$name-poison-stack.log" 2>&1; then
        fail "nll_${name}_poison_stack_build" \
          "$(tail -n 3 "$OUT/$name-poison-stack.log")"
        return
    fi
    output=$(ASAN_OPTIONS=detect_leaks=0 "$binary" 2>"$OUT/$name-poison-stack.err") || {
        fail "nll_${name}_poison_stack_run" \
          "$(head -n 5 "$OUT/$name-poison-stack.err")"
        return
    }
    if [ "$output" != "$expected" ]; then
        fail "nll_${name}_poison_stack_output" \
          "expected [$expected] got [$output]"
        return
    fi

    binary="$OUT/$name-poison-heap"
    if ! "$YACK" build --memory=heap --arena-debug=poison "$file" "$binary" \
         >"$OUT/$name-poison-heap.log" 2>&1; then
        fail "nll_${name}_poison_heap_build" \
          "$(tail -n 3 "$OUT/$name-poison-heap.log")"
        return
    fi
    output=$(ASAN_OPTIONS=detect_leaks=1 "$binary" 2>"$OUT/$name-poison-heap.err") || {
        fail "nll_${name}_poison_heap_run" \
          "$(head -n 5 "$OUT/$name-poison-heap.err")"
        return
    }
    if [ "$output" != "$expected" ]; then
        fail "nll_${name}_poison_heap_output" \
          "expected [$expected] got [$output]"
        return
    fi
    pass "nll_$name"
}

run_xfail() {
    local name=$1
    local file="test/nll/accept/$name.yack"
    if "$YACK" check "$file" >"$OUT/$name.check" 2>&1; then
        fail "nll_${name}_xfail" \
          "accepted: flip its expectation to accept in test/nll/cases.tsv"
    else
        pass "nll_${name}_xfail"
    fi
}

run_reject() {
    local name=$1 code=$2
    local file="test/nll/reject/$name.yack"
    local output
    if output=$("$YACK" check "$file" 2>&1); then
        fail "nll_${name}" "expected rejection"
        return
    fi
    if ! printf '%s' "$output" | grep -q "\[$code\]"; then
        fail "nll_${name}" "missing $code" "$(printf '%s' "$output" | tail -n 3)"
        return
    fi
    pass "nll_$name"
}

if [ "${1:-}" = "--update-refs" ]; then
    node test/nll/ir-ref.mjs --update
    shift
fi

while IFS=$'\t' read -r kind name extra; do
    case "$kind" in
        ''|'#'*) continue ;;
        accept) run_accept "$name" ;;
        xfail) run_xfail "$name" ;;
        reject) run_reject "$name" "${extra:-}" ;;
        *) fail "nll_matrix" "unknown case kind [$kind]" ;;
    esac
done < test/nll/cases.tsv

if ! node test/nll/ir-ref.mjs; then
    failed=1
fi

if [ "$failed" -ne 0 ]; then
    echo "NLL checks failed" >&2
    exit 1
fi
echo "all NLL checks passed"
