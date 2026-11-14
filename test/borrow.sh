#!/usr/bin/env bash




set -euo pipefail
cd "$(dirname "$0")/.."

YACK="${YACK:-./yack}"
OUT="${TMPDIR:-/tmp}/yack-borrow-test"
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

run_positive() {
    local name=$1 expected=$2
    local file="test/borrow/$name.yack"
    local heap="$OUT/$name-heap"
    local stack="$OUT/$name-stack"

    if ! "$YACK" build "$file" "$heap" >"$OUT/$name-heap.log" 2>&1; then
        fail "borrow_${name}_heap_build" "$(tail -n 3 "$OUT/$name-heap.log")"
        return
    fi
    local output
    output=$("$heap" 2>&1) || {
        fail "borrow_${name}_heap_run" "$output"
        return
    }
    if [ "$output" != "$expected" ]; then
        fail "borrow_${name}_heap_output" "expected [$expected] got [$output]"
        return
    fi

    if ! "$YACK" build --memory=stack "$file" "$stack" >"$OUT/$name-stack.log" 2>&1; then
        fail "borrow_${name}_stack_build" "$(tail -n 3 "$OUT/$name-stack.log")"
        return
    fi
    output=$("$stack" 2>&1) || {
        fail "borrow_${name}_stack_run" "$output"
        return
    }
    if [ "$output" != "$expected" ]; then
        fail "borrow_${name}_stack_output" "expected [$expected] got [$output]"
        return
    fi

    pass "borrow_$name"
}

run_poison_heap() {
    local name=$1 expected=$2
    local file="test/borrow/$name.yack"
    local bin="$OUT/$name-poison-heap"
    if ! "$YACK" build --memory=heap --arena-debug=poison \
         "$file" "$bin" >"$OUT/$name-poison-heap.log" 2>&1; then
        fail "borrow_${name}_poison_heap_build" \
          "$(tail -n 3 "$OUT/$name-poison-heap.log")"
        return
    fi
    local output
    output=$("$bin" 2>"$OUT/$name-poison-heap.err") || {
        fail "borrow_${name}_poison_heap_run" \
          "$(tail -n 3 "$OUT/$name-poison-heap.err")"
        return
    }
    if [ "$output" != "$expected" ]; then
        fail "borrow_${name}_poison_heap_output" \
          "expected [$expected] got [$output]"
        return
    fi
    pass "borrow_${name}_poison_heap"
}

run_poison() {
    local name=$1 expected=$2
    local file="test/borrow/$name.yack"
    local bin="$OUT/$name-poison"
    if ! "$YACK" build --memory=stack --arena-debug=poison \
         --arena-quarantine=strict "$file" "$bin" \
         >"$OUT/$name-poison.log" 2>&1; then
        fail "borrow_${name}_poison_build" "$(tail -n 3 "$OUT/$name-poison.log")"
        return
    fi
    local output
    output=$("$bin" 2>"$OUT/$name-poison.err") || {
        fail "borrow_${name}_poison_run" "$(tail -n 3 "$OUT/$name-poison.err")"
        return
    }
    if [ "$output" != "$expected" ]; then
        fail "borrow_${name}_poison_output" "expected [$expected] got [$output]"
        return
    fi
    pass "borrow_${name}_poison"
}

run_negative() {
    local name=$1 code=$2
    local file="test/borrow/$name.yack"
    local output
    if output=$("$YACK" check "$file" 2>&1); then
        fail "borrow_$name" "expected rejection"
        return
    fi
    if ! printf '%s' "$output" | grep -q "\[$code\]"; then
        fail "borrow_$name" "missing $code" "$(printf '%s' "$output" | tail -n 3)"
        return
    fi
    pass "borrow_$name"
}

run_positive list_int 9
run_positive list_string 7
run_positive list_datatype 10
run_positive record_affine 42
run_positive tuple_affine 42
run_positive derived_return 5
run_positive array_read 3
run_positive map_read $'2\n1'
run_positive array_get 4
run_positive map_get 5
run_positive callback 7
run_positive string_pattern 2
run_positive array_copy_borrow 8
run_positive map_copy_borrow 8
run_positive list_empty 0
run_positive array_copy_borrow_last 7
run_positive map_missing 0
run_positive function_value 9

run_poison list_string 7
run_poison list_datatype 10
run_poison derived_return 5
run_poison_heap string_pattern 2

run_negative reject_return_local E0503
run_negative reject_store_array E0515
run_negative reject_store_map E0515
run_negative reject_escape_composite E0506
run_negative reject_mut_container E0516
run_negative reject_callback_store E0515
run_negative reject_callback_return E0503

if [ "$failed" -ne 0 ]; then
    echo "borrow checks failed" >&2
    exit 1
fi
echo "all borrow checks passed"
