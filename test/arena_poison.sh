#!/usr/bin/env bash







set -u

cd "$(dirname "$0")/.."

YACK=./yack
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
failed=0

fail() {
  printf 'FAIL %s\n' "$1"
  shift
  [ "$#" -eq 0 ] || printf '     %s\n' "$@"
  failed=1
}

pass() {
  printf 'ok   %s\n' "$1"
}

build() {
  local label=$1
  shift
  timeout 30 "$YACK" build "$@" >"$TMP/$label.build" 2>&1
}

run() {
  local label=$1
  local binary=$2


  ASAN_OPTIONS=detect_leaks=0 timeout 10 \
    bash -c '"$1" >"$2" 2>"$3"' _ "$binary" \
    "$TMP/$label.out" "$TMP/$label.err" 2>/dev/null
}

expect_poison() {
  local label=$1
  local memory=$2
  local file=$3
  local binary="$TMP/$label"

  if ! build "$label" --memory="$memory" --arena-debug=poison \
       --arena-quarantine=strict "test/poison/$file.yack" "$binary"; then
    fail "$label" "build: $(tail -n 3 "$TMP/$label.build")"
    return
  fi
  run "$label" "$binary"
  local status=$?
  if [ "$status" -eq 0 ]; then
    fail "$label" "expected a sanitizer failure"
    return
  fi
  if ! grep -q "use-after-poison" "$TMP/$label.err"; then
    fail "$label" \
      "expected use-after-poison: $(head -n 2 "$TMP/$label.err")"
    return
  fi
  pass "$label"
}

expect_ok() {
  local label=$1
  local memory=$2
  local file=$3
  shift 3
  local binary="$TMP/$label"

  if ! build "$label" "$@" --memory="$memory" \
       "test/poison/$file.yack" "$binary"; then
    fail "$label" "build: $(tail -n 3 "$TMP/$label.build")"
    return
  fi
  run "$label" "$binary"
  local status=$?
  if [ "$status" -ne 0 ]; then
    fail "$label" \
      "exit $status: $(head -n 2 "$TMP/$label.err")"
    return
  fi
  if grep -q "AddressSanitizer" "$TMP/$label.err"; then
    fail "$label" "unexpected sanitizer report"
    return
  fi
  pass "$label"
}

expect_quarantine_limit() {
  local label=$1
  local memory=$2
  local binary="$TMP/$label"

  if ! build "$label" --memory="$memory" --arena-debug=poison \
       --arena-quarantine=strict --arena-quarantine-limit=1K \
       test/poison/quarantine_limit.yack "$binary"; then
    fail "$label" "build: $(tail -n 3 "$TMP/$label.build")"
    return
  fi
  run "$label" "$binary"
  local status=$?
  if [ "$status" -eq 0 ]; then
    fail "$label" "expected the quarantine budget to fail"
    return
  fi
  if ! grep -q "arena quarantine limit exceeded" "$TMP/$label.err"; then
    fail "$label" \
      "expected the quarantine message: $(head -n 2 "$TMP/$label.err")"
    return
  fi
  pass "$label"
}

for memory in heap stack; do


  expect_ok "poison_${memory}_read_after_scope" "$memory" read_after_scope \
    --arena-debug=poison --arena-quarantine=strict
  expect_ok "poison_${memory}_read_after_allocations" "$memory" \
    read_after_allocations --arena-debug=poison --arena-quarantine=strict


  expect_ok "plain_${memory}_read_after_scope" "$memory" read_after_scope


  expect_ok "poison_${memory}_rooted_value_ok" "$memory" rooted_value_ok \
    --arena-debug=poison --arena-quarantine=strict
  expect_ok "poison_${memory}_outer_after_nested_ok" "$memory" \
    outer_after_nested_ok --arena-debug=poison --arena-quarantine=strict
  expect_ok "poison_${memory}_fresh_allocation_ok" "$memory" \
    fresh_allocation_ok --arena-debug=poison --arena-quarantine=strict

  expect_quarantine_limit "poison_${memory}_quarantine_limit" "$memory"
done

if [ "$failed" -ne 0 ]; then
  echo "arena poison checks failed"
  exit 1
fi
echo "all arena poison checks passed"
