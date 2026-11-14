#!/usr/bin/env bash




set -u
cd "$(dirname "$0")/.."

YACK=./yack
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
failed=0

check() {
  local name=$1
  local example=$2
  shift 2
  if ! "$YACK" llvm --memory=heap --arena=noop "$example" \
       > "$TMP/$name.ll" 2>/dev/null; then
    echo "FAIL llvm $name"
    failed=1
    return
  fi
  if ! clang -O1 -g -fsanitize=address -Wno-override-module \
       -o "$TMP/$name" "$TMP/$name.ll" "$@" >/dev/null 2>&1; then
    echo "FAIL clang $name"
    failed=1
    return
  fi
  if ! ASAN_OPTIONS=detect_leaks=1 "$TMP/$name" >/dev/null 2>"$TMP/$name.err"; then
    echo "FAIL asan $name"
    head -20 "$TMP/$name.err"
    failed=1
  else
    echo "ok   asan_$name"
  fi
}

for example in examples/*.yack test/asan/*.yack test/nll/accept/*.yack test/ssa/opt/*.yack test/guard/*.yack; do
  name=$(basename "$example" .yack)
  check "$name" "$example"
done



check sql_smoke bench/sql/smoke.yack -lsqlite3
check sql_open_failure test/sql/open_failure.yack -lsqlite3
check sql_open_temp_path test/sql/open_temp_path.yack -lsqlite3
check sql_pool_readers test/sql/pool_readers.yack -lsqlite3
check sql_prepare_finalize test/sql/prepare_finalize_close.yack -lsqlite3
check ssa_partial_join test/ssa/trap/partial_join.yack
check ssa_map_temp_key test/ssa/trap/map_temp_key.yack
check alpage_tutorial examples/alpage_tutorial/src/test.yack



check webapp_smoke examples/webapp/src/smoke.yack

if [ "$failed" -ne 0 ]; then
  echo "asan checks failed"
  exit 1
fi
echo "all asan checks passed"
