#!/usr/bin/env bash
















set -u
cd "$(dirname "$0")/.."

YACK=${YACK:-./yack}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
failed=0

check() {
  local name=$1 source=$2 expected=$3 memory=$4
  local base="$TMP/$name-$memory"
  local ll="$base.ll"
  if ! "$YACK" llvm --memory="$memory" "$source" > "$ll" 2>"$base.err"; then
    echo "FAIL tail_loop_$name ($memory) llvm"
    head -5 "$base.err"
    failed=1
    return
  fi
  local binary="$base.o0"
  if ! clang -O0 -Wno-override-module -o "$binary" "$ll" >/dev/null 2>&1; then
    echo "FAIL tail_loop_$name ($memory) clang"
    failed=1
    return
  fi
  local output
  output=$( (ulimit -s 8192; "$binary") 2>&1 )
  if [ "$output" != "$expected" ]; then
    echo "FAIL tail_loop_$name ($memory) run: got [$output]"
    failed=1
    return
  fi
  echo "ok   tail_loop_$name ($memory)"

  local asan="$base.asan"
  if ! clang -O1 -g -fsanitize=address -Wno-override-module \
       -o "$asan" "$ll" >/dev/null 2>&1; then
    echo "FAIL tail_loop_$name ($memory) asan clang"
    failed=1
    return
  fi
  output=$( (ulimit -s 8192; ASAN_OPTIONS=detect_leaks=1 "$asan") 2>&1 )
  if [ "$output" != "$expected" ]; then
    echo "FAIL tail_loop_$name ($memory) asan run: got [$output]"
    failed=1
    return
  fi
  echo "ok   tail_loop_$name ($memory) +asan"
}





check_shape() {
  local want=$1 name=$2 source=$3 expected=$4 memory=$5
  shift 5
  local base="$TMP/$name-$memory"
  local ll="$base.ll"
  local flags=(--memory="$memory")
  if [ "$memory" = heap ]; then flags+=(--arena=noop); fi
  if ! "$YACK" llvm "${flags[@]}" "$source" > "$ll" 2>"$base.err"; then
    echo "FAIL tail_${want}_$name ($memory) llvm"
    head -5 "$base.err"
    failed=1
    return
  fi
  local function body
  for function in "$@"; do
    body=$(awk -v f="@yack_$function(" \
      'index($0, "define ") == 1 && index($0, f) { inside = 1 }
       inside { print }
       inside && $0 == "}" { exit }' "$ll")
    if [ -z "$body" ]; then
      echo "FAIL tail_${want}_$name ($memory) no function $function"
      failed=1
      return
    fi
    if printf '%s\n' "$body" | sed 1d | grep -q "call .*@yack_$function("; then
      if [ "$want" = loop ]; then
        echo "FAIL tail_loop_$name ($memory) $function still calls itself"
        failed=1
        return
      fi
    elif [ "$want" = call ]; then
      echo "FAIL tail_call_$name ($memory) $function lost its self call"
      failed=1
      return
    fi
  done
  local binary="$base.o0"
  if ! clang -O0 -Wno-override-module -o "$binary" "$ll" >/dev/null 2>&1; then
    echo "FAIL tail_${want}_$name ($memory) clang"
    failed=1
    return
  fi
  local output
  output=$( (ulimit -s 8192; "$binary") 2>&1 )
  if [ "$output" != "$expected" ]; then
    echo "FAIL tail_${want}_$name ($memory) run: got [$output]"
    failed=1
    return
  fi
  local asan="$base.asan"
  if ! clang -O1 -g -fsanitize=address -Wno-override-module \
      -o "$asan" "$ll" >/dev/null 2>&1; then
    echo "FAIL tail_${want}_$name ($memory) asan clang"
    failed=1
    return
  fi
  output=$( (ulimit -s 8192; ASAN_OPTIONS=detect_leaks=1 "$asan") 2>&1 )
  if [ "$output" != "$expected" ]; then
    echo "FAIL tail_${want}_$name ($memory) asan run: got [$output]"
    failed=1
    return
  fi
  echo "ok   tail_${want}_$name ($memory) +asan"
}

check_call() { check_shape call "$@"; }
check_loop() { check_shape loop "$@"; }















MAX_SELF_CALLS=4
count_self_calls() {
  local file
  find examples -name '*.yack' | sort | while read -r file; do
    "$YACK" llvm --memory=heap "$file" 2>/dev/null || continue
  done | awk '
    index($0, "define ") == 1 {
      name = $0
      sub(/^define [^@]*@/, "", name); sub(/\(.*$/, "", name)
      current = name; looped = 0; called = 0; next
    }
    current == "" { next }
    index($0, "loop.stack") { looped = 1 }
    index($0, "call ") && index($0, "@" current "(") { called = 1 }
    $0 == "}" { if (looped && called) seen[current] = 1; current = "" }
    END { for (name in seen) print name }' | sort
}

for memory in heap stack; do
  check int test/tail_deep.yack 10000000 "$memory"
  check record test/tail_deep_record.yack 10000000 "$memory"
  check param test/tail_deep_param.yack 1000000 "$memory"
  check drop_list test/drop_long_list.yack 1 "$memory"
  check drop_tree test/drop_deep_tree.yack 21 "$memory"
  check borrow_walk test/tail_borrow_walk.yack 10000000 "$memory"
  check_call borrow_local test/tail_borrow_local.yack 1011 "$memory" \
    sum_local shadowed view_cell let_shadow
  check_call trap_argument test/tail_trap_argument.yack 101 "$memory" loop
  check_call trap_local test/tail_trap_local.yack 500500 "$memory" walk
  check_call trap_view test/tail_trap_view.yack 500500 "$memory" walk
  check_call trap_map test/tail_trap_map.yack 500500 "$memory" walk
  check_call trap_forms test/tail_trap_forms.yack 1501500 "$memory" \
    field_walk mut_walk shadow_walk
  check_call trap_suffix test/tail_trap_suffix.yack 500500 "$memory" walk
  check_loop trap_region test/tail_trap_region.yack 5505500 "$memory" \
    region_walk native_walk
  check_loop trap_nested test/tail_trap_nested.yack 3000001 "$memory" walk
  check_loop if_arg test/tail_if_arg.yack 10000000 "$memory" \
    build_chain length_of
  check_call trap_if test/tail_trap_if.yack 501000 "$memory" walk
  check_loop owned_let test/tail_owned_let.yack 3 "$memory" walk
  check filter_deep test/tail_filter_deep.yack 1000000 "$memory"
  check list_deep test/tail_list_deep.yack 17500004 "$memory"
  check drop_option test/drop_deep_option.yack 1 "$memory"
  check drop_option_option test/drop_deep_option_option.yack 1 "$memory"
  check drop_record test/drop_deep_record.yack 1 "$memory"
done





list_self_calls=$("$YACK" llvm --memory=heap test/tail_list_deep.yack 2>/dev/null | awk '
  index($0, "define ") == 1 {
    name = $0
    sub(/^define [^@]*@/, "", name); sub(/\(.*$/, "", name)
    current = name; called = 0; next
  }
  current == "" { next }
  current !~ /^yack__yack_list_/ { next }
  index($0, "call ") && index($0, "@" current "(") { called = 1 }
  $0 == "}" { if (called) seen[current] = 1; current = "" }
  END { for (name in seen) print name }' | sort)
count=$(printf '%s' "$list_self_calls" | grep -c .)
if [ "$count" -ne 0 ]; then
  echo "FAIL tail_list_corpus: $count yack/list functions still call themselves"
  printf '  %s\n' $list_self_calls
  failed=1
else
  echo "ok   tail_list_corpus: every yack/list traversal is a loop"
fi

self_calls=$(count_self_calls)
count=$(printf '%s' "$self_calls" | grep -c .)
if [ "$count" -gt "$MAX_SELF_CALLS" ]; then
  echo "FAIL tail_corpus: $count looping functions still call themselves" \
    "(max $MAX_SELF_CALLS)"
  printf '  %s\n' $self_calls
  failed=1
else
  echo "ok   tail_corpus: $count looping functions still call themselves" \
    "(max $MAX_SELF_CALLS)"
fi

if [ "$failed" -ne 0 ]; then
  echo "tail loop checks failed"
  exit 1
fi
echo "tail loops: no stack growth"
