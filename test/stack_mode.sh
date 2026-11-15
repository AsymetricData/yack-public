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




if ! cc -O2 -o "$TMP/peakrss" test/peak_rss.c >"$TMP/peakrss.log" 2>&1; then
  fail "peakrss_build" "$(tail -n 3 "$TMP/peakrss.log")"
  printf '\nstack mode checks failed\n'
  exit 1
fi


flat_rss() {
  name=$1
  source=$2
  ceiling=$3
  if ! timeout 20 "$YACK" build --memory=stack "$source" "$TMP/$name" \
       >"$TMP/$name.build.log" 2>&1; then
    fail "stack_$name" "$(tail -n 3 "$TMP/$name.build.log")"
    return
  fi
  peak=$(timeout 60 "$TMP/peakrss" "$TMP/$name" 2>"$TMP/$name.err")
  status=$?
  if [ "$status" -eq 0 ] && [ -n "$peak" ] && [ "$peak" -lt "$ceiling" ]; then
    pass "stack_$name"
  else
    fail "stack_$name" "exit $status, peak ${peak}KB (ceiling ${ceiling}KB)" \
      "$(tail -n 3 "$TMP/$name.err")"
  fi
}

for example in examples/*.yack; do
  name=$(basename "$example" .yack)

  if ! timeout 20 "$YACK" build "$example" "$TMP/heap-$name" \
       >"$TMP/heap-build.log" 2>&1; then
    fail "heap_build_$name" "$(tail -n 3 "$TMP/heap-build.log")"
    continue
  fi

  if ! timeout 20 "$YACK" build --memory=stack "$example" "$TMP/stack-$name" \
       >"$TMP/stack-build.log" 2>&1; then
    fail "stack_build_$name" "$(tail -n 3 "$TMP/stack-build.log")"
    continue
  fi

  heap_output=$(timeout 10 "$TMP/heap-$name" 2>&1)
  heap_status=$?
  stack_output=$(timeout 10 "$TMP/stack-$name" 2>&1)
  stack_status=$?

  if [ "$heap_status" -ne "$stack_status" ]; then
    fail "status_$name" "heap exit $heap_status, stack exit $stack_status"
  elif [ "$heap_output" != "$stack_output" ]; then
    fail "output_$name" "heap [$heap_output], stack [$stack_output]"
  else
    pass "run_$name"
  fi

  if ! "$YACK" llvm --memory=stack "$example" >"$TMP/$name.ll" 2>&1; then
    fail "stack_llvm_$name" "llvm failed"
  elif grep -Eq '@(malloc|calloc|realloc|free)\b' "$TMP/$name.ll"; then
    fail "stack_heap_symbols_$name" "libc heap symbol remains in stack IR"
  else
    pass "stack_ir_$name"
  fi
done


"$YACK" llvm --memory=stack examples/stack_mode.yack >"$TMP/stack_mode.ll" 2>&1
if grep -q '@yack_alloc' "$TMP/stack_mode.ll"; then
  pass "stack_mode_uses_arena"
else
  fail "stack_mode_uses_arena" "no yack_alloc call"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack_array_inline_store.yack "$TMP/inline-store" \
     >"$TMP/inline-store.log" 2>&1; then
  output=$(timeout 10 "$TMP/inline-store" 2>&1)
  if [ "$output" = "42" ]; then
    pass "stack_array_inline_store"
  else
    fail "stack_array_inline_store" "output [$output]"
  fi
else
  fail "stack_array_inline_store" "$(tail -n 3 "$TMP/inline-store.log")"
fi


if timeout 20 "$YACK" build --memory=stack \
     test/reject/stack_array_unproven_store.yack "$TMP/unproven-store" \
     >"$TMP/unproven-store.log" 2>&1; then
  fail "stack_array_unproven_store" "build unexpectedly succeeded"
elif grep -q 'E0702' "$TMP/unproven-store.log"; then
  pass "stack_array_unproven_store"
else
  fail "stack_array_unproven_store" "$(tail -n 3 "$TMP/unproven-store.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/map_inline_store.yack "$TMP/map-inline-store" \
     >"$TMP/map-inline-store.log" 2>&1; then
  output=$(timeout 10 "$TMP/map-inline-store" 2>&1)
  if [ "$output" = "42" ]; then
    pass "stack_map_inline_store"
  else
    fail "stack_map_inline_store" "output [$output]"
  fi
else
  fail "stack_map_inline_store" "$(tail -n 3 "$TMP/map-inline-store.log")"
fi



for spec in "m3_array_regions 110" "m3_map_regions 108" \
            "m3_arena_array 2" "m3_arena_map 1" "m3_map_keys 1" \
            "inter_region_deferred_write hello" \
            "inter_region_array_push_outer pushed" \
            "inter_region_map_outer mapped" \
            "inter_region_borrow_outer_ok 5" \
            "freelist_mark 1000000ZZZZZZ"; do
  set -- $spec
  name=$1
  expected=$2
  if timeout 20 "$YACK" build --memory=stack \
       "test/stack/$name.yack" "$TMP/$name" >"$TMP/$name.log" 2>&1; then
    output=$(timeout 10 "$TMP/$name" 2>&1)
    if [ "$output" = "$expected" ]; then
      pass "stack_$name"
    else
      fail "stack_$name" "output [$output]"
    fi
  else
    fail "stack_$name" "$(tail -n 3 "$TMP/$name.log")"
  fi
done



array_map=$("$YACK" alloc-map --memory=stack test/stack/m3_array_regions.yack 2>&1)
if printf '%s' "$array_map" | grep -q $'\tdestination\tyes\tarray_make' &&
   ! printf '%s' "$array_map" | grep -q $'\troot\tyes\tarray_make'; then
  pass "stack_m3_array_destination"
else
  fail "stack_m3_array_destination" "$(printf '%s' "$array_map" | tail -n 8)"
fi

arena_map=$("$YACK" alloc-map --memory=stack test/stack/m3_arena_array.yack 2>&1)
if printf '%s' "$arena_map" | grep -q 'arena.*array_new'; then
  pass "stack_m3_array_arena"
else
  fail "stack_m3_array_arena" "$(printf '%s' "$arena_map" | tail -n 8)"
fi

keys_map=$("$YACK" alloc-map --memory=stack test/stack/m3_map_keys.yack 2>&1)
if printf '%s' "$keys_map" | grep -q $'\tactivation\tyes\tmap map_keys' &&
   ! printf '%s' "$keys_map" | grep -q $'\troot\tyes\tmap map_keys'; then
  pass "stack_m3_map_keys_region"
else
  fail "stack_m3_map_keys_region" "$(printf '%s' "$keys_map" | tail -n 8)"
fi


if timeout 20 "$YACK" build --memory=stack \
     test/reject/stack_map_unproven_store.yack "$TMP/map-unproven-store" \
     >"$TMP/map-unproven-store.log" 2>&1; then
  fail "stack_map_unproven_store" "build unexpectedly succeeded"
elif grep -q 'E0702' "$TMP/map-unproven-store.log"; then
  pass "stack_map_unproven_store"
else
  fail "stack_map_unproven_store" "$(tail -n 3 "$TMP/map-unproven-store.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/scratch_container_string.yack "$TMP/scratch-container-string" \
     >"$TMP/scratch-container-string.log" 2>&1; then
  output=$(timeout 10 "$TMP/scratch-container-string" 2>&1)
  if [ "$output" = "hello world" ]; then
    pass "stack_scratch_container_string"
  else
    fail "stack_scratch_container_string" "output [$output]"
  fi
else
  fail "stack_scratch_container_string" \
    "$(tail -n 3 "$TMP/scratch-container-string.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/array_put_escape.yack "$TMP/array-put-escape" \
     >"$TMP/array-put-escape.log" 2>&1; then
  output=$(timeout 10 "$TMP/array-put-escape" 2>&1)
  if [ "$output" = "ab" ]; then
    pass "stack_array_put_escape"
  else
    fail "stack_array_put_escape" "output [$output]"
  fi
else
  fail "stack_array_put_escape" "$(tail -n 3 "$TMP/array-put-escape.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/array_put_arena_rooted.yack "$TMP/array-put-arena-rooted" \
     >"$TMP/array-put-arena-rooted.log" 2>&1; then
  output=$(timeout 10 "$TMP/array-put-arena-rooted" 2>&1)
  if [ "$output" = "ab" ]; then
    pass "stack_array_put_arena_rooted"
  else
    fail "stack_array_put_arena_rooted" "output [$output]"
  fi
else
  fail "stack_array_put_arena_rooted" \
    "$(tail -n 3 "$TMP/array-put-arena-rooted.log")"
fi

if timeout 20 "$YACK" build --memory=stack \
     test/stack/scratch_map_string.yack "$TMP/scratch-map-string" \
     >"$TMP/scratch-map-string.log" 2>&1; then
  output=$(timeout 10 "$TMP/scratch-map-string" 2>&1)
  if [ "$output" = "value1" ]; then
    pass "stack_scratch_map_string"
  else
    fail "stack_scratch_map_string" "output [$output]"
  fi
else
  fail "stack_scratch_map_string" "$(tail -n 3 "$TMP/scratch-map-string.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/scratch_call_string.yack "$TMP/scratch-call-string" \
     >"$TMP/scratch-call-string.log" 2>&1; then
  output=$(timeout 10 "$TMP/scratch-call-string" 2>&1)
  if [ "$output" = "hello" ]; then
    pass "stack_scratch_call_string"
  else
    fail "stack_scratch_call_string" "output [$output]"
  fi
else
  fail "stack_scratch_call_string" \
    "$(tail -n 3 "$TMP/scratch-call-string.log")"
fi


if timeout 20 "$YACK" build --memory=stack \
     test/stack/scratch_tuple_store.yack "$TMP/scratch-tuple-store" \
     >"$TMP/scratch-tuple-store.log" 2>&1; then
  output=$(timeout 10 "$TMP/scratch-tuple-store" 2>&1)
  if [ "$output" = "1" ] &&
     "$YACK" llvm --memory=stack test/stack/scratch_tuple_store.yack 2>/dev/null |
       grep -q 'yack_string_root'; then
    pass "stack_scratch_tuple_store"
  else
    fail "stack_scratch_tuple_store" "output [$output] or no root normalisation"
  fi
else
  fail "stack_scratch_tuple_store" \
    "$(tail -n 3 "$TMP/scratch-tuple-store.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/scratch_nested_tuple_store.yack "$TMP/scratch-nested-tuple" \
     >"$TMP/scratch-nested-tuple.log" 2>&1; then
  output=$(timeout 10 "$TMP/scratch-nested-tuple" 2>&1)
  if [ "$output" = "1" ] &&
     "$YACK" llvm --memory=stack test/stack/scratch_nested_tuple_store.yack 2>/dev/null |
       grep -q 'yack_string_root'; then
    pass "stack_scratch_nested_tuple_store"
  else
    fail "stack_scratch_nested_tuple_store" \
      "output [$output] or no root normalisation"
  fi
else
  fail "stack_scratch_nested_tuple_store" \
    "$(tail -n 3 "$TMP/scratch-nested-tuple.log")"
fi


for arena_case in arena_result_data:ab arena_result_list:2; do
  arena_file=${arena_case%%:*}
  arena_expected=${arena_case##*:}
  if timeout 20 "$YACK" build --memory=stack \
       "test/stack/$arena_file.yack" "$TMP/$arena_file" \
       >"$TMP/$arena_file.log" 2>&1; then
    output=$(timeout 10 "$TMP/$arena_file" 2>&1)
    if [ "$output" = "$arena_expected" ]; then
      pass "stack_$arena_file"
    else
      fail "stack_$arena_file" "output [$output]"
    fi
  else
    fail "stack_$arena_file" "$(tail -n 3 "$TMP/$arena_file.log")"
  fi
done



arena_record_expected=$(printf '7\nlabel\nalpha\nbeta\ngamma')
for memory_mode in heap stack; do
  for arena_mode in on noop; do
    if [ "$memory_mode" = "stack" ] && [ "$arena_mode" = "noop" ]; then
      continue
    fi
    arena_record_bin="$TMP/arena-return-record-$memory_mode-$arena_mode"
    if timeout 20 "$YACK" build --memory=$memory_mode --arena=$arena_mode \
         test/stack/arena_return_record_tuple.yack "$arena_record_bin" \
         >"$TMP/arena-return-record.log" 2>&1; then
      output=$(timeout 10 "$arena_record_bin" 2>&1)
      if [ "$output" = "$arena_record_expected" ]; then
        pass "stack_arena_return_record_tuple_${memory_mode}_${arena_mode}"
      else
        fail "stack_arena_return_record_tuple_${memory_mode}_${arena_mode}" \
          "output [$output]"
      fi
    else
      fail "stack_arena_return_record_tuple_${memory_mode}_${arena_mode}" \
        "$(tail -n 3 "$TMP/arena-return-record.log")"
    fi
  done
done




for memory_mode in heap stack; do
  for arena_mode in on noop; do
    if [ "$memory_mode" = "stack" ] && [ "$arena_mode" = "noop" ]; then
      continue
    fi
    generic_arena_bin="$TMP/generic-arena-move-$memory_mode-$arena_mode"
    if timeout 20 "$YACK" build --memory=$memory_mode --arena=$arena_mode \
         test/asan/generic_field_arena_move.yack "$generic_arena_bin" \
         >"$TMP/generic-arena-move.log" 2>&1; then
      if timeout 10 "$generic_arena_bin" >/dev/null 2>&1; then
        pass "stack_generic_field_arena_move_${memory_mode}_${arena_mode}"
      else
        fail "stack_generic_field_arena_move_${memory_mode}_${arena_mode}" \
          "run failed"
      fi
    else
      fail "stack_generic_field_arena_move_${memory_mode}_${arena_mode}" \
        "$(tail -n 3 "$TMP/generic-arena-move.log")"
    fi
  done
done




if timeout 20 "$YACK" build --memory=stack \
     examples/benchserver/src/main.yack "$TMP/benchserver-stack" \
     >"$TMP/benchserver-build.log" 2>&1; then
  if "$YACK" llvm --memory=stack examples/benchserver/src/main.yack \
       >"$TMP/benchserver.ll" 2>&1 &&
     ! grep -Eq '@(malloc|calloc|realloc|free)\b' "$TMP/benchserver.ll" &&
     grep -q '@yack_string_alloc' "$TMP/benchserver.ll"; then
    pass "stack_build_http_server"
  else
    fail "stack_build_http_server" "stack IR still uses a libc heap symbol"
  fi
else
  fail "stack_build_http_server" "$(tail -n 3 "$TMP/benchserver-build.log")"
fi



if timeout 20 "$YACK" build --memory=stack \
     test/stack/apply_consume.yack "$TMP/apply-consume" \
     >"$TMP/apply-consume.log" 2>&1; then
  output=$(timeout 10 "$TMP/apply-consume" 2>&1)
  if [ "$output" = "42" ]; then
    pass "stack_apply_consume"
  else
    fail "stack_apply_consume" "output [$output]"
  fi
else
  fail "stack_apply_consume" "$(tail -n 3 "$TMP/apply-consume.log")"
fi





if timeout 20 "$YACK" build --memory=stack \
     test/stack/tail_map.yack "$TMP/tail-map" >"$TMP/tail-map.log" 2>&1; then
  output=$(timeout 10 "$TMP/tail-map" 2>&1)
  "$YACK" llvm --memory=stack test/stack/tail_map.yack 2>/dev/null |
    awk '/^define .*@yack_loop\(/,/^}/' > "$TMP/tail-map.ll"
  if [ "$output" = "1" ] &&
     grep -q 'br label %loop' "$TMP/tail-map.ll" &&
     ! grep -q 'call .*@yack_loop(' "$TMP/tail-map.ll"; then
    pass "stack_tail_map"
  else
    fail "stack_tail_map" "output [$output] or yack_loop is not a loop"
  fi
else
  fail "stack_tail_map" "$(tail -n 3 "$TMP/tail-map.log")"
fi





flat_rss box_loop test/stack/box_loop.yack 8192
flat_rss string_loop test/stack/string_loop.yack 8192
flat_rss deep_recursion test/stack/deep_recursion.yack 32768
flat_rss tail_concat_arena test/stack/tail_concat_arena.yack 8192


if timeout 20 "$YACK" build --memory=stack \
     test/stack/let_scope.yack "$TMP/let-scope" >"$TMP/let-scope.log" 2>&1; then
  output=$(timeout 10 "$TMP/let-scope" 2>&1)
  "$YACK" llvm --memory=stack test/stack/let_scope.yack 2>/dev/null     >"$TMP/let-scope.ll"
  if [ "$output" = "1088895" ] &&
     grep -q 'call i64 @yack_region_mark' "$TMP/let-scope.ll" &&
     grep -q 'call void @yack_region_release' "$TMP/let-scope.ll"; then
    pass "stack_let_scope"
  else
    fail "stack_let_scope" "output [$output] or no mark/release pair"
  fi
else
  fail "stack_let_scope" "$(tail -n 3 "$TMP/let-scope.log")"
fi


flat_rss slot_loop test/stack/slot_loop.yack 8192
flat_rss req_loop test/stack/req_loop.yack 8192
flat_rss map_slot_loop test/stack/map_slot_loop.yack 8192


flat_rss yack_array_string test/stack/yack_array_string.yack 8192
for api in test/stack/array_api_string.yack test/stack/array_builtins_string.yack \
           test/yack_array.yack; do
  api_name=$(basename "$api" .yack)
  if timeout 20 "$YACK" build "$api" "$TMP/api-heap-$api_name" \
       >"$TMP/api-heap.log" 2>&1 &&
     timeout 20 "$YACK" build --memory=stack "$api" "$TMP/api-stack-$api_name" \
       >"$TMP/api-stack.log" 2>&1; then
    api_heap=$(timeout 10 "$TMP/api-heap-$api_name" 2>&1)
    api_stack=$(timeout 10 "$TMP/api-stack-$api_name" 2>&1)
    if [ "$api_heap" = "$api_stack" ]; then
      pass "stack_api_$api_name"
    else
      fail "stack_api_$api_name" "heap [$api_heap], stack [$api_stack]"
    fi
  else
    fail "stack_api_$api_name" "$(tail -n 3 "$TMP/api-stack.log")"
  fi
done


flat_rss freelist_growth test/stack/freelist_growth.yack 16384
flat_rss arena_loop test/stack/arena_loop.yack 8192
flat_rss datatype_slot_loop test/stack/datatype_slot_loop.yack 8192
flat_rss nonreturned_arg_loop test/stack/nonreturned_arg_loop.yack 8192
flat_rss closure_arg_loop test/stack/closure_arg_loop.yack 8192

if [ "$failed" -ne 0 ]; then
  printf '\nstack mode checks failed\n'
  exit 1
fi

printf '\nall stack mode checks passed\n'
