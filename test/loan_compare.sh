#!/usr/bin/env bash


















set -u
cd "$(dirname "$0")/.." || exit 2
YACK=${YACK:-./yack}
VERBOSE=${1:-}








declare -A EXPLAINED=(
  [test/nll/reject/return_owned_borrow]="before-graph E0503"
  [test/reject/array_borrow_return]="before-graph E0503"
  [test/reject/borrow_aggregate_return]="before-graph E0503"
  [test/reject/borrow_field_affine]="before-graph E0508"
  [test/reject/borrow_from_unknown_name]="before-graph E0505"
  [test/reject/borrow_mut_literal]="before-graph E0500"
  [test/reject/borrow_non_variable]="before-graph E0500"
  [test/reject/borrow_return_ambiguous]="before-graph E0503"
  [test/reject/borrow_return_non_borrow_param]="before-graph E0505"
  [test/reject/borrow_return_range]="before-graph E0504"
  [test/reject/borrow_return_wrong_source]="before-graph E0506"
  [test/reject/borrow_return]="before-graph E0503"
  [test/reject/borrow_temp_array]="before-graph E0500"
  [test/reject/borrow_temp_map]="before-graph E0500"
  [test/reject/lambda_borrow_return]="before-graph E0503"
  [test/reject/local_fun_shadow]="before-graph E0519"
  [test/reject/local_fun]="before-graph E0518"
  [test/reject/record_linear_not_consumed]="before-graph E0403, lowering"
  [test/reject/trap_lin_in_option]="before-graph E0403, lowering"
  [test/reject/trap_lin_in_tuple_field_read]="before-graph E0403, lowering"
  [test/reject/trap_lin_poly_drop]="before-graph E0403, lowering"
  [test/reject/borrow_field_forgotten]="before-graph E0403, lowering"
  [test/reject/record_use_after_move]="precision rename: \`val moved = user\` consumes nothing"
  [test/reject/tuple_use_after_move]="precision rename: \`val moved = pair\` consumes nothing"
  [test/reject/borrow_local_escape]="precision: the escaped borrow is never used"
  [test/nll/reject/store_borrow_container]="language E0515: the map is dropped unread"
  [test/reject/closure]="language E0511: captured local, unsupported"
)

corpus_a=$(find test examples bench hey zoo lib -name '*.yack' \
             -not -path 'test/reject/*' -not -path 'test/nll/reject/*' | sort)
corpus_b=$(ls test/reject/*.yack test/nll/reject/*.yack | sort)

a_files=0 a_rows=0
for file in $corpus_a; do
  a_files=$((a_files + 1))
  findings=$(timeout 60 "$YACK" check "$file" 2>&1 \
               | grep -E 'E090[01]')
  if [ -n "$findings" ]; then
    a_rows=$((a_rows + 1))
    echo "A  graph rejects, frontend accepts: $file"
    printf '%s\n' "$findings" | sed 's/^/     /'
  fi
done

b_files=0 b_both=0 b_explained=0 b_rows=0
for file in $corpus_b; do
  output=$(YACK_BORROW_BYPASS=1 timeout 60 "$YACK" check "$file" 2>&1)
  frontend=$(printf '%s\n' "$output" | grep -oE '^error\[E0[45][0-9]{2}\]' | sort -u | tr '\n' ' ')
  [ -n "$frontend" ] || continue
  b_files=$((b_files + 1))
  key=${file%.yack}
  if printf '%s\n' "$output" | grep -qE 'E090[01]'; then
    b_both=$((b_both + 1))
    [ -n "$VERBOSE" ] && echo "B  both reject: $file ($frontend)"
    if [ -n "${EXPLAINED[$key]:-}" ]; then
      echo "B  stale explanation, the graph rejects now: $file"
      b_rows=$((b_rows + 1))
    fi
  elif [ -n "${EXPLAINED[$key]:-}" ]; then
    b_explained=$((b_explained + 1))
    [ -n "$VERBOSE" ] && echo "B  graph silent ($frontend): $file: ${EXPLAINED[$key]}"
  else
    b_rows=$((b_rows + 1))
    echo "B  graph silent, unexplained ($frontend): $file"
  fi
done

echo
echo "A: $a_files accepted files, $a_rows rejected by the graph"
echo "B: $b_files rejected tests, $b_both rejected by the graph too," \
     "$b_explained explained, $b_rows unexplained"
[ "$a_rows" -eq 0 ] && [ "$b_rows" -eq 0 ]
