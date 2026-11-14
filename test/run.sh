#!/usr/bin/env bash
set -u

cd "$(dirname "$0")/.."

YACK=./yack
YACK_ABS=$PWD/yack
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

failed=0

pass() {
  printf 'ok   %s\n' "$1"
}

fail() {
  printf 'FAIL %s\n' "$1"
  failed=1
  if [ "$#" -gt 1 ]; then
    shift
    printf '     %s\n' "$@"
  fi
}

expect_check() {
  local name=$1
  local file=$2
  local output status
  output=$("$YACK" check "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "expected success, got exit $status"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_warning() {
  local name=$1
  local code=$2
  local file=$3
  local expected=${4:-}
  local output status
  output=$("$YACK" check "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "warning should not fail the check, got exit $status"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! printf '%s' "$output" | grep -q "\[$code\]"; then
    fail "$name" "missing warning $code"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if [ -n "$expected" ] && ! printf '%s' "$output" | grep -Fq "$expected"; then
    fail "$name" "missing message [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_no_warning() {
  local name=$1
  local code=$2
  local file=$3
  local output status
  output=$("$YACK" check "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "check failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if printf '%s' "$output" | grep -q "\[$code\]"; then
    fail "$name" "unexpected warning $code"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_alloc_warning() {
  local name=$1
  local code=$2
  local file=$3
  local output status
  output=$("$YACK" alloc-map --memory=stack "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "alloc-map should succeed, got exit $status"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! printf '%s' "$output" | grep -q "\[$code\]"; then
    fail "$name" "missing warning $code"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}


expect_ssa() {
  local name=$1
  local file=$2
  local output
  output=$("$YACK" ssa "$file" 2>&1)
  if [ $? -ne 0 ]; then
    fail "$name" "ssa failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! diff -u "${file%.yack}.ssa" <(printf '%s\n' "$output") > "$TMP/$name.diff"; then
    fail "$name" "ssa dump differs"
    sed 's/^/     /' "$TMP/$name.diff" | head -40
    return
  fi
  pass "$name"
}





expect_ssa_loans() {
  local name=$1
  local file=$2
  local golden=$3
  if ! "$YACK" ssa --loans "$file" > "$TMP/$name.loans" 2>&1; then
    fail "$name" "ssa --loans failed"
    sed 's/^/     /' "$TMP/$name.loans" | head -20
    return
  fi
  if ! diff -u "$golden" "$TMP/$name.loans" > "$TMP/$name.diff"; then
    fail "$name" "loan dump differs"
    sed 's/^/     /' "$TMP/$name.diff" | head -40
    return
  fi
  "$YACK" ssa "$file" > "$TMP/$name.plain" 2>&1
  if ! "$YACK" ssa-fmt "$TMP/$name.loans" > "$TMP/$name.round" 2>&1 \
     || ! diff -q "$TMP/$name.plain" "$TMP/$name.round" > /dev/null; then
    fail "$name" "ssa-fmt does not read the loan dump back to the plain dump"
    return
  fi
  pass "$name"
}




expect_ssa_roundtrip() {
  local name=$1
  local file=$2
  local status
  "$YACK" ssa "$file" > "$TMP/$name.dump" 2>&1
  status=$?
  if [ $status -ne 0 ]; then
    fail "$name" "ssa failed"
    sed 's/^/     /' "$TMP/$name.dump"
    return
  fi
  if ! "$YACK" ssa-fmt "$TMP/$name.dump" > "$TMP/$name.round" 2>&1; then
    fail "$name" "ssa-fmt rejected a dump it wrote"
    sed 's/^/     /' "$TMP/$name.round" | head -20
    return
  fi
  if ! diff -u "$TMP/$name.dump" "$TMP/$name.round" > "$TMP/$name.rt.diff"; then
    fail "$name" "round-trip differs"
    sed 's/^/     /' "$TMP/$name.rt.diff" | head -40
    return
  fi


  if ! "$YACK" ssa-check "$TMP/$name.dump" > "$TMP/$name.check" 2>&1; then
    fail "$name" "ssa-check rejected a dump yack accepted"
    sed 's/^/     /' "$TMP/$name.check" | head -20
    return
  fi
  pass "$name"
}





expect_ssa_reject() {
  local file=$1
  local name
  name=ssa_reject_$(basename "$file" .ssa)
  local expected output
  expected=$(sed -n '1s/^# expect: //p' "$file")
  if [ -z "$expected" ]; then
    fail "$name" "first line is not '# expect: <substring>'"
    return
  fi
  output=$("$YACK" ssa-check "$file" 2>&1)
  if [ $? -eq 0 ]; then
    fail "$name" "expected rejection"
    return
  fi
  if ! printf '%s' "$output" | grep -Fq "$expected"; then
    fail "$name" "missing finding [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  local control=${file%.ssa}.ok.ssa
  if [ -f "$control" ]; then
    if ! "$YACK" ssa-check "$control" > "$TMP/$name.ok" 2>&1; then
      fail "$name" "control ${control##*/} was rejected"
      sed 's/^/     /' "$TMP/$name.ok"
      return
    fi
  fi
  pass "$name"
}



expect_arena() {
  local name=$1
  local file=$2
  local memory=$3
  local output
  output=$("$YACK" arena --memory="$memory" "$file" 2>/dev/null)
  if [ $? -ne 0 ]; then
    fail "$name" "arena report failed"
    return
  fi
  if ! diff -u "${file%.yack}.$memory.out" <(printf '%s\n' "$output") > "$TMP/$name.diff"; then
    fail "$name" "arena report differs"
    sed 's/^/     /' "$TMP/$name.diff" | head -40
    return
  fi
  pass "$name"
}

expect_error() {
  local name=$1
  local code=$2
  local file=$3
  local expected=${4:-}
  local output status
  output=$("$YACK" check "$file" 2>&1)
  status=$?
  if [ "$status" -eq 0 ]; then
    fail "$name" "expected rejection"
    return
  fi
  if ! printf '%s' "$output" | grep -q "\[$code\]"; then
    fail "$name" "missing $code"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if [ -n "$expected" ] && ! printf '%s' "$output" | grep -Fq "$expected"; then
    fail "$name" "missing message [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_resolve() {
  local name=$1
  local file=$2
  local expected=$3
  local output status
  output=$("$YACK" resolve "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "resolve failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! printf '%s' "$output" | grep -q "$expected"; then
    fail "$name" "missing [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_types() {
  local name=$1
  local file=$2
  local expected=$3
  local output status
  output=$("$YACK" types "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "types failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! printf '%s' "$output" | grep -Fq "$expected"; then
    fail "$name" "missing [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}


expect_build_error() {
  local name=$1
  local code=$2
  local file=$3
  local output
  if output=$("$YACK" build "$file" "$TMP/$name" 2>&1); then
    fail "$name" "expected build rejection"
    return
  fi
  if ! printf '%s' "$output" | grep -q "\[$code\]"; then
    fail "$name" "missing $code"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_build() {
  local name=$1
  local file=$2
  local output
  if output=$("$YACK" build "$file" "$TMP/$name" 2>&1); then
    pass "$name"
  else
    fail "$name" "compile failed"
    printf '%s\n' "$output" | sed 's/^/     /'
  fi
}

expect_run() {
  local name=$1
  local file=$2
  local expected=$3
  local output status actual
  output=$("$YACK" build "$file" "$TMP/$name" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "compile failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  actual=$("$TMP/$name" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "program exited with $status"
    printf '%s\n' "$actual" | sed 's/^/     /'
    return
  fi
  if [ "$actual" != "$expected" ]; then
    fail "$name" "unexpected output" "expected [$expected]" "got [$actual]"
    return
  fi
  pass "$name"
}


expect_run_stack() {
  local name=$1
  local file=$2
  local expected=$3
  local output actual
  if ! output=$("$YACK" build --memory=stack "$file" "$TMP/$name" 2>&1); then
    fail "$name" "compile failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! actual=$("$TMP/$name" 2>&1); then
    fail "$name" "program failed"
    return
  fi
  if [ "$actual" != "$expected" ]; then
    fail "$name" "unexpected output" "expected [$expected]" "got [$actual]"
    return
  fi
  pass "$name"
}



expect_abort() {
  local name=$1
  local file=$2
  local message=$3
  local memory output status
  for memory in heap stack; do
    if ! output=$("$YACK" build --memory=$memory "$file" "$TMP/$name" 2>&1); then
      fail "$name ($memory)" "compile failed"
      printf '%s\n' "$output" | sed 's/^/     /'
      return
    fi
    output=$("$TMP/$name" 2>&1 >/dev/null)
    status=$?
    if [ "$status" -eq 0 ]; then
      fail "$name ($memory)" "program exited with 0, expected an abort"
      return
    fi
    case "$output" in
      *"$message"*) ;;
      *)
        fail "$name ($memory)" "missing message" "expected [$message]" "got [$output]"
        return
        ;;
    esac
  done
  pass "$name"
}



expect_abort_under_vmem() {
  local name=$1
  local file=$2
  local limit=$3
  local message=$4
  local output status
  if ! output=$("$YACK" build "$file" "$TMP/$name" 2>&1); then
    fail "$name" "compile failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  output=$(bash -c "ulimit -v $limit; exec '$TMP/$name'" 2>&1 >/dev/null)
  status=$?
  if [ "$status" -eq 0 ]; then
    fail "$name" "program exited with 0, expected an abort"
    return
  fi
  case "$output" in
    *"$message"*) ;;
    *)
      fail "$name" "missing message" "expected [$message]" "got [$output]"
      return
      ;;
  esac
  pass "$name"
}


expect_ir_line() {
  local name=$1
  local file=$2
  local function=$3
  local pattern=$4
  local line
  line=$("$YACK" ir "$file" 2>&1 | grep "^fun yack_$function ")
  if printf '%s' "$line" | grep -Eq -- "$pattern"; then
    pass "$name"
  else
    fail "$name" "IR shape" "pattern [$pattern]" "got [$line]"
  fi
}


expect_llvm_absent() {
  local name=$1
  local file=$2
  local pattern=$3
  local output
  if ! output=$("$YACK" llvm "$file" 2>&1); then
    fail "$name" "llvm failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if printf '%s' "$output" | grep -Eq -- "$pattern"; then
    fail "$name" "LLVM shape" "must not match [$pattern]"
  else
    pass "$name"
  fi
}

expect_no_leak() {
  local name=$1
  local file=$2
  local map=$TMP/$name.map
  local trace=$TMP/$name.trace
  local report=$TMP/$name.report
  local output status
  if ! "$YACK" alloc-map --json "$file" >"$map" 2>"$TMP/$name.maperr"; then
    fail "$name" "alloc-map failed"
    return
  fi
  if ! "$YACK" build --trace-alloc "$file" "$TMP/$name" \
       >"$TMP/$name.build" 2>&1; then
    fail "$name" "trace build failed"
    printf '%s\n' "$(tail -n 3 "$TMP/$name.build")" | sed 's/^/     /'
    return
  fi
  "$TMP/$name" >"$TMP/$name.out" 2>"$trace"
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "program exited with $status"
    return
  fi
  if ./tools/alloc-report.py --map "$map" "$trace" >"$report" 2>&1; then
    pass "$name"
  else
    fail "$name" "live allocations remain"
    sed -n '1,12p' "$report" | sed 's/^/     /'
  fi
}

expect_ir() {
  local name=$1
  local file=$2
  local pattern=$3
  local output
  if ! output=$("$YACK" llvm "$file" 2>&1); then
    fail "$name" "llvm failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if printf '%s' "$output" | grep -Fq "$pattern"; then
    pass "$name"
  else
    fail "$name" "missing IR pattern [$pattern]"
  fi
}

expect_stack() {
  local name=$1
  local file=$2
  local output
  if ! output=$("$YACK" llvm "$file" 2>&1); then
    fail "$name" "llvm failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if printf '%s' "$output" | grep -q 'alloca %yack_data'; then
    pass "$name"
  else
    fail "$name" "expected stack allocation"
  fi
}

expect_parse() {
  local name=$1
  local file=$2
  local expected=$3
  local output status
  output=$("$YACK" parse "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "parse failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! printf '%s' "$output" | grep -q "$expected"; then
    fail "$name" "missing [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_tokens() {
  local name=$1
  local file=$2
  local expected=$3
  local output status
  output=$("$YACK" tokens "$file" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$name" "tokenization failed"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  if ! printf '%s' "$output" | grep -q "$expected"; then
    fail "$name" "missing [$expected]"
    printf '%s\n' "$output" | sed 's/^/     /'
    return
  fi
  pass "$name"
}

expect_fmt() {
  local name=$1
  local file=$2
  local first="$TMP/fmt_$name.1"
  local second="$TMP/fmt_$name.2"
  if ! "$YACK" fmt "$file" > "$first" 2> "$TMP/fmt_$name.err"; then
    fail "$name" "fmt failed"
    sed 's/^/     /' "$TMP/fmt_$name.err"
    return
  fi
  if ! "$YACK" fmt "$first" > "$second" 2> "$TMP/fmt_$name.err"; then
    fail "$name" "second fmt failed"
    sed 's/^/     /' "$TMP/fmt_$name.err"
    return
  fi
  if ! cmp -s "$first" "$second"; then
    fail "$name" "not idempotent"
    diff -u "$first" "$second" | head -20 | sed 's/^/     /'
    return
  fi
  if ! "$YACK" check "$first" >/dev/null 2>&1; then
    fail "$name" "formatted output does not type-check"
    return
  fi
  if ! "$YACK" fmt - < "$file" 2>/dev/null | cmp -s - "$first"; then
    fail "$name" "stdin and file outputs differ"
    return
  fi
  if ! "$YACK" fmt --check "$first" >/dev/null 2>&1; then
    fail "$name" "--check rejects formatted output"
    return
  fi
  pass "$name"
}

expect_fmt_check_unformatted() {
  local name=$1
  local file=$2
  if "$YACK" fmt --check "$file" >/dev/null 2>&1; then
    fail "$name" "expected unformatted input"
  else
    pass "$name"
  fi
}

expect_fmt_comments() {
  local name=$1
  local file="$TMP/fmt_comments.yack"
  local output="$TMP/fmt_comments.out"
  cat > "$file" <<'EOF'
(* top *)
fun f (x : int) : int = (* body *)
    x + 1

(* between *)
fun main () =
    let val a = 1 in
        print_int (f (a) (* call *))
    end
EOF
  if ! "$YACK" fmt "$file" > "$output" 2> "$TMP/fmt_comments.err"; then
    fail "$name" "fmt failed"
    sed 's/^/     /' "$TMP/fmt_comments.err"
    return
  fi
  for marker in '(* top *)' '(* body *)' '(* between *)' '(* call *)'; do
    if ! grep -Fq "$marker" "$output"; then
      fail "$name" "lost comment $marker"
      return
    fi
  done
  if ! "$YACK" check "$output" >/dev/null 2>&1; then
    fail "$name" "commented output does not type-check"
    return
  fi
  pass "$name"
}

expect_fmt_write() {
  local name=$1
  local file="$TMP/fmt_write.yack"
  cp examples/fact.yack "$file"
  if ! "$YACK" fmt --write "$file" >/dev/null 2>&1; then
    fail "$name" "fmt --write failed"
    return
  fi
  if ! "$YACK" fmt --check "$file" >/dev/null 2>&1; then
    fail "$name" "written file is not formatted"
    return
  fi
  pass "$name"
}

expect_fmt_imports() {
  local name=$1
  local file="$TMP/fmt_imports.yack"
  local output="$TMP/fmt_imports.out"
  cat > "$file" <<'EOF'
import yack/os

import yack/option

fun main () =
    print_string (os.cwd)
EOF
  if ! "$YACK" fmt "$file" > "$output" 2> "$TMP/fmt_imports.err"; then
    fail "$name" "fmt failed"
    sed 's/^/     /' "$TMP/fmt_imports.err"
    return
  fi
  cat > "$TMP/fmt_imports.expected" <<'EOF'
import yack/os
import yack/option

fun main () = print_string (os.cwd)
EOF
  if ! cmp -s "$output" "$TMP/fmt_imports.expected"; then
    fail "$name" "imports are not grouped"
    diff -u "$TMP/fmt_imports.expected" "$output" | sed 's/^/     /'
    return
  fi
  if ! "$YACK" fmt --check "$output" >/dev/null 2>&1; then
    fail "$name" "--check rejects formatted output"
    return
  fi
  pass "$name"
}

expect_fmt_clauses() {
  local name=$1
  local file="$TMP/fmt_clauses.yack"
  local output="$TMP/fmt_clauses.out"
  cat > "$file" <<'EOF'
fun classify (0) = "zero"

fun classify (1) = "one"

fun classify (n) = "many"

fun main () =
    print_string (classify (1))
EOF
  if ! "$YACK" fmt "$file" > "$output" 2> "$TMP/fmt_clauses.err"; then
    fail "$name" "fmt failed"
    sed 's/^/     /' "$TMP/fmt_clauses.err"
    return
  fi
  cat > "$TMP/fmt_clauses.expected" <<'EOF'
fun classify (0) = "zero"
fun classify (1) = "one"
fun classify (n) = "many"

fun main () = print_string (classify (1))
EOF
  if ! cmp -s "$output" "$TMP/fmt_clauses.expected"; then
    fail "$name" "equations of one function are not glued"
    diff -u "$TMP/fmt_clauses.expected" "$output" | sed 's/^/     /'
    return
  fi
  if ! "$YACK" fmt --check "$output" >/dev/null 2>&1; then
    fail "$name" "--check rejects formatted output"
    return
  fi
  pass "$name"
}

expect_fmt_best_effort() {
  local name=$1
  local file="$TMP/fmt_best_effort.yack"
  local output="$TMP/fmt_best_effort.out"
  printf 'fun say_hello name = "Hello " ^ name\n' > "$file"
  if ! "$YACK" fmt --best-effort - < "$file" > "$output" 2> "$TMP/fmt_best_effort.err"; then
    fail "$name" "best-effort rejected invalid input"
    return
  fi
  if [ -s "$TMP/fmt_best_effort.err" ]; then
    fail "$name" "best-effort wrote to stderr"
    return
  fi
  if ! cmp -s "$file" "$output"; then
    fail "$name" "best-effort changed invalid input"
    return
  fi
  pass "$name"
}

expect_fmt_pipe_lines() {
  local name=$1
  local file=$2
  local expected=$3
  local output actual
  if ! output=$("$YACK" fmt "$file" 2>&1); then
    fail "$name" "fmt failed"
    return
  fi
  actual=$(printf '%s\n' "$output" | grep -cE '^[[:space:]]*\|>')
  if [ "$actual" -eq "$expected" ]; then
    pass "$name"
  else
    fail "$name" "expected $expected pipe lines, got $actual"
    printf '%s\n' "$output" | sed 's/^/     /'
  fi
}

expect_check hello examples/hello.yack
expect_check add examples/add.yack
expect_check fact examples/fact.yack
expect_check mutual examples/mutual.yack
expect_check let examples/let.yack
expect_check let_destructure examples/let_destructure.yack
expect_check local_fun examples/local_fun.yack
expect_run local_fun_run examples/local_fun.yack 89
expect_check local_fun_value examples/local_fun_value.yack
expect_run local_fun_value_run examples/local_fun_value.yack 2
expect_check case_guard examples/case_guard.yack
expect_check guard examples/guard.yack
expect_run guard examples/guard.yack $'41\n0\n7\n0\n30\n0\n7\nbad!'
expect_check pipe examples/pipe.yack
expect_check arith examples/arith.yack
expect_check token_consume examples/token_consume.yack
expect_check token_move examples/token_move.yack
expect_check token_branch examples/token_branch.yack
expect_check token_identity examples/token_identity.yack
expect_check resource_drop examples/resource_drop.yack
expect_check resource_move examples/resource_move.yack
expect_check type_user examples/type_user.yack
expect_check type_properties examples/type_properties.yack
expect_check type_alias examples/type_alias.yack
expect_run type_alias examples/type_alias.yack 8
expect_check type_alias_property test/type_alias_property.yack
expect_check int_widths examples/int_widths.yack
expect_check record_user examples/record_user.yack
expect_check record_structural examples/record_structural.yack
expect_check record_copy examples/record_copy.yack
expect_check tuple examples/tuple.yack
expect_check tuple_annot examples/tuple_annot.yack
expect_check datatype_option examples/datatype_option.yack
expect_check datatype_list examples/datatype_list.yack
expect_check list_literal examples/list_literal.yack
expect_run list_literal examples/list_literal.yack 6
expect_check list_borrow_map examples/list_borrow_map.yack
expect_run codegen_list_borrow_map examples/list_borrow_map.yack "20"
expect_check datatype_bool examples/datatype_bool.yack
expect_check borrow_shared examples/borrow_shared.yack
expect_check borrow_mut examples/borrow_mut.yack
expect_check borrow_param examples/borrow_param.yack
expect_check borrow_amp test/borrow_amp.yack
expect_run borrow_literal test/borrow_literal.yack 50
expect_check generic_id examples/generic_id.yack
expect_check generic_id_annot examples/generic_id_annot.yack
expect_check generic_same examples/generic_same.yack
expect_check generic_same_annot examples/generic_same_annot.yack
expect_check generic_add_numeric examples/generic_add_numeric.yack
expect_check float_mixed examples/float_mixed.yack
expect_check record_pattern examples/record_pattern.yack
expect_check recursive_record examples/recursive_record.yack
expect_check float_annot examples/float_annot.yack
expect_check float_widths examples/float_widths.yack
expect_check generic_option examples/generic_option.yack
expect_check generic_list examples/generic_list.yack
expect_check generic_identity examples/generic_identity.yack
expect_check generic_pair examples/generic_pair.yack
expect_types generic_option_types examples/generic_option.yack "datatype 'a option"
expect_check nested_bool_option examples/nested_bool_option.yack
expect_check nested_tuple examples/nested_tuple.yack
expect_check nested_option examples/nested_option.yack
expect_check borrow_return examples/borrow_return.yack
expect_check borrow_return_inferred examples/borrow_return_inferred.yack
expect_check borrow_field examples/borrow_field.yack
expect_check borrow_mut_field examples/borrow_mut_field.yack
expect_check borrow_map_field examples/borrow_map_field.yack
expect_check conversions examples/conversions.yack
expect_check conversions_widths examples/conversions_widths.yack
expect_check map_keys examples/map_keys.yack
expect_check map_keys_edges examples/map_keys_edges.yack
expect_check prelude_helpers examples/prelude_helpers.yack
expect_check module_qualified examples/module_qualified.yack
expect_check module_selective examples/module_selective.yack
expect_check module_alias examples/module_alias.yack
expect_check module_import_prelude examples/module_import_prelude.yack
expect_check module_quad examples/module_quad.yack
expect_check module_quad_selective examples/module_quad_selective.yack
expect_check module_rename examples/module_rename.yack
expect_check module_type_alias examples/module_type_alias.yack
expect_check module_alias_selective examples/module_alias_selective.yack
expect_check module_discard_qualifier examples/module_discard_qualifier.yack
expect_check nested_field_move examples/nested_field_move.yack
expect_check branch_partial_move examples/branch_partial_move.yack
expect_check lambda_consume_drop examples/lambda_consume_drop.yack
expect_check lambda_local_drop examples/lambda_local_drop.yack
expect_warning unused_function W3001 test/unused_function.yack "never used"
expect_error borrow_literal_hint E3004 test/borrow_literal_hint.yack 'a literal works too: `borrow "text"`'
expect_error thread_spawn_string E3004 test/reject/thread_spawn_string.yack 'expected: int64'
expect_error thread_spawn_record E3004 test/reject/thread_spawn_record.yack 'expected: int64'
expect_error thread_spawn_borrow E3004 test/reject/thread_spawn_borrow.yack 'expected: int64'
expect_error thread_spawn_worker_type E3004 test/reject/thread_spawn_worker_type.yack 'expected: (int64) -> unit'
expect_error mut_borrow_read_field E0502 test/reject/mut_borrow_read_field.yack 'cannot read `c.value` while a mutable borrow is active'
expect_error callback_borrowed_binding E0514 test/reject/callback_borrowed_binding.yack 'borrows its parameter, but this callback consumes it'
expect_error callback_borrowed_call E0517 test/reject/callback_borrowed_call.yack 'its parameter is borrowed'
expect_run callback_owned test/callback_owned.yack "ab"
expect_no_leak callback_owned_leak test/callback_owned.yack
expect_run callback_borrowed_unused test/callback_borrowed_unused.yack ""
expect_check mut_borrow_after_use test/mut_borrow_after_use.yack
expect_check mut_borrow_copy_owner test/mut_borrow_copy_owner.yack


expect_graph_note() {
  local name=$1
  local code=$2
  local file=$3
  local line=$4
  local note=$5
  local output
  output=$(YACK_BORROW_BYPASS=1 "$YACK" check "$file" 2>&1)
  if ! printf '%s' "$output" | grep -q "\[$code\]"; then
    fail "$name" "missing $code"
    return
  fi
  if ! printf '%s' "$output" | grep -Fq -- "--> $file:$line"; then
    fail "$name" "finding does not point to $file:$line"
    printf '%s\n' "$output" | sed 's/^/     /' | head -20
    return
  fi
  if [ -n "$note" ] && ! printf '%s' "$output" | grep -Fq -- "the other side is at $note"; then
    fail "$name" "missing note at $note"
    return
  fi
  pass "$name"
}

expect_graph_note arm_move_graph_line E0901 test/reject/arm_move_used_after.yack 10:33 9:24
expect_error alpage_tutorial_request_twice E3004 examples/alpage_tutorial/traps/request_twice.yack 'expected: borrow Request.t'
expect_warning container_readonly W3102 test/container_warnings/readonly_param.yack "only read"
expect_warning container_copy_hot W3101 test/container_warnings/copy_hot.yack "copies an owned list"
expect_no_warning container_borrow_ok_w3102 W3102 test/container_warnings/borrow_ok.yack
expect_no_warning container_borrow_ok_w3101 W3101 test/container_warnings/borrow_ok.yack
expect_no_warning container_copy_startup W3101 test/container_warnings/copy_startup.yack
expect_alloc_warning container_root_site W3103 test/container_warnings/root_site.yack
expect_no_warning stdlib_functions W3001 examples/stdlib/src/main.yack
expect_run unused_function_runs test/unused_function.yack 1

expect_parse parse-hello examples/hello.yack "fun main/0"
expect_resolve resolve-hello examples/hello.yack "function main"
expect_types type_user examples/type_user.yack "type user : Affine -> Affine"
expect_types type_point examples/type_properties.yack "type point : Copy -> Copy"
expect_types type_linear examples/type_properties.yack "type handle : Linear -> Linear"
expect_types type_default examples/type_properties.yack "type default_affine : Affine -> Affine"
expect_types datatype_list examples/datatype_list.yack "datatype 'a list : Affine -> Affine"
expect_tokens tokens-hello examples/hello.yack "\`fun\`"
expect_tokens tokens-guard examples/guard.yack "\`<-\`"

expect_fmt fmt-fact examples/fact.yack
expect_fmt fmt-arith examples/arith.yack
expect_fmt fmt-case examples/case_literals.yack
expect_fmt fmt-let-destructure examples/let_destructure.yack
expect_fmt fmt-case-guard examples/case_guard.yack
expect_fmt fmt-guard examples/guard.yack
expect_fmt fmt-pipe examples/pipe.yack
expect_fmt_pipe_lines fmt-pipe-lines examples/pipe.yack 2
expect_fmt fmt-list examples/list_prelude.yack
expect_fmt fmt-datatype-common-field examples/datatype_common_field.yack
expect_fmt fmt-list-literal examples/list_literal.yack
expect_fmt fmt-pattern-string examples/pattern_string.yack
expect_fmt fmt-pattern-bytes examples/pattern_bytes.yack
expect_fmt fmt-pattern-list-cons examples/pattern_list_cons.yack
expect_fmt fmt-types examples/generic_pair.yack
expect_fmt fmt-type-alias examples/type_alias.yack
expect_fmt fmt-consume-bang test/consume_bang.yack
expect_fmt_check_unformatted fmt-check examples/map_int.yack
expect_fmt_comments fmt-comments
expect_fmt_imports fmt-imports
expect_fmt_clauses fmt-clauses
expect_fmt_write fmt-write
expect_fmt_best_effort fmt-best-effort

expect_error closure_capture E0511 test/reject/closure.yack
expect_error local_fun_escape E0518 test/reject/local_fun.yack
expect_error local_fun_shadow E0519 test/reject/local_fun_shadow.yack
expect_error partial E1003 test/reject/partial.yack
expect_error type_mismatch E3004 test/reject/type_mismatch.yack
expect_error guard_no_else E2002 test/reject/guard_no_else.yack
expect_error guard_fallback_type E3004 test/reject/guard_fallback_type.yack
expect_error arity E3003 test/reject/arity.yack
expect_error unknown_var E3001 test/reject/unknown_var.yack
expect_error type_alias_arity E3029 test/reject/type_alias_arity.yack
expect_error type_alias_unknown E3012 test/reject/type_alias_unknown.yack
expect_error type_alias_duplicate E3013 test/reject/type_alias_duplicate.yack
expect_error type_alias_property E2002 test/reject/type_alias_property.yack
expect_error duplicate_param E3008 test/reject/duplicate_param.yack
expect_error duplicate_function E3009 test/reject/duplicate_function.yack
expect_error builtin_redefine E3011 test/reject/builtin_redefine.yack
expect_error no_main E3010 test/reject/no_main.yack
expect_error main_params E3010 test/reject/main_params.yack
expect_error main_returns_int E3010 test/reject/main_returns_int.yack
expect_error lexical_error E2001 test/reject/lexical_error.yack
expect_error integer_literal_range E2002 test/reject/integer_literal_range.yack
expect_error syntax_error E2002 test/reject/syntax_error.yack
expect_error use_after_move E0402 test/reject/use_after_move.yack
expect_error unconsumed_token E0403 test/reject/unconsumed_token.yack
expect_error token_branch_mismatch E0403 test/reject/token_branch_mismatch.yack
expect_error token_param_forgot E0403 test/reject/token_param_forgot.yack
expect_error resource_use_after_move E0402 test/reject/resource_use_after_move.yack
expect_error resource_equality E3004 test/reject/resource_equality.yack
expect_error type_copy_affine_field E3015 test/reject/type_copy_affine_field.yack
expect_error type_affine_linear_field E3016 test/reject/type_affine_linear_field.yack
expect_error type_unknown_field E3012 test/reject/type_unknown_field.yack
expect_error type_duplicate E3013 test/reject/type_duplicate.yack
expect_error record_missing_field E3004 test/reject/record_missing_field.yack
expect_error record_unknown_field E3014 test/reject/record_unknown_field.yack
expect_error datatype_common_field_partial E3014 test/reject/datatype_common_field_partial.yack "common to every constructor"
expect_error record_duplicate_field E3018 test/reject/record_duplicate_field.yack
expect_error record_use_after_move E0402 test/reject/record_use_after_move.yack
expect_error record_linear_not_consumed E0403 test/reject/record_linear_not_consumed.yack
expect_error int_bool_mismatch E3004 test/reject/int_bool_mismatch.yack
expect_error tuple_index_out_of_bounds E3019 test/reject/tuple_index_out_of_bounds.yack
expect_error tuple_index_non_tuple E3019 test/reject/tuple_index_non_tuple.yack
expect_error tuple_use_after_move E0402 test/reject/tuple_use_after_move.yack
expect_error tuple_argument_mismatch E3004 test/reject/tuple_argument_mismatch.yack
expect_error case_non_exhaustive E3021 test/reject/case_non_exhaustive.yack
expect_error let_refutable E3021 test/reject/let_refutable.yack
expect_error case_guard_type E3004 test/reject/case_guard_type.yack
expect_error case_guard_exhaustive E3021 test/reject/case_guard_exhaustive.yack
expect_error pipe_rhs E2002 test/reject/pipe_rhs.yack
expect_error lambda_borrowed_move E0512 test/reject/lambda_borrowed_move.yack

expect_error trap_affine_borrow_after_move E0402 test/reject/trap_affine_borrow_after_move.yack
expect_error trap_concat_borrow_operand E3004 test/reject/trap_concat_borrow_operand.yack
expect_error trap_concat_twice E0402 test/reject/trap_concat_twice.yack
expect_error trap_copy_generic_param E3015 test/reject/trap_copy_generic_param.yack
expect_error trap_lin_arena E0403 test/reject/trap_lin_arena.yack
expect_error trap_lin_discard_val E0403 test/reject/trap_lin_discard_val.yack
expect_error trap_lin_guard_branch E0403 test/reject/trap_lin_guard_branch.yack
expect_error trap_guard_lin_fallback E0403 test/reject/trap_guard_lin_fallback.yack
expect_error trap_guard_lin_guarded E0403 test/reject/trap_guard_lin_guarded.yack
GUARD_OUT=$'hi!\nnone\n1\n0\npqr\nno-b\nno-a'
expect_run guard_owned_heap test/guard/guard_owned.yack "$GUARD_OUT"
expect_run_stack guard_owned_stack test/guard/guard_owned.yack "$GUARD_OUT"
expect_error trap_lin_in_option E0403 test/reject/trap_lin_in_option.yack
expect_error trap_lin_in_tuple_field_read E0403 test/reject/trap_lin_in_tuple_field_read.yack
expect_error trap_lin_poly_drop E0403 test/reject/trap_lin_poly_drop.yack
expect_error trap_lin_poly_dup E0402 test/reject/trap_lin_poly_dup.yack
expect_error trap_lin_return_dropped_call E0403 test/reject/trap_lin_return_dropped_call.yack
expect_error trap_lin_seq_discard E0403 test/reject/trap_lin_seq_discard.yack
expect_error trap_lin_shadow E0403 test/reject/trap_lin_shadow.yack
expect_error trap_lin_twice E0402 test/reject/trap_lin_twice.yack
expect_error trap_lin_literal_twice E0402 test/reject/trap_lin_literal_twice.yack


expect_run borrow_arm_move test/borrow/arm_move.yack "31"
expect_run_stack borrow_arm_move_stack test/borrow/arm_move.yack "31"
expect_no_leak borrow_arm_move_leak test/borrow/arm_move.yack



expect_run borrow_elem_array_data test/borrow_elem_array_data.yack "42"
expect_run_stack borrow_elem_array_data_stack test/borrow_elem_array_data.yack "42"
expect_llvm_absent borrow_elem_array_data_slot test/borrow_elem_array_data.yack 'call i32 @yack_value_of\(ptr %slot'
expect_run borrow_elem_map_data test/borrow_elem_map_data.yack "42"
expect_run_stack borrow_elem_map_data_stack test/borrow_elem_map_data.yack "42"
expect_llvm_absent borrow_elem_map_data_slot test/borrow_elem_map_data.yack 'call i32 @yack_value_of\(ptr %slot'
expect_run borrow_elem_array_list test/borrow_elem_array_list.yack "42"
expect_run_stack borrow_elem_array_list_stack test/borrow_elem_array_list.yack "42"
expect_run borrow_elem_kernel_data test/borrow_elem_kernel_data.yack "42"
expect_run_stack borrow_elem_kernel_data_stack test/borrow_elem_kernel_data.yack "42"
expect_run borrow_elem_array_nested test/borrow_elem_array_nested.yack "2"
expect_run_stack borrow_elem_array_nested_stack test/borrow_elem_array_nested.yack "2"
expect_run borrow_elem_map_nested test/borrow_elem_map_nested.yack "1"
expect_run_stack borrow_elem_map_nested_stack test/borrow_elem_map_nested.yack "1"
expect_run borrow_elem_array_record test/borrow_elem_array_record.yack "42"
expect_run_stack borrow_elem_array_record_stack test/borrow_elem_array_record.yack "42"
expect_run borrow_elem_map_record test/borrow_elem_map_record.yack "42"
expect_run_stack borrow_elem_map_record_stack test/borrow_elem_map_record.yack "42"
expect_run borrow_elem_array_nonview test/borrow_elem_array_nonview.yack "42"
expect_run_stack borrow_elem_array_nonview_stack test/borrow_elem_array_nonview.yack "42"
expect_run borrow_elem_map_nonview test/borrow_elem_map_nonview.yack "42"
expect_run_stack borrow_elem_map_nonview_stack test/borrow_elem_map_nonview.yack "42"



expect_run suffix_borrow_tail test/suffix_borrow_tail.yack "55"
expect_run_stack suffix_borrow_tail_stack test/suffix_borrow_tail.yack "55"
expect_error arm_move_used_after E0501 test/reject/arm_move_used_after.yack
expect_error arm_move_same_arm E0501 test/reject/arm_move_same_arm.yack
expect_error arm_move_after_case E0501 test/reject/arm_move_after_case.yack
expect_error trap_lin_wildcard E0403 test/reject/trap_lin_wildcard.yack
expect_error trap_lin_ctor_wildcard E0403 test/reject/trap_lin_ctor_wildcard.yack
expect_error trap_lin_record_omitted E0403 test/reject/trap_lin_record_omitted.yack
expect_error trap_lin_nested_wildcard E0403 test/reject/trap_lin_nested_wildcard.yack
expect_error borrow_builtin_escape E0507 test/reject/borrow_builtin_escape.yack
expect_error borrow_arena_escape E0703 test/reject/borrow_arena_escape.yack
expect_error borrow_arena_escape_composite E0703 test/reject/borrow_arena_escape_composite.yack
expect_error nested_field_use_after_move E0402 test/reject/nested_field_use_after_move.yack
expect_error generic_tuple_field_reuse E0402 test/reject/generic_tuple_field_reuse.yack
expect_error callback_named_owns E0513 test/reject/callback_named_owns.yack
expect_error case_unknown_constructor E3027 test/reject/case_unknown_constructor.yack
expect_error case_branch_type E3004 test/reject/case_branch_type.yack
expect_error pattern_string_type E3004 test/reject/pattern_string_type.yack "string pattern"
expect_error pattern_bytes_type E3004 test/reject/pattern_bytes_type.yack "bit array pattern"
expect_error constructor_arity E3026 test/reject/constructor_arity.yack
expect_error datatype_duplicate_constructor E3024 test/reject/datatype_duplicate_constructor.yack
expect_error datatype_copy_linear E3015 test/reject/datatype_copy_linear.yack
expect_error datatype_affine_linear E3016 test/reject/datatype_affine_linear.yack
expect_error borrow_non_variable E0500 test/reject/borrow_non_variable.yack
expect_error borrow_mut_literal E0500 test/reject/borrow_mut_literal.yack "immutable"
expect_error borrow_return E0503 test/reject/borrow_return.yack
expect_error equality_token E3004 test/reject/equality_token.yack
expect_error generic_add_bool E3004 test/reject/generic_add_bool.yack
expect_error float_modulo E3004 test/reject/float_modulo.yack
expect_error float_equality E3004 test/reject/float_equality.yack
expect_error record_pattern_unknown_field E3014 test/reject/record_pattern_unknown_field.yack
expect_error record_pattern_non_record E3014 test/reject/record_pattern_non_record.yack
expect_error generic_missing_constructor E3021 test/reject/generic_missing_constructor.yack
expect_error generic_type_arity E3029 test/reject/generic_type_arity.yack
expect_error generic_argument_mismatch E3004 test/reject/generic_argument_mismatch.yack
expect_error nested_bool_option_missing E3021 test/reject/nested_bool_option_missing.yack
expect_error nested_tuple_missing E3021 test/reject/nested_tuple_missing.yack
expect_error nested_option_missing E3021 test/reject/nested_option_missing.yack
expect_error borrow_return_ambiguous E0503 test/reject/borrow_return_ambiguous.yack
expect_error borrow_return_wrong_source E0506 test/reject/borrow_return_wrong_source.yack
expect_error borrow_return_range E0504 test/reject/borrow_return_range.yack
expect_error borrow_return_non_borrow_param E0505 test/reject/borrow_return_non_borrow_param.yack
expect_error borrow_local_escape E0507 test/reject/borrow_local_escape.yack
expect_error borrow_from_unknown_name E0505 test/reject/borrow_from_unknown_name.yack
expect_error borrow_let_escape E0501 test/reject/borrow_let_escape.yack
expect_error borrow_aggregate_return E0503 test/reject/borrow_aggregate_return.yack
expect_error lambda_borrow_return E0503 test/reject/lambda_borrow_return.yack
expect_error named_function_alias E0402 test/reject/named_function_alias.yack
expect_error token_field_double_move E0402 test/reject/token_field_double_move.yack
expect_error callback_convention_annotated E3004 test/reject/callback_convention_annotated.yack
expect_error array_affine_element E3040 test/reject/array_affine_element.yack
expect_error list_prelude_redeclare E3013 test/reject/list_prelude_redeclare.yack
expect_error module_use_after_move E0402 test/reject/module_use_after_move.yack
expect_error module_import_prelude_collision E3030 test/reject/module_import_prelude_collision.yack
expect_error module_import_ambiguous E3030 test/reject/module_import_ambiguous.yack
expect_error module_import_alias_collision E3030 test/reject/module_import_alias_collision.yack
expect_error module_import_local_collision E3030 test/reject/module_import_local_collision.yack
expect_error module_import_prelude_alias E3030 test/reject/module_import_prelude_alias.yack
expect_error module_import_unknown E3031 test/reject/module_import_unknown.yack
expect_error module_import_type_as_value E3031 test/reject/module_import_type_as_value.yack
expect_error module_unqualified_import E3002 test/reject/module_unqualified_import.yack
expect_error module_unknown_member E3002 test/reject/module_unknown_member.yack 'unknown function `os.nope`'
expect_error map_get_affine E3044 test/reject/map_get_affine.yack
expect_error map_key_moved E0402 test/reject/map_key_moved.yack
expect_error array_get_affine E3041 test/reject/array_get_affine.yack
expect_error array_kernel_user E3042 test/reject/array_kernel_user.yack




mkdir -p "$TMP/kc/src" "$TMP/kc/mylib/src"
cat > "$TMP/kc/yack.toml" <<'EOF'
[package]
name = "kc"
[dependencies]
mylib = "mylib"
EOF
cat > "$TMP/kc/mylib/src/k.yack" <<'EOF'
fun peek (values : borrow int array) : int = array_kernel_load (values, 1000000)
EOF
cat > "$TMP/kc/src/main.yack" <<'EOF'
import mylib/k
fun main () =
    let val values = array_new ()
    in print_int (k.peek (borrow values))
    end
EOF
expect_error kernel_dependency E3042 "$TMP/kc/src/main.yack" '`array_kernel_load` is an internal array primitive'
mkdir -p "$TMP/kb/src"
cat > "$TMP/kb/yack.toml" <<'EOF'
[package]
name = "kb"
[dependencies]
self = "."
EOF
cat > "$TMP/kb/src/main.yack" <<'EOF'
fun main () =
    let val values : int array = array_new ()
    in print_int (array_kernel_load (borrow values, 100000))
    end
EOF
expect_error kernel_self_dependency E3042 "$TMP/kb/src/main.yack"
stdlib_array_output=$("$YACK" check lib/yack/src/array.yack 2>&1)
stdlib_array_status=$?
if [ "$stdlib_array_status" -ne 0 ] ||
   printf '%s' "$stdlib_array_output" | grep -q '\[E3042\]\|\[W3001\]'; then
  fail stdlib_array_relative "expected a clean check, got exit $stdlib_array_status"
  printf '%s\n' "$stdlib_array_output" | head -20 | sed 's/^/     /'
else
  pass stdlib_array_relative
fi
expect_error array_borrow_mutation E0502 test/reject/array_borrow_mutation.yack
expect_error array_push_moved E0402 test/reject/array_push_moved.yack
expect_error array_borrow_return E0503 test/reject/array_borrow_return.yack
expect_build_error borrow_temp_array E0500 test/reject/borrow_temp_array.yack
expect_build_error borrow_temp_map E0500 test/reject/borrow_temp_map.yack
expect_error borrow_field_affine E0508 test/reject/borrow_field_affine.yack
expect_error borrow_field_double_close E0402 test/reject/borrow_field_double_close.yack
expect_error borrow_field_use_after_close E0402 test/reject/borrow_field_use_after_close.yack
expect_error borrow_field_forgotten E0403 test/reject/borrow_field_forgotten.yack
expect_error unreachable_clause E3022 test/reject/unreachable_clause.yack

expect_run json_combinators examples/json_combinators.yack "4"
expect_run json_decode examples/json_decode.yack "8"
expect_run codegen_int examples/codegen_int.yack "42"
expect_run codegen_add examples/codegen_add.yack $'42\n4.000000'
expect_run codegen_if examples/codegen_if.yack "5"
expect_run codegen_id examples/codegen_id.yack $'42\n1'
expect_run codegen_tuple examples/tuple.yack "42"
expect_run codegen_record examples/record_user.yack "42"
expect_run codegen_record_pattern examples/record_pattern.yack "42"
expect_run codegen_datatype_option examples/datatype_option.yack "42"
expect_run codegen_datatype_list examples/datatype_list.yack "42"
expect_run codegen_generic_list examples/generic_list.yack "2"
expect_run codegen_nested_tuple examples/nested_tuple.yack "2"
expect_run codegen_float_mixed examples/float_mixed.yack "3.500000"
expect_run codegen_stack_local examples/stack_local.yack "42"
expect_run codegen_generic_recursive examples/generic_recursive_list.yack "3"
expect_run codegen_bytes examples/bytes.yack "hello"
expect_run codegen_bytes_recv test/bytes_recv.yack "buffer out of bounds"
expect_run codegen_bytes_send test/bytes_send.yack "buffer out of bounds"
expect_run codegen_sys_loopback examples/sys_loopback.yack "hello"
expect_run codegen_sys_reuseport examples/sys_reuseport.yack "0 0"
expect_run codegen_buffer examples/buffer.yack "hello"
expect_run codegen_borrow_field test/borrow_field.yack "42"
expect_run codegen_tail_rec test/tail_rec.yack "1000000"
expect_run codegen_tail_auto_rec test/tail_auto_rec.yack "1000000"
expect_run codegen_tail_aggregate test/tail_aggregate.yack "10"
expect_run codegen_borrow_return_tuple_ok test/borrow_return_tuple_ok.yack ""
expect_run codegen_callback_convention_lambda_ok test/callback_convention_lambda_ok.yack ""
expect_run codegen_netapp examples/netapp/src/main.yack "hello"
expect_run codegen_httpapp examples/httpapp/src/main.yack "GET /hello HTTP/1.1"
expect_build_error array_copy_datatype E0702 test/reject/array_copy_datatype.yack
expect_build_error array_uninit_string E3040 test/reject/array_uninit_string.yack
expect_check webapp examples/webapp/src/main.yack
expect_check webapp_smoke examples/webapp/src/smoke.yack
expect_run_stack webapp_body_twice examples/webapp/src/request_body_twice.yack '{"items":[1,2,3]}|{"items":[1,2,3]}'
expect_run if_branch_drop test/asan/if_branch_drop.yack ""
expect_run codegen_generic_option_types examples/generic_option_types.yack $'42\n1'
expect_run codegen_record_return examples/record_return.yack "81"
expect_run codegen_tuple_return examples/tuple_return.yack "81"
expect_run codegen_case_literals examples/case_literals.yack $'33\n0'
expect_run codegen_pattern_string examples/pattern_string.yack "hello alice|typo world|api users|exact hello|empty|other"
expect_run codegen_pattern_list_cons examples/pattern_list_cons.yack "three 6 rest 2|two 30|one 7|none"
expect_run codegen_pattern_bytes examples/pattern_bytes.yack "get 6|post 6|bare get 0|numeric put 2|empty|other"
expect_run codegen_case_guard examples/case_guard.yack "100"
expect_run linear_ok test/linear/linear_ok.yack "8"
expect_ssa ssa_passthrough test/ssa/passthrough.yack
for arena_file in test/arena_report/*.yack; do
  arena_name=$(basename "$arena_file" .yack)
  expect_arena "arena_report_${arena_name}_heap" "$arena_file" heap
  expect_arena "arena_report_${arena_name}_stack" "$arena_file" stack
done
expect_ssa ssa_branches test/ssa/branches.yack
expect_ssa ssa_types test/ssa/types.yack
expect_ssa_roundtrip ssa_roundtrip_passthrough test/ssa/passthrough.yack
expect_ssa_roundtrip ssa_roundtrip_branches test/ssa/branches.yack
expect_ssa_roundtrip ssa_roundtrip_types test/ssa/types.yack
expect_ssa_roundtrip ssa_roundtrip_borrowed_lambda test/callback_borrowed_unused.yack
expect_ssa_loans ssa_loans test/ssa/loans.yack test/ssa/loans.ssa
expect_run ssa_loans_run test/ssa/loans.yack "10"


for reject_file in test/ssa/reject/*.ssa; do
  case $reject_file in
    *.ok.ssa) ;;
    *) expect_ssa_reject "$reject_file" ;;
  esac
done

"$YACK" ssa-check test/ssa/reject/copy_sibling.ok.ssa > "$TMP/copy_sibling.ok" 2>&1 \
  && pass ssa_reject_copy_sibling \
  || { fail ssa_reject_copy_sibling "control was rejected"; sed 's/^/     /' "$TMP/copy_sibling.ok"; }

expect_run ssa_trap_partial_join test/ssa/trap/partial_join.yack $'3\n1'
expect_run_stack ssa_trap_partial_join_stack test/ssa/trap/partial_join.yack $'3\n1'
expect_error ssa_trap_field_order E0402 test/ssa/trap/field_order.yack
expect_run ssa_trap_map_temp_key test/ssa/trap/map_temp_key.yack "15"
expect_run ssa_trap_borrowed_alias_view test/ssa/trap/borrowed_alias_view.yack $'7\n7'
expect_run_stack ssa_trap_borrowed_alias_view_stack test/ssa/trap/borrowed_alias_view.yack $'7\n7'
expect_run ssa_trap_linear_field_move test/ssa/trap/linear_field_move.yack "7"
expect_run_stack ssa_trap_linear_field_move_stack test/ssa/trap/linear_field_move.yack "7"
expect_run ssa_opt_concat_heap test/ssa/opt/concat_chain.yack $'a-xy\n;\nb!?\n;\nc123\n;\n[d]\n;\nbefore;\ngef\n;\nhbT\nF\n;\n7\n;\nr...'
expect_run_stack ssa_opt_concat_stack test/ssa/opt/concat_chain.yack $'a-xy\n;\nb!?\n;\nc123\n;\n[d]\n;\nbefore;\ngef\n;\nhbT\nF\n;\n7\n;\nr...'
APPEND_OUT=$(printf 'sxyxyxyxyxy\n3\nqqq<\nlit:mm\n81\nuuvv\n%s|' "$(printf 'z%.0s' $(seq 300))")
expect_run ssa_opt_append_heap test/ssa/opt/append_last_use.yack "$APPEND_OUT"
expect_run_stack ssa_opt_append_stack test/ssa/opt/append_last_use.yack "$APPEND_OUT"

expect_ir_line ssa_opt_chain_fused test/ssa/opt/concat_chain.yack chain 'let val c : string = \(\(\(s \^ "-"\) \^ "x"\) \^ "y"\) in c end'
expect_ir_line ssa_opt_shadow_fused test/ssa/opt/concat_chain.yack shadow '\(\(\(s \^ "1"\) \^ "2"\) \^ "3"\)'
expect_ir_line ssa_opt_arena_fused test/ssa/opt/concat_chain.yack boxed '\(\(s \^ "abc"\) \^ "def"\)'

expect_ir_line ssa_opt_twice_kept test/ssa/opt/concat_chain.yack twice 'val a : string = \(s \^ "!"\)'
expect_ir_line ssa_opt_tail_kept test/ssa/opt/concat_chain.yack tail_use 'val a : string = \(s \^ "\]"\) in \("\[" \^ a\)'
expect_ir_line ssa_opt_effect_kept test/ssa/opt/concat_chain.yack effect 'val a : string = \(s \^ "e"\) in case print_string'
expect_ir_line ssa_opt_branch_kept test/ssa/opt/concat_chain.yack branch 'val a : string = \(s \^ "b"\) in if k'
expect_run codegen_let_destructure examples/let_destructure.yack "42"
expect_run codegen_pipe examples/pipe.yack "16"
expect_run codegen_if_aggregate examples/if_aggregate.yack "42"
expect_run codegen_data_in_record examples/data_in_record.yack "42"
expect_run codegen_case_option_return examples/case_option_return.yack "42"
expect_run codegen_clause_reorder test/clause_reorder.yack "49"
expect_run codegen_case_data_field examples/case_data_field.yack "42"
expect_run codegen_datatype_common_field examples/datatype_common_field.yack $'yack\nocaml\nalpaca'
expect_run codegen_data_wildcard examples/data_wildcard.yack ""
expect_run codegen_data_record_wildcard examples/data_record_wildcard.yack ""
expect_run codegen_int64_arithmetic examples/int64_arithmetic.yack ""
expect_run codegen_empty_record examples/empty_record.yack ""
expect_run codegen_generic_id_data examples/generic_id_data.yack "42"
expect_run codegen_if_data examples/if_data.yack "42"
expect_run codegen_record_data_return examples/record_data_return.yack "42"
expect_run codegen_generic_pair_fns examples/generic_pair_fns.yack "40"
expect_run codegen_unit_param examples/unit_param.yack ""
expect_run codegen_shadow examples/shadow.yack "42"
expect_run codegen_drop_data examples/drop_data.yack ""
expect_run codegen_many_constructors examples/many_constructors.yack "42"
expect_run codegen_record_field_order examples/record_field_order.yack "42"
expect_run codegen_record_field_move examples/record_field_move.yack "abcd"
expect_run codegen_nested_result examples/nested_result.yack "42"
expect_run codegen_http_header examples/http_header.yack "Content-Type -> text/html; charset=utf-8"
expect_run codegen_http_request examples/http_request.yack "Content-Type -> text/html; charset=utf-8"
expect_run codegen_module_main examples/module_main.yack "42"
expect_run codegen_module_import examples/module_import.yack "42"
expect_run codegen_module_qualified examples/module_qualified.yack "6"
expect_run codegen_module_selective examples/module_selective.yack "16"
expect_run codegen_module_alias examples/module_alias.yack "42"
expect_run codegen_module_import_prelude examples/module_import_prelude.yack "42"
expect_run codegen_module_quad examples/module_quad.yack "20"
expect_run codegen_module_quad_selective examples/module_quad_selective.yack "20"
expect_run codegen_module_rename examples/module_rename.yack "42"
expect_run codegen_module_type_alias examples/module_type_alias.yack "16"
expect_run codegen_module_alias_selective examples/module_alias_selective.yack "42"
expect_run codegen_module_discard_qualifier examples/module_discard_qualifier.yack "20"
expect_run package_layout examples/pkg/src/main.yack "42"
expect_run package_dependency examples/pkgdep/app/src/main.yack "49"


expect_run implicit_stdlib test/implicit_stdlib.yack "ok"



if (cd examples/stdlib/src && "$YACK_ABS" check main.yack >/dev/null 2>&1); then
  pass check_bare_name_package
else
  fail check_bare_name_package "yack.toml not found from the source directory"
fi
if (cd examples/pkgdep/app/src && "$YACK_ABS" check main.yack >/dev/null 2>&1); then
  pass check_bare_name_dependency
else
  fail check_bare_name_dependency "dependency not found from the source directory"
fi
if (cd test && "$YACK_ABS" check implicit_stdlib.yack >/dev/null 2>&1); then
  pass check_bare_name_implicit_stdlib
else
  fail check_bare_name_implicit_stdlib "implicit stdlib not found from the file directory"
fi
expect_run codegen_io_roundtrip examples/io_roundtrip.yack "hello from io"
expect_run codegen_io_error examples/io_error.yack "cannot read file"
expect_run codegen_array examples/array.yack "44"
expect_run codegen_array_uninit examples/array_uninit.yack "44"
expect_run codegen_array_bounds examples/array_bounds.yack "13"
expect_run unchecked_in_bounds test/unchecked_in_bounds.yack "109"
expect_run_stack unchecked_in_bounds_stack test/unchecked_in_bounds.yack "109"
expect_no_leak unchecked_in_bounds_leak test/unchecked_in_bounds.yack
expect_abort trap_unchecked_get_oob test/reject_run/unchecked_get_oob.yack "index 10 out of bounds (length 3)"
expect_abort trap_unchecked_get_negative test/reject_run/unchecked_get_negative.yack "index -1 out of bounds (length 3)"
expect_abort trap_unchecked_set_oob test/reject_run/unchecked_set_oob.yack "index 3 out of bounds (length 3)"
expect_abort trap_unchecked_push_full test/reject_run/unchecked_push_full.yack "out of bounds"
expect_abort trap_char_at_unchecked_oob test/reject_run/char_at_unchecked_oob.yack "index 5 out of bounds (length 3)"
expect_abort trap_array_get_oob test/reject_run/array_get_oob.yack "index 3 out of bounds (length 3)"
expect_abort trap_array_set_oob test/reject_run/array_set_oob.yack "index -1 out of bounds (length 1)"
expect_abort trap_array_take_oob test/reject_run/array_take_oob.yack "index 1 out of bounds (length 1)"
expect_abort trap_array_put_oob test/reject_run/array_put_oob.yack "index 1 out of bounds (length 1)"
expect_abort trap_array_get_borrow_oob test/reject_run/array_get_borrow_oob.yack "index 2 out of bounds (length 1)"
expect_abort trap_array_take_twice test/reject_run/array_take_twice.yack "array element 0 was already taken"
expect_abort trap_array_take_twice_record test/reject_run/array_take_twice_record.yack "array element 0 was already taken"
expect_run yack_array test/yack_array.yack $'3\n4\n19\n9\n3\n2\n0'
expect_run_stack yack_array_stack test/yack_array.yack $'3\n4\n19\n9\n3\n2\n0'
expect_no_leak alloc_yack_array_set test/alloc/yack_array_set.yack
expect_no_leak alloc_yack_array_new test/alloc/yack_array_new.yack
expect_no_leak alloc_yack_array_make_get test/alloc/yack_array_make_get.yack
expect_run_stack alloc_yack_array_make_get_stack test/alloc/yack_array_make_get.yack "16"
expect_run_stack alloc_yack_array_new_stack test/alloc/yack_array_new.yack "0"
expect_no_leak alloc_yack_array_get test/alloc/yack_array_get.yack
expect_run_stack alloc_yack_array_get_stack test/alloc/yack_array_get.yack "9"
expect_no_leak alloc_yack_array_datatype test/alloc/yack_array_datatype.yack
expect_run_stack alloc_yack_array_datatype_stack test/alloc/yack_array_datatype.yack "6"
expect_no_leak alloc_yack_array_set_int test/alloc/yack_array_set_int.yack
expect_run_stack alloc_yack_array_set_int_stack test/alloc/yack_array_set_int.yack "4"
expect_no_leak alloc_yack_array_take test/alloc/yack_array_take.yack
expect_run_stack alloc_yack_array_take_stack test/alloc/yack_array_take.yack "3"
expect_no_leak alloc_yack_array_put test/alloc/yack_array_put.yack
expect_run_stack alloc_yack_array_put_stack test/alloc/yack_array_put.yack "2"
expect_no_leak alloc_yack_array_push test/alloc/yack_array_push.yack
expect_run_stack alloc_yack_array_push_stack test/alloc/yack_array_push.yack "2"
expect_no_leak alloc_yack_array_reverse_int test/alloc/yack_array_reverse_int.yack
expect_run_stack alloc_yack_array_reverse_int_stack test/alloc/yack_array_reverse_int.yack "3"
expect_no_leak alloc_yack_array_copy_int test/alloc/yack_array_copy_int.yack
expect_run_stack alloc_yack_array_copy_int_stack test/alloc/yack_array_copy_int.yack "5"
expect_no_leak alloc_yack_array_sum_int test/alloc/yack_array_sum_int.yack
expect_run_stack alloc_yack_array_sum_int_stack test/alloc/yack_array_sum_int.yack "3"
expect_no_leak alloc_yack_array_max_min_int test/alloc/yack_array_max_min_int.yack
expect_run_stack alloc_yack_array_max_min_int_stack test/alloc/yack_array_max_min_int.yack "4"
expect_no_leak alloc_yack_array_index_of_int test/alloc/yack_array_index_of_int.yack
expect_run_stack alloc_yack_array_index_of_int_stack test/alloc/yack_array_index_of_int.yack "1"
expect_abort trap_yack_array_get_oob test/reject_run/yack_array_get_oob.yack "index 5 out of bounds (length 2)"
expect_abort trap_yack_array_set_oob test/reject_run/yack_array_set_oob.yack "index 3 out of bounds (length 1)"
expect_abort trap_yack_array_push_full test/reject_run/yack_array_push_full.yack "out of bounds"
expect_abort_under_vmem trap_oom_array test/reject_run/oom_array.yack 100000 "out of memory"
expect_abort trap_div_zero test/reject_run/div_zero.yack "division by zero"
expect_abort trap_div_zero_const test/reject_run/div_zero_const.yack "division by zero"
expect_abort trap_mod_zero test/reject_run/mod_zero.yack "division by zero"
expect_abort trap_div_zero_int64 test/reject_run/div_zero_int64.yack "division by zero"
expect_abort trap_div_zero_int8 test/reject_run/div_zero_int8.yack "division by zero"
expect_abort trap_div_min_neg1 test/reject_run/div_min_neg1.yack "division overflow"
expect_run div_min_mod test/div_min_mod.yack "0"
expect_run div_signs test/div_signs.yack $'-3\n-3\n-1\n1'
expect_llvm_absent div_const_no_guard test/div_const.yack 'div\.zero\.abort'
expect_abort trap_overflow_add test/reject_run/overflow_add.yack "integer overflow"
expect_abort trap_overflow_sub test/reject_run/overflow_sub.yack "integer overflow"
expect_abort trap_overflow_mul test/reject_run/overflow_mul.yack "integer overflow"
expect_abort trap_overflow_neg test/reject_run/overflow_neg.yack "integer overflow"
expect_abort trap_overflow_add64 test/reject_run/overflow_add64.yack "integer overflow"
expect_abort trap_overflow_sub64 test/reject_run/overflow_sub64.yack "integer overflow"
expect_abort trap_overflow_mul64 test/reject_run/overflow_mul64.yack "integer overflow"
expect_abort trap_overflow_neg64 test/reject_run/overflow_neg64.yack "integer overflow"
expect_abort trap_overflow_add8 test/reject_run/overflow_add8.yack "integer overflow"
expect_abort trap_overflow_int64_to_int test/reject_run/overflow_int64_to_int.yack "integer overflow"
expect_abort trap_overflow_sub8 test/reject_run/overflow_sub8.yack "integer overflow"
expect_abort trap_overflow_mul8 test/reject_run/overflow_mul8.yack "integer overflow"
expect_abort trap_overflow_neg8 test/reject_run/overflow_neg8.yack "integer overflow"
expect_run wrapping_int test/wrapping_int.yack $'-2147483648\n2147483647\n1410065408'
expect_run wrapping_int64 test/wrapping_int64.yack $'-9223372036854775808\n9223372036854775807\n-2\n-2147483648\n-42'
expect_run codegen_array_string examples/array_string.yack "9"
expect_run bytes_range test/bytes_range.yack $'cde\nfgh\nabcdefgh'
expect_run codegen_array_record examples/array_record.yack "2"
expect_run codegen_array_nested examples/array_nested.yack "11"
expect_run arena_heap_strings test/stack/scratch_container_string.yack "hello world"
expect_run arena_heap_map test/stack/scratch_map_string.yack "value1"
expect_run arena_heap_call test/stack/scratch_call_string.yack "hello"
expect_run arena_heap_tuple test/stack/scratch_tuple_store.yack "1"
expect_run arena_heap_nested_tuple test/stack/scratch_nested_tuple_store.yack "1"
expect_run arena_array_put test/stack/array_put_escape.yack "ab"
expect_run arena_array_put_arena test/stack/array_put_arena_rooted.yack "ab"
expect_run arena_result_data test/stack/arena_result_data.yack "ab"
expect_run arena_result_list test/stack/arena_result_list.yack "2"
expect_no_leak alloc_temp_array test/alloc/temp_array.yack
expect_no_leak tail_loop_string test/alloc/tail_loop_string.yack
expect_no_leak tail_param_string test/alloc/tail_param_string.yack
expect_no_leak tail_param_record test/alloc/tail_param_record.yack
expect_no_leak tail_param_branch test/alloc/tail_param_branch.yack
expect_no_leak drop_deep_option test/alloc/drop_deep_option.yack
expect_no_leak drop_deep_option_option test/alloc/drop_deep_option_option.yack
expect_no_leak drop_deep_record test/alloc/drop_deep_record.yack
expect_no_leak option_name_prefix test/alloc/option_name_prefix.yack
expect_no_leak nested_types test/alloc/nested_types.yack
expect_no_leak alloc_list_api test/alloc/list_api.yack
expect_no_leak tail_param_array test/alloc/tail_param_array.yack
expect_no_leak alloc_temp_map test/alloc/temp_map.yack
expect_no_leak alloc_temp_array_arena test/alloc/temp_array_arena.yack
expect_no_leak alloc_temp_array_get test/alloc/temp_array_get.yack
expect_no_leak alloc_temp_array_get_arena test/alloc/temp_array_get_arena.yack
expect_no_leak alloc_temp_map_get test/alloc/temp_map_get.yack
expect_no_leak alloc_temp_bytes test/alloc/temp_bytes.yack
expect_no_leak alloc_copy_list test/alloc/copy_list.yack
expect_no_leak alloc_copy_box test/alloc/copy_box.yack
expect_no_leak alloc_copy_map_temp test/alloc/copy_map_temp.yack
expect_no_leak alloc_copy_use_twice test/alloc/copy_use_twice.yack
expect_run codegen_list_prelude examples/list_prelude.yack "11"
expect_run list_sum_order test/list_sum_order.yack "2147483647"
expect_run codegen_module_string examples/module_string.yack "13"
expect_run codegen_map_int examples/map_int.yack "133"
expect_run codegen_map_string examples/map_string.yack "71"
expect_run codegen_borrow_amp test/borrow_amp.yack "46"
expect_run codegen_consume_bang test/consume_bang.yack "5"
expect_run codegen_borrow_field examples/borrow_field.yack "209"
expect_run codegen_borrow_mut_field examples/borrow_mut_field.yack ""
expect_run codegen_borrow_map_field examples/borrow_map_field.yack "0"
expect_run codegen_conversions examples/conversions.yack "33784561"
expect_run codegen_conversions_widths examples/conversions_widths.yack "342701004"
expect_run codegen_format_builtins examples/format_builtins.yack $'ada has 42 apples (100%)\n7\n42\n1.5\ncopy'
expect_run codegen_map_keys examples/map_keys.yack "2305"
expect_run codegen_map_keys_edges examples/map_keys_edges.yack "112"
expect_run codegen_prelude_helpers examples/prelude_helpers.yack "1111"


if output=$("$YACK" check "$PWD/examples/module_string.yack" 2>&1); then
  pass absolute_module_path
else
  fail absolute_module_path "absolute entry path failed" "$output"
fi
expect_stack stack_local_allocation examples/stack_local.yack


expect_ir prelude_list_drop examples/list_prelude.yack "yack_drop_yack_data_list__string"
expect_ir module_param_drop examples/module_string.yack "yack_drop_string({ ptr, i64, i1 } %arg0)"

expect_ir borrow_field_place examples/borrow_field.yack "place.fieldp"
expect_ir borrow_field_load examples/borrow_field.yack "place.load"



new_project="$TMP/projects/hello"
if output=$("$YACK" new "$new_project" </dev/null 2>&1); then
  pass new_project
else
  fail new_project "scaffold failed" "$output"
fi

if [ -f "$new_project/yack.toml" ] &&
   grep -q 'name = "hello"' "$new_project/yack.toml"; then
  pass new_project_manifest
else
  fail new_project_manifest "missing or invalid yack.toml"
fi

if [ -f "$new_project/src/main.yack" ] &&
   grep -q 'Hello from Yack !' "$new_project/src/main.yack"; then
  pass new_project_source
else
  fail new_project_source "missing Hello from Yack ! source"
fi

if [ -f "$new_project/README.md" ] && [ -f "$new_project/.gitignore" ]; then
  pass new_project_files
else
  fail new_project_files "missing README.md or .gitignore"
fi

new_output=$(cd "$new_project" && "$YACK_ABS" run 2>/dev/null)
new_status=$?
if [ "$new_status" -eq 0 ] && [ "$new_output" = "Hello from Yack !" ]; then
  pass new_project_run
else
  fail new_project_run "unexpected output" \
    "status=$new_status" "output=[$new_output]"
fi

new_output=$(cd "$new_project" && "$YACK_ABS" run src/main.yack 2>/dev/null)
new_status=$?
if [ "$new_status" -eq 0 ] && [ "$new_output" = "Hello from Yack !" ]; then
  pass new_project_run_file
else
  fail new_project_run_file "unexpected output" \
    "status=$new_status" "output=[$new_output]"
fi

if "$YACK" new "$new_project" --template hello >/dev/null 2>&1; then
  fail new_refuses_existing "expected rejection"
else
  pass new_refuses_existing
fi

if "$YACK" new "hello world" --template hello >/dev/null 2>&1; then
  fail new_invalid_name "expected rejection"
else
  pass new_invalid_name
fi

if "$YACK" new --help >/dev/null 2>&1; then
  pass new_help
else
  fail new_help "help failed"
fi

cli_project="$TMP/projects/tool"
if "$YACK" new "$cli_project" --template cli >/dev/null 2>&1 &&
   grep -q 'import yack/args' "$cli_project/src/main.yack" &&
   grep -q '^yack = { path = ' "$cli_project/yack.toml"; then
  pass new_template_cli
else
  fail new_template_cli "CLI template is missing its source or dependency"
fi

cli_output=$(cd "$cli_project" && "$YACK_ABS" run 2>/dev/null)
cli_status=$?
if [ "$cli_status" -eq 0 ] && [ "$cli_output" = "Hello from Yack !" ]; then
  pass new_template_cli_run
else
  fail new_template_cli_run "unexpected output" \
    "status=$cli_status" "output=[$cli_output]"
fi



if "$YACK" fmt --check "$cli_project/src/main.yack" >/dev/null 2>&1 &&
   grep -q 'option.unwrap_or' "$cli_project/src/main.yack" &&
   grep -q 'args.take_option (' "$cli_project/src/main.yack" &&
   ! grep -q 'take_option_or' "$cli_project/src/main.yack"; then
  pass new_template_cli_style
else
  fail new_template_cli_style "template is not formatted, or bypasses yack/option"
fi

web_project="$TMP/projects/site"
if "$YACK" new "$web_project" --template web >/dev/null 2>&1 &&
   grep -q 'import alpage/Alpage' "$web_project/src/main.yack" &&
   grep -q '^alpage = { path = ' "$web_project/yack.toml" &&
   "$YACK" check "$web_project/src/main.yack" >/dev/null 2>&1; then
  pass new_template_web
else
  fail new_template_web "web template does not type-check"
fi

tui_project="$TMP/projects/screen"
if "$YACK" new "$tui_project" -t tui >/dev/null 2>&1 &&
   "$YACK" check "$tui_project/src/main.yack" >/dev/null 2>&1; then
  pass new_template_tui
else
  fail new_template_tui "tui template does not type-check"
fi

if "$YACK" new "$TMP/projects/bad" --template nope >/dev/null 2>&1; then
  fail new_template_unknown "expected rejection"
else
  pass new_template_unknown
fi


if "$YACK" build examples/fact.yack "$TMP/trim-default" >/dev/null 2>&1 &&
   "$YACK" build --trim examples/fact.yack "$TMP/trim-trimmed" >/dev/null 2>&1; then
  default_size=$(wc -c < "$TMP/trim-default")
  trimmed_size=$(wc -c < "$TMP/trim-trimmed")
  if [ "$trimmed_size" -lt "$default_size" ] &&
     timeout 10 "$TMP/trim-trimmed" >/dev/null 2>&1; then
    pass trim_build
  else
    fail trim_build "expected a smaller, working binary"
  fi
else
  fail trim_build "build failed"
fi



for example in examples/*.yack; do
  name=$(basename "$example" .yack)
  if ! timeout 20 "$YACK" build "$example" "$TMP/all_$name" >/dev/null 2>&1; then
    fail "build_$name" "compile failed"
    continue
  fi
  if ! timeout 10 "$TMP/all_$name" >/dev/null 2>&1; then
    fail "run_$name" "program failed"
    continue
  fi
  pass "build_$name"
done

if [ "$failed" -ne 0 ]; then
  printf '\ntests failed\n'
  exit 1
fi

printf '\nall tests passed\n'
