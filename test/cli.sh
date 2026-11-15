#!/usr/bin/env bash

set -u

cd "$(dirname "$0")/.."

YACK=$PWD/yack
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

failed=0

pass() {
  printf 'ok   %s\n' "$1"
}

fail() {
  printf 'FAIL %s\n' "$1"
  failed=1
}

build() {
  ( cd examples/cli && "$YACK" build "src/$1.yack" "$TMP/$1" >/dev/null 2>&1 )
}

for program in ls cat wc main; do
  output=$TMP/$program
  if build "$program"; then
    pass "build $program"
  else
    fail "build $program"
  fi
done


needs_sqlite() {
  readelf -d "$1" 2>/dev/null | grep -q 'libsqlite3'
}

if command -v readelf >/dev/null 2>&1; then
  if needs_sqlite "$TMP/wc"; then
    fail "wc does not link libsqlite3"
  else
    pass "wc does not link libsqlite3"
  fi
else
  pass "wc does not link libsqlite3 (readelf absent, skipped)"
fi

if ( cd test/sql && "$YACK" build open_temp_path.yack "$TMP/sql-open" >/dev/null 2>&1 ); then
  pass "build sql open_temp_path"
  if command -v readelf >/dev/null 2>&1; then
    if needs_sqlite "$TMP/sql-open"; then
      pass "sql program links libsqlite3"
    else
      fail "sql program links libsqlite3"
    fi
  else
    pass "sql program links libsqlite3 (readelf absent, skipped)"
  fi
else
  fail "build sql open_temp_path"
fi


actual=$(cd examples/cli && "$TMP/ls" -al README.md src/ls.yack)
status=$?
readme=$(stat -c %s examples/cli/README.md)
expected=$(printf '%8s  README.md\n%8s  src/ls.yack' "$readme" "3974")
if [ "$status" -eq 0 ] && [ "$actual" = "$expected" ]; then
  pass "ls -al sizes"
else
  fail "ls -al sizes"
  printf '%s\n' "$actual" | sed 's/^/     /'
fi


( cd examples/cli && "$TMP/ls" >/dev/null 2>&1 )
if [ "$?" -eq 2 ]; then
  pass "ls usage exit status"
else
  fail "ls usage exit status"
fi


error=$(cd examples/cli && "$TMP/ls" -l missing.yack 2>&1 >/dev/null)
status=$?
if [ "$status" -eq 1 ] && printf '%s' "$error" | grep -q "ls: missing.yack"; then
  pass "ls missing file"
else
  fail "ls missing file"
  printf '%s\n' "$error" | sed 's/^/     /'
fi


actual=$(printf 'a b\nc\n' | "$TMP/wc" | tr -s ' ' | sed 's/^ //')
expected='2 3 6 -'
if [ "$actual" = "$expected" ]; then
  pass "wc stdin"
else
  fail "wc stdin"
  printf '%s\n' "$actual" | sed 's/^/     /'
fi


yes 'yack cli example line' | head -c 5000 > "$TMP/big.bin"
"$TMP/cat" "$TMP/big.bin" > "$TMP/big.out"
if cmp -s "$TMP/big.bin" "$TMP/big.out"; then
  pass "cat large file"
else
  fail "cat large file"
fi


"$TMP/cat" "$TMP/missing.bin" >/dev/null 2>&1
if [ "$?" -eq 1 ]; then
  pass "cat missing file"
else
  fail "cat missing file"
fi


mkdir -p "$TMP/args/src"
cat > "$TMP/args/yack.toml" <<EOF
[package]
name = "args-test"
version = "0.1.0"

[dependencies]
yack = { path = "$PWD/lib/yack" }
EOF
cat > "$TMP/args/src/main.yack" <<'EOF'
import yack/args
import yack/bool
import yack/io
import yack/os
import yack/string

fun specs () : args.spec list =
    [
        args.Flag (("all", Some ("a"), "all")),
        args.Flag (("long", Some ("l"), "long")),
        args.Opt (("output", Some ("o"), "output", None))
    ]

fun main () =
    case args.parse (specs (), os.command_args ()) of
        Error (message) => print_string (message)
      | Ok (value) =>
            let val allPair = args.take_flag (value, "all")
                val longPair = args.take_flag (allPair.1, "long")
                val outPair = args.take_option_or (longPair.1, "output", "-")
                val rest = args.positionals (outPair.1)
            in
                let val _ =
                        print_string
                            (bool.to_string (allPair.0) ^ " " ^
                             bool.to_string (longPair.0) ^ " " ^
                             outPair.0 ^ " " ^ string.join (",", rest))
                in io.print_lines (Cons (("x", Cons (("y", Nil)))))
                end
            end
EOF
if ( cd "$TMP/args" && "$YACK" build src/main.yack args-test >/dev/null 2>&1 ); then
  actual=$("$TMP/args/args-test" -al -o out a.yack b.yack)
  expected=$'true true out a.yack,b.yack\nx\ny'
  if [ "$actual" = "$expected" ]; then
    pass "args -al cluster"
  else
    fail "args -al cluster"
    printf '%s\n' "$actual" | sed 's/^/     /'
  fi
  actual=$("$TMP/args/args-test" -al=x 2>&1)
  if printf '%s' "$actual" | grep -q "unknown option"; then
    pass "args flag with value"
  else
    fail "args flag with value"
    printf '%s\n' "$actual" | sed 's/^/     /'
  fi
else
  fail "args cluster test build"
fi


mkdir -p "$TMP/relargs/src"
printf '[package]\nname = "relargs-test"\nversion = "0.1.0"\n\n[dependencies]\nyack = { path = "%s" }\n' \
  "$(realpath --relative-to="$TMP/relargs" "$PWD/lib/yack")" > "$TMP/relargs/yack.toml"
echo 'fun main () = print_string ("relative dep")' > "$TMP/relargs/src/main.yack"
if ( cd "$TMP/relargs" && "$YACK" build src/main.yack relargs-test >/dev/null 2>&1 ) &&
   [ "$("$TMP/relargs/relargs-test")" = "relative dep" ]; then
  pass "relative dependency path"
else
  fail "relative dependency path"
fi


demo=$(cd examples/cli && "$TMP/main" -v -o "$TMP/out.txt" a.yack)
if printf '%s' "$demo" | grep -q "out.txt" && printf '%s' "$demo" | grep -q "done"; then
  pass "cli demo"
else
  fail "cli demo"
fi



LIB=$PWD/lib
mkdir -p "$TMP/bin" "$TMP/outside/src"
cp "$YACK" "$TMP/bin/yack"
cat > "$TMP/outside/src/stdlib.yack" <<'EOF'
import yack/io

fun main () = io.print_line (borrow "stdlib ok")
EOF
if ( cd "$TMP/outside/src" &&
     YACK_LIB="$LIB" "$TMP/bin/yack" build stdlib.yack stdlib >/dev/null 2>&1 ) &&
   [ "$("$TMP/outside/src/stdlib")" = "stdlib ok" ]; then
  pass "stdlib via YACK_LIB"
else
  fail "stdlib via YACK_LIB"
fi

output=$( cd "$TMP/outside/src" &&
          env -u YACK_LIB "$TMP/bin/yack" build stdlib.yack stdlib-nolib 2>&1 )
status=$?
if [ "$status" -ne 0 ] &&
   printf '%s' "$output" | grep -q "cannot find module yack/io" &&
   printf '%s' "$output" | grep -q "YACK_LIB"; then
  pass "stdlib hint without YACK_LIB"
else
  fail "stdlib hint without YACK_LIB"
  printf '%s\n' "$output" | sed 's/^/     /'
fi


mkdir -p "$TMP/dist" "$TMP/tarsrc"
cp "$YACK" "$TMP/dist/yack"
cp -R "$LIB" "$TMP/dist/lib"
cp "$TMP/outside/src/stdlib.yack" "$TMP/tarsrc/stdlib.yack"
if ( cd "$TMP/tarsrc" &&
     env -u YACK_LIB "$TMP/dist/yack" build stdlib.yack stdlib >/dev/null 2>&1 ) &&
   [ "$("$TMP/tarsrc/stdlib")" = "stdlib ok" ]; then
  pass "stdlib next to the executable"
else
  fail "stdlib next to the executable"
fi



explain_output=$("$YACK" explain E0501)
explain_status=$?
if [ "$explain_status" -eq 0 ] &&
   printf '%s' "$explain_output" | grep -Fq "E0501 — déplacement d'une valeur empruntée" &&
   printf '%s' "$explain_output" | grep -Fq "fun eat" &&
   ! printf '%s' "$explain_output" | grep -q '```'; then
  pass "explain E0501"
else
  fail "explain E0501" "unexpected output"
  printf '%s\n' "$explain_output" | sed 's/^/     /'
fi

explain_lower=$("$YACK" explain e0501)
if [ "$explain_lower" = "$explain_output" ]; then
  pass "explain is case-insensitive"
else
  fail "explain is case-insensitive" "e0501 differs from E0501"
fi

for unknown in E9999 foo; do
  explain_error=$("$YACK" explain "$unknown" 2>&1 >/dev/null)
  explain_status=$?
  if [ "$explain_status" -eq 1 ] &&
     printf '%s' "$explain_error" | grep -Fq "error: unknown diagnostic code '$unknown'" &&
     printf '%s' "$explain_error" | grep -Fq \
       "= help: run 'yack explain' to list the documented codes"; then
    pass "explain unknown $unknown"
  else
    fail "explain unknown $unknown" "unexpected output"
    printf '%s\n' "$explain_error" | sed 's/^/     /'
  fi
done

docs_codes=$(grep -c '^### ' docs/errors.md)
listed=$("$YACK" explain | wc -l | tr -d ' ')
if [ "$listed" = "$docs_codes" ]; then
  pass "explain lists every code"
else
  fail "explain lists every code" "$listed lines for $docs_codes sections"
fi

explain_all_ok=1
for code in $(grep -oE '^### [EW][0-9]{4}' docs/errors.md | cut -c5-); do
  if ! "$YACK" explain "$code" >/dev/null 2>&1; then
    fail "explain $code" "expected exit 0"
    explain_all_ok=0
    break
  fi
done
if [ "$explain_all_ok" -eq 1 ]; then
  pass "explain every documented code"
fi


if "$TMP/dist/yack" explain E0501 >/dev/null 2>&1; then
  pass "explain from a copied binary"
else
  fail "explain from a copied binary"
fi


cat > "$TMP/explain-bad.yack" <<'EOF'
fun main () = print_int ("text")
EOF
explain_error=$("$YACK" check "$TMP/explain-bad.yack" 2>&1)
explain_status=$?
explain_last=$(printf '%s\n' "$explain_error" | tail -n 1)
if [ "$explain_status" -eq 1 ] &&
   [ "$explain_last" = "For more information about an error, try 'yack explain E3004'." ]; then
  pass "explain hint after a type error"
else
  fail "explain hint after a type error" "got [$explain_last]"
fi



cp docs/errors.md "$TMP/errors.md"
printf '\n### E9997 — titre de test\nCorps de test.\n' >> "$TMP/errors.md"
if LC_ALL=C awk -f tools/gen_error_docs.awk "$TMP/errors.md" \
     > "$TMP/error_docs.sml" &&
   grep -Fq '"E9997", "titre de test"' "$TMP/error_docs.sml" &&
   grep -Eq '^\$\(ERROR_DOCS\): docs/errors.md tools/gen_error_docs.awk' Makefile; then
  pass "explain generator regeneration"
else
  fail "explain generator regeneration" "new section not generated, or Makefile rule lost its prerequisites"
fi

exit "$failed"
