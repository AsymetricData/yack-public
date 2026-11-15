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

exit "$failed"
