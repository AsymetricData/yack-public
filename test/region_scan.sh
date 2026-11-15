#!/usr/bin/env bash





set -u

cd "$(dirname "$0")/.."

YACK=./yack
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
failed=0
scanned=0

for program in $(find examples bench letsgo zoo site hello_server tutorial test \
                   -name '*.yack' \
                   ! -path 'test/reject/*' \
                   ! -path 'test/reject_run/*' \
                   ! -path 'test/poison/*' 2>/dev/null); do
  scanned=$((scanned + 1))
  if timeout 20 "$YACK" ir --memory=stack "$program" \
       >/dev/null 2>"$TMP/one.log"; then
    continue
  fi
  if grep -q 'E0701\|E0704' "$TMP/one.log"; then
    printf 'FAIL %s\n' "$program"
    grep -m2 'E070' "$TMP/one.log"
    failed=1
  fi
done

if [ "$failed" -ne 0 ]; then
  echo "region verifier failed"
  exit 1
fi
printf 'region verifier: %d programs, no E0701/E0704\n' "$scanned"
