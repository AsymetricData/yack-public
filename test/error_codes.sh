#!/usr/bin/env bash





set -euo pipefail

cd "$(dirname "$0")/.."

export LC_ALL=C

docs=docs/errors.md

if [ ! -f "$docs" ]; then
  echo "error codes: missing $docs" >&2
  exit 1
fi





sources=()
for source in src/*.sml; do
  [ "$source" = src/error_docs.sml ] || sources+=("$source")
done
emitted=$(grep -ohE '[EW][0-9]{4}' "${sources[@]}" | sort -u)
documented=$(grep -oE '^### [EW][0-9]{4}' "$docs" |
  sed 's/^### //' | sort -u)

undocumented=$(comm -23 <(printf '%s\n' "$emitted") \
                         <(printf '%s\n' "$documented"))
stale=$(comm -13 <(printf '%s\n' "$emitted") \
                  <(printf '%s\n' "$documented"))

if [ -n "$undocumented" ]; then
  echo "error codes: emitted but undocumented in $docs:" >&2
  printf '  %s\n' $undocumented >&2
fi

if [ -n "$stale" ]; then
  echo "error codes: documented in $docs but no longer emitted:" >&2
  printf '  %s\n' $stale >&2
fi

if [ -n "$undocumented" ] || [ -n "$stale" ]; then
  exit 1
fi

count=$(printf '%s\n' "$documented" | grep -c .)
echo "error codes: $count documented"
