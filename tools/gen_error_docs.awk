# Generate src/error_docs.sml from docs/errors.md.
#
# POSIX awk only: the build must not depend on python.  Documentation says:
#
#   ### E2001 — erreur lexicale
#   body ...
#
# Each `### CODE — titre` section becomes one `(code, title, body)` entry.
# The body runs until the next heading; text before the first section and
# `##` group headings are ignored.

function escape(text,    out, i, ch) {
  out = ""
  for (i = 1; i <= length(text); i++) {
    ch = substr(text, i, 1)
    if (ch == "\\")
      out = out "\\\\"
    else if (ch == "\"")
      out = out "\\\""
    else
      out = out ch
  }
  return out
}

function quoted(text) {
  return "\"" escape(text) "\""
}

# Emit the entry collected so far, trimming leading and trailing blank lines
# so the terminal output has no stray empty line at either end.
function flush(    first, last, body, i) {
  if (!active) return
  first = 1
  last = nbody
  while (first <= last && bl[first] == "") first++
  while (last >= first && bl[last] == "") last--
  body = ""
  for (i = first; i <= last; i++)
    body = body (i > first ? "\\n" : "") escape(bl[i])
  printf "%s (%s, %s, \"%s\")\n", (count == 0 ? "      [" : "      ,"), \
    quoted(code), quoted(title), body
  count++
  active = 0
}

BEGIN {
  print "(* Generated from docs/errors.md by the Makefile.  Do not edit. *)"
  print "structure ErrorDocs ="
  print "struct"
  print "  val entries : (string * string * string) list ="
}

/^### / {
  flush()
  code = substr($0, 5, 5)
  if (code !~ /^[EW][0-9][0-9][0-9][0-9]$/) next
  heading = substr($0, 10)
  sub(/^ — /, "", heading)
  title = heading
  active = 1
  nbody = 0
  next
}

/^#/ {
  flush()
  next
}

{
  if (active) {
    nbody++
    bl[nbody] = $0
  }
}

END {
  flush()
  if (count == 0) print "      ["
  print "      ]"
  print "end"
}
