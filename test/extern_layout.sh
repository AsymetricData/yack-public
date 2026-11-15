#!/usr/bin/env bash












set -euo pipefail

cd "$(dirname "$0")/.."

export LC_ALL=C

self_test_only=0
case "${1:-}" in
  "") ;;
  --self-test) self_test_only=1 ;;
  *)
    echo "usage: $0 [--self-test]" >&2
    exit 2
    ;;
esac

os=$(uname -s 2>/dev/null || echo unknown)
machine=$(uname -m 2>/dev/null || echo unknown)
if [ "$os" != Linux ] || [ "$machine" != x86_64 ]; then
  echo "skip extern_layout: not Linux x86-64"
  exit 0
fi




python3 - "$self_test_only" <<'PY'
import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = "."
# Offsets and sizes documented as Linux x86-64.  Each record maps its Yack
# fields to one C struct member: (offsetof expression, sizeof expression,
# C spelling used in the failure message).
STRUCTS = {
    "timespec": {
        "size_expr": "sizeof(struct timespec)",
        "size_name": "struct timespec",
        "fields": {
            "tv_sec": ("offsetof(struct timespec, tv_sec)",
                       "sizeof(((struct timespec *)0)->tv_sec)", "long"),
            "tv_nsec": ("offsetof(struct timespec, tv_nsec)",
                        "sizeof(((struct timespec *)0)->tv_nsec)", "long"),
        },
    },
    "raw_stat": {
        "size_expr": "sizeof(struct stat)",
        "size_name": "struct stat",
        "fields": {
            "st_mode": ("offsetof(struct stat, st_mode)",
                        "sizeof(((struct stat *)0)->st_mode)", "mode_t"),
            "st_size": ("offsetof(struct stat, st_size)",
                        "sizeof(((struct stat *)0)->st_size)", "off_t"),
            "st_mtim": ("offsetof(struct stat, st_mtim)",
                        "sizeof(((struct stat *)0)->st_mtim)",
                        "struct timespec"),
        },
    },
    "addrinfo": {
        "size_expr": "sizeof(struct addrinfo)",
        "size_name": "struct addrinfo",
        "fields": {
            "ai_flags": ("offsetof(struct addrinfo, ai_flags)",
                         "sizeof(((struct addrinfo *)0)->ai_flags)", "int"),
            "ai_family": ("offsetof(struct addrinfo, ai_family)",
                          "sizeof(((struct addrinfo *)0)->ai_family)", "int"),
            "ai_socktype": ("offsetof(struct addrinfo, ai_socktype)",
                            "sizeof(((struct addrinfo *)0)->ai_socktype)",
                            "int"),
            "ai_protocol": ("offsetof(struct addrinfo, ai_protocol)",
                            "sizeof(((struct addrinfo *)0)->ai_protocol)",
                            "int"),
            "ai_addrlen": ("offsetof(struct addrinfo, ai_addrlen)",
                           "sizeof(((struct addrinfo *)0)->ai_addrlen)",
                           "socklen_t"),
            "ai_addr": ("offsetof(struct addrinfo, ai_addr)",
                        "sizeof(((struct addrinfo *)0)->ai_addr)",
                        "struct sockaddr *"),
            "ai_canonname": ("offsetof(struct addrinfo, ai_canonname)",
                             "sizeof(((struct addrinfo *)0)->ai_canonname)",
                             "char *"),
            "ai_next": ("offsetof(struct addrinfo, ai_next)",
                        "sizeof(((struct addrinfo *)0)->ai_next)",
                        "struct addrinfo *"),
        },
    },
    # `getaddrinfo` writes a `struct addrinfo *` into the output parameter.
    "addr_result": {
        "size_expr": "sizeof(void *)",
        "size_name": "void *",
        "fields": {
            "value": ("(size_t)0", "sizeof(void *)", "struct addrinfo *"),
        },
    },
    "pollfd": {
        "size_expr": "sizeof(struct pollfd)",
        "size_name": "struct pollfd",
        "fields": {
            "fd": ("offsetof(struct pollfd, fd)",
                   "sizeof(((struct pollfd *)0)->fd)", "int"),
            "events": ("offsetof(struct pollfd, events)",
                       "sizeof(((struct pollfd *)0)->events)", "short"),
        },
    },
    # relecture de `revents` dans le même `struct pollfd`.
    "poll_result": {
        "size_expr": "sizeof(struct pollfd)",
        "size_name": "struct pollfd",
        "fields": {
            "revents": ("offsetof(struct pollfd, revents)",
                        "sizeof(((struct pollfd *)0)->revents)", "short"),
        },
    },
    # sortie de `getsockopt (SOL_SOCKET, SO_ERROR)`.
    "so_error": {
        "size_expr": "sizeof(int)",
        "size_name": "int",
        "fields": {
            "value": ("(size_t)0", "sizeof(int)", "int"),
        },
    },
}

TYPE_SIZES = {
    "int8": 1,
    "int16": 2,
    "int32": 4,
    "int64": 8,
    "float32": 4,
    "float64": 8,
    "c_ptr": 8,
}

RECORD_RE = re.compile(
    r"extern\s+record\s+([A-Za-z_][A-Za-z0-9_]*)\s*"
    r"\(\s*size\s*=\s*([0-9]+)\s*\)\s*\{(.*?)\}",
    re.S,
)
FIELD_RE = re.compile(
    r"([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([A-Za-z_][A-Za-z0-9_]*)\s*@\s*([0-9]+)"
)


def parse_records():
    """Return {name: {file, size, fields: [(name, type, offset)]}}."""
    records = {}
    for path in sorted(glob.glob(os.path.join(ROOT, "lib", "**", "*.yack"),
                                 recursive=True)):
        with open(path, encoding="utf-8") as handle:
            text = handle.read()
        for match in RECORD_RE.finditer(text):
            name, size, body = match.group(1), int(match.group(2)), match.group(3)
            fields = [(f.group(1), f.group(2), int(f.group(3)))
                      for f in FIELD_RE.finditer(body)]
            rel = os.path.relpath(path, ROOT)
            records[name] = {"file": rel, "size": size, "fields": fields}
    return records


def type_size(name, records):
    if name in TYPE_SIZES:
        return TYPE_SIZES[name]
    return records[name]["size"]


def build_c(records, mapping):
    lines = [
        "#define _GNU_SOURCE",
        "#include <stddef.h>",
        "#include <stdio.h>",
        "#include <time.h>",
        "#include <sys/stat.h>",
        "#include <netdb.h>",
        "#include <poll.h>",
        "#include <sys/socket.h>",
        "",
        "int main(void) {",
    ]
    for name in sorted(records):
        spec = mapping[name]
        lines.append('  printf("size %s %%zu\\n", (size_t)(%s));'
                     % (name, spec["size_expr"]))
        for field in records[name]["fields"]:
            offset_expr, size_expr, _c_name = spec["fields"][field[0]]
            key = "%s.%s" % (name, field[0])
            lines.append('  printf("offset %s %%zu\\n", (size_t)(%s));'
                         % (key, offset_expr))
            lines.append('  printf("fieldsize %s %%zu\\n", (size_t)(%s));'
                         % (key, size_expr))
    lines.append("  return 0;")
    lines.append("}")
    return "\n".join(lines) + "\n"


def run_c(records, mapping):
    tmpdir = tempfile.mkdtemp(prefix="extern_layout.")
    try:
        source = os.path.join(tmpdir, "layout.c")
        binary = os.path.join(tmpdir, "layout")
        with open(source, "w", encoding="utf-8") as handle:
            handle.write(build_c(records, mapping))
        cc = os.environ.get("CC") or "cc"
        proc = subprocess.run([cc, "-std=c11", "-O0", "-o", binary, source],
                              capture_output=True, text=True)
        if proc.returncode != 0:
            sys.stderr.write("extern_layout: %s failed to build the C probe:\n%s"
                             % (cc, proc.stderr))
            raise SystemExit(1)
        run = subprocess.run([binary], capture_output=True, text=True)
        if run.returncode != 0:
            sys.stderr.write("extern_layout: C probe failed:\n%s" % run.stderr)
            raise SystemExit(1)
        values = {}
        for line in run.stdout.splitlines():
            kind, key, value = line.split(" ", 2)
            values[(kind, key)] = int(value)
        return values
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


def compare(records, mapping, values):
    """Return (failures, checked_fields)."""
    failures = []
    checked = 0
    for name in sorted(records):
        record = records[name]
        spec = mapping.get(name)
        if spec is None:
            failures.append("%s %s: no C mapping in test/extern_layout.sh"
                            % (record["file"], name))
            continue
        declared = record["size"]
        actual = values[("size", name)]
        if declared != actual:
            failures.append("%s %s: yack size %d, C %s %d"
                            % (record["file"], name, declared,
                               spec["size_name"], actual))
        for field_name, field_type, offset in record["fields"]:
            member = spec["fields"].get(field_name)
            if member is None:
                failures.append(
                    "%s %s.%s: no C mapping in test/extern_layout.sh"
                    % (record["file"], name, field_name))
                continue
            checked += 1
            c_name = member[2]
            c_offset = values[("offset", "%s.%s" % (name, field_name))]
            c_size = values[("fieldsize", "%s.%s" % (name, field_name))]
            yack_size = type_size(field_type, records)
            if offset != c_offset or yack_size != c_size:
                failures.append(
                    "%s %s.%s: yack %s (%d bytes) @%d, C %s (%d bytes) @%d"
                    % (record["file"], name, field_name, field_type,
                       yack_size, offset, c_name, c_size, c_offset))
    return failures, checked


def check(records, mapping):
    return compare(records, mapping, run_c(records, mapping))


def self_test():
    """A fake record with a wrong offset must be reported as a mismatch."""
    records = {
        "bad": {
            "file": "self-test",
            "size": 16,
            "fields": [("x", "int32", 2)],
        }
    }
    mapping = {
        "bad": {
            "size_expr": "sizeof(struct timespec)",
            "size_name": "struct timespec",
            "fields": {
                "x": ("offsetof(struct timespec, tv_sec)",
                      "sizeof(((struct timespec *)0)->tv_sec)", "long"),
            },
        }
    }
    failures, _checked = check(records, mapping)
    expect = "self-test bad.x: yack int32 (4 bytes) @2, C long (8 bytes) @0"
    if expect not in failures:
        for failure in failures:
            sys.stderr.write("extern_layout: %s\n" % failure)
        sys.stderr.write(
            "extern_layout: self-test did not detect the injected mismatch: "
            "expected '%s'\n" % expect)
        raise SystemExit(1)


def main():
    if sys.argv[1] == "1":
        self_test()
        print("ok   extern_layout self-test (wrong offset detected)")
        return

    failures, checked = check(parse_records(), STRUCTS)
    if failures:
        for failure in failures:
            sys.stderr.write("extern_layout: %s\n" % failure)
        raise SystemExit(1)

    # Trap check on every run: the comparator itself must still fail on a
    # wrong offset.
    self_test()
    print("ok   extern_layout (%d fields checked)" % checked)


main()
PY
