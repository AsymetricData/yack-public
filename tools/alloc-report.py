#!/usr/bin/env python3
"""Summarise a `yack --trace-alloc` run by allocation site.

Usage:
    tools/alloc-report.py --map alloc-map.json trace.log
    tools/alloc-report.py trace.log

The trace format is the one produced by `--trace-alloc`:
    [yack] alloc site=0 ptr=0x... size=3
    [yack] free ptr=0x...
    [yack] realloc 0x... -> 0x... size 64
"""

import argparse
import json
import re
import sys
from collections import defaultdict

ALLOC = re.compile(r"alloc site=(\d+) ptr=(0x[0-9a-fA-F]+) size=(\d+)")
FREE = re.compile(r"free ptr=(0x[0-9a-fA-F]+)")
REALLOC = re.compile(r"realloc (0x[0-9a-fA-F]+|\(nil\)) -> (0x[0-9a-fA-F]+) size (\d+)")


def load_map(path):
    if not path:
        return {}
    with open(path, "r", encoding="utf-8") as handle:
        data = json.load(handle)
    return {int(site["id"]): site for site in data.get("sites", [])}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", help="stderr log from a --trace-alloc run")
    parser.add_argument("--map", help="JSON output from `yack alloc-map --json`")
    parser.add_argument("--top", type=int, default=0, help="show only the N largest live sites")
    args = parser.parse_args()

    sites = load_map(args.map)
    live = {}
    allocs = defaultdict(int)
    frees = defaultdict(int)
    bytes_allocated = defaultdict(int)
    bytes_freed = defaultdict(int)
    ever_freed = set()
    double_frees = 0
    unknown_frees = 0

    def account_free(pointer):
        nonlocal double_frees, unknown_frees
        if pointer in live:
            site, size = live.pop(pointer)
            frees[site] += 1
            bytes_freed[site] += size
            ever_freed.add(pointer)
        elif pointer in ever_freed:
            double_frees += 1
        else:
            unknown_frees += 1

    with open(args.trace, "r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            match = ALLOC.search(line)
            if match:
                site = int(match.group(1))
                pointer = int(match.group(2), 16)
                size = int(match.group(3))
                live[pointer] = (site, size)
                allocs[site] += 1
                bytes_allocated[site] += size
                continue

            match = REALLOC.search(line)
            if match:
                old, new, size = match.groups()
                size = int(size)
                old_ptr = None if old == "(nil)" else int(old, 16)
                new_ptr = int(new, 16)
                if old_ptr is not None and old_ptr in live:
                    site, _ = live[old_ptr]
                    account_free(old_ptr)
                else:
                    site = 0
                live[new_ptr] = (site, size)
                allocs[site] += 1
                bytes_allocated[site] += size
                continue

            match = FREE.search(line)
            if match:
                account_free(int(match.group(1), 16))

    rows = []
    for pointer, (site, size) in live.items():
        info = sites.get(site, {})
        rows.append((site, info, size))
    per_site = defaultdict(lambda: {"live": 0, "live_bytes": 0})
    for site, info, size in rows:
        per_site[site]["live"] += 1
        per_site[site]["live_bytes"] += size

    def label(site):
        info = sites.get(site)
        if not info:
            return f"site {site}"
        return (f"{info.get('function', '?')} {info.get('line', '?')}:"
                f"{info.get('column', '?')}  [{info.get('scope', '?')}] "
                f"{info.get('kind', '?')}")

    output = []
    seen = set(sites.keys()) | set(allocs.keys()) | set(frees.keys())
    for site in seen:
        entry = per_site[site]
        output.append(
            (
                entry["live_bytes"],
                entry["live"],
                allocs[site],
                frees[site],
                bytes_allocated[site],
                bytes_freed[site],
                site,
            )
        )
    output.sort(reverse=True)
    if args.top > 0:
        output = output[: args.top]

    print(f"{'live B':>10} {'live':>6} {'alloc':>8} {'free':>8} {'alloc B':>10} {'free B':>10}  site")
    for live_bytes, live_count, alloc_count, free_count, allocated, freed, site in output:
        print(f"{live_bytes:10d} {live_count:6d} {alloc_count:8d} {free_count:8d} "
              f"{allocated:10d} {freed:10d}  {label(site)}")

    total_live = sum(size for _, size in live.values())
    print()
    print(f"live pointers: {len(live)}")
    print(f"live bytes:    {total_live}")
    if unknown_frees:
        print(f"unknown frees: {unknown_frees}")
    if double_frees:
        print(f"double frees:  {double_frees}")
    if total_live:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
