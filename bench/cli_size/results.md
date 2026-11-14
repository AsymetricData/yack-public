# CLI size: one program, four toolchains

Date: 2026-11-14

## What is compared

Exactly the same contract in each language: an optional `--name` VALUE with
default `Yack`, printing `Hello from <name> !`.

Every version uses its language's idiomatic argument parser, so nobody is
measured while doing more work than the others:

- `main.yack` : `yack/args` (declarative `args.spec`, usage generated from it),
  with `yack/option` for the default;
- `main.go` : `flag` from the standard library;
- `main.ts` : `process.argv` by hand: TypeScript ships no argument parser;
- `main.rs` : `clap` with derive.

Source size, non-blank and non-comment lines:

| language | lines |
|---|---:|
| Yack | 16 |
| Go | 10 |
| TypeScript | 4 |
| Rust | 8 |

## Method

- `yack build`, then `yack build --trim`;
- `go build`, then `go build -trimpath -ldflags="-s -w"`;
- `cargo build --release`, then `strip` on a copy;
- sizes are `wc -c` on the produced file, no post-build stripping beyond the
  flags shown;
- no timing: size only, because startup time depends on the loader and the page
  cache at least as much as on the runtime.

Reproduce with `./bench/cli_size/run.sh`. It rebuilds every variant and runs
each binary twice (`--name Ada` and no flag) to check the output matches.

## Results

Linux x86-64, `clang` 23.1.4, `go` 1.27.3, `rustc` 1.97.0 with `clap` 4.6,
Node 24.

| build | bytes | KiB | vs `yack build` | run-time needs |
|---|---:|---:|---:|---|
| `yack build` | 130,280 | 127.2 | 1.0× | libc, libm, libsqlite3 |
| `yack build --trim` | 92,680 | 90.5 | 0.7× | libc, libm, libsqlite3 |
| `go build` | 2,471,820 | 2413.9 | 19.0× | nothing (static) |
| `go build -s -w` | 1,589,408 | 1552.2 | 12.2× | nothing (static) |
| `cargo build --release` (`clap`) | 1,042,640 | 1018.2 | 8.0× | libc, libgcc_s |
| `cargo build --release` + `strip` | 800,128 | 781.4 | 6.1× | libc, libgcc_s |
| TypeScript script | 187 | 0.2 | - | Node 24 (121 MiB) |

## Reading the numbers

Yack links against the system libc and loads `libsqlite3.so` (1.6 MiB here)
because the runtime exposes SQLite; a Go binary embeds its own runtime and libc
instead, and Rust compiles `clap` in.

This compares the artefact a user ships, not speed, and not memory use at run
time. The point is not that Yack beats a scratch script: it is that a checked,
native binary with a real argument parser lands in the same size class as a
stripped Rust one, with ownership checked at compile time and no GC.
