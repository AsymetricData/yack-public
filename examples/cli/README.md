# CLI examples

Four small command-line programs, built only with the `yack/*` standard
library. The guide on the site is `site/cli.html`; `make test-cli` builds and
runs them.

| File | What it shows |
|---|---|
| `src/ls.yack` | flags, `-al` clusters, `args.parse`, sizes, exit status 1 |
| `src/cat.yack` | `io.read_file` / `io.read_stdin`, raw output, errors on stderr |
| `src/wc.yack` | byte-level counting on a borrowed string, stdin fallback |
| `src/main.yack` | `ansi` colours, `tui.box`, `menu`, `progress`, `status_bar` |

```sh
cd examples/cli
../../yack build src/ls.yack ls
../../yack build src/cat.yack cat
../../yack build src/wc.yack wc
../../yack build src/main.yack cli-demo

./ls -al README.md src/ls.yack
./wc src/*.yack
./cat README.md
./cli-demo -v -o out.txt a.yack b.yack
```

`ls -al` is real: `-a` and `-l` are combined by the cluster support in
`yack/args`. What the kernel does not expose yet:

- no `readdir`: `ls` lists the paths named on the command line, it cannot walk
  a directory;
- no `stat`: `-l` uses `io.read_file`'s length as the size, so directories read
  as empty and there is no mode, owner or date;
- no raw mode or `io.read_line`: a real interactive TUI loop waits for
  `termios`; `tui` renders a screen today, and `io.read_stdin` reads a pipe.

Everything else (`args`, `cli`, `ansi`, `tui`, `os`, `io`, `sort`, `path`) is in
`lib/yack`. The missing primitives are listed in `docs/stdlib.md` §11.
