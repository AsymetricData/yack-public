#!/usr/bin/env node

import { spawn } from "node:child_process";
import { readdirSync, statSync } from "node:fs";
import { dirname, extname, join, resolve } from "node:path";

type Options = {
  entry: string;
  roots: string[];
  interval: number;
  debounce: number;
  yack: string;
  fmt: boolean;
  once: boolean;
  clear: boolean;
  color: boolean;
};

type RunResult = {
  status: number;
  output: string;
  durationMs: number;
};

const ANSI = {
  reset: "\u001b[0m",
  bold: "\u001b[1m",
  dim: "\u001b[2m",
  red: "\u001b[31m",
  green: "\u001b[32m",
  yellow: "\u001b[33m",
  blue: "\u001b[34m",
  magenta: "\u001b[35m",
  cyan: "\u001b[36m",
};

function paint(enabled: boolean, code: string, text: string): string {
  if (!enabled) return text;
  return code + text + ANSI.reset;
}

function usage(): string {
  return [
    "yack-watch : watcher de lint Yack",
    "",
    "Usage :",
    "  node tools/yack-watch.ts <entry.yack> [options]",
    "  bun  tools/yack-watch.ts <entry.yack> [options]",
    "",
    "Options :",
    "  --root <dir>       dossier à surveiller (répétable)",
    "  --interval <ms>    période de polling (défaut : 400)",
    "  --debounce <ms>    délai après une modification (défaut : 150)",
    "  --yack <path>      binaire Yack (défaut : ./yack)",
    "  --fmt              vérifie aussi `yack fmt --check`",
    "  --once             un seul check puis quitte",
    "  --no-clear         n'efface pas le terminal",
    "  --no-color         désactive les couleurs",
    "  --help             affiche cette aide",
  ].join("\n");
}

function parseArgs(argv: string[]): Options | null {
  const options: Options = {
    entry: "",
    roots: [],
    interval: 400,
    debounce: 150,
    yack: resolve("yack"),
    fmt: false,
    once: false,
    clear: true,
    color: process.stdout.isTTY === true,
  };

  let index = 0;
  while (index < argv.length) {
    const arg = argv[index];
    if (arg === "--help" || arg === "-h") {
      console.log(usage());
      return null;
    } else if (arg === "--root") {
      index += 1;
      if (index >= argv.length) throw new Error("--root attend un dossier");
      options.roots.push(resolve(argv[index]));
    } else if (arg === "--interval") {
      index += 1;
      options.interval = Number(argv[index]);
    } else if (arg === "--debounce") {
      index += 1;
      options.debounce = Number(argv[index]);
    } else if (arg === "--yack") {
      index += 1;
      options.yack = resolve(argv[index]);
    } else if (arg === "--fmt") {
      options.fmt = true;
    } else if (arg === "--once") {
      options.once = true;
    } else if (arg === "--no-clear") {
      options.clear = false;
    } else if (arg === "--no-color") {
      options.color = false;
    } else if (arg.startsWith("-")) {
      throw new Error("option inconnue : " + arg);
    } else if (options.entry === "") {
      options.entry = resolve(arg);
    } else {
      throw new Error("fichier d'entrée déjà fourni : " + options.entry);
    }
    index += 1;
  }

  if (options.entry === "") {
    throw new Error("fichier d'entrée manquant");
  }
  if (!Number.isFinite(options.interval) || options.interval < 50) {
    throw new Error("--interval doit être >= 50");
  }
  if (!Number.isFinite(options.debounce) || options.debounce < 0) {
    throw new Error("--debounce doit être >= 0");
  }

  if (options.roots.length === 0) {
    options.roots.push(findPackageRoot(dirname(options.entry)));
  }

  return options;
}

function exists(path: string): boolean {
  try {
    statSync(path);
    return true;
  } catch {
    return false;
  }
}

function findPackageRoot(start: string): string {
  let current = start;
  while (true) {
    if (exists(join(current, "yack.toml"))) return current;
    const parent = dirname(current);
    if (parent === current) return start;
    current = parent;
  }
}

const SKIP_DIRECTORIES = new Set([
  ".git",
  ".trail",
  ".commandcode",
  ".yack",
  "node_modules",
  "target",
  "build",
  "dist",
]);

function isWatchedFile(path: string): boolean {
  const extension = extname(path);
  return extension === ".yack" || path.endsWith("yack.toml");
}

function scanFiles(root: string, output: Map<string, number>): void {
  let entries;
  try {
    entries = readdirSync(root, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const path = join(root, entry.name);
    if (entry.isDirectory()) {
      if (!SKIP_DIRECTORIES.has(entry.name)) scanFiles(path, output);
    } else if (entry.isFile() && isWatchedFile(path)) {
      try {
        output.set(path, statSync(path).mtimeMs);
      } catch {

      }
    }
  }
}

function snapshot(roots: string[]): Map<string, number> {
  const files = new Map<string, number>();
  for (const root of roots) scanFiles(root, files);
  return files;
}

function changedFiles(
  previous: Map<string, number>,
  current: Map<string, number>,
): string[] {
  const changed: string[] = [];
  for (const [path, mtime] of current) {
    if (previous.get(path) !== mtime) changed.push(path);
  }
  for (const path of previous.keys()) {
    if (!current.has(path)) changed.push(path);
  }
  changed.sort();
  return changed;
}

function run(command: string, args: string[], cwd: string): Promise<RunResult> {
  return new Promise((resolvePromise) => {
    const started = Date.now();
    const child = spawn(command, args, {
      cwd,
      stdio: ["ignore", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk: Buffer) => {
      stdout += chunk.toString("utf8");
    });
    child.stderr.on("data", (chunk: Buffer) => {
      stderr += chunk.toString("utf8");
    });
    child.on("error", (error: Error) => {
      resolvePromise({
        status: 127,
        output: error.message,
        durationMs: Date.now() - started,
      });
    });
    child.on("close", (status: number | null) => {
      resolvePromise({
        status: status === null ? 1 : status,
        output: (stdout + stderr).replace(/\s+$/, ""),
        durationMs: Date.now() - started,
      });
    });
  });
}

function countDiagnostics(output: string): number {
  const matches = output.match(/\[E\d{4}\]/g);
  return matches === null ? 0 : matches.length;
}

function timestamp(): string {
  const now = new Date();
  const pad = (value: number) => String(value).padStart(2, "0");
  return (
    pad(now.getHours()) +
    ":" +
    pad(now.getMinutes()) +
    ":" +
    pad(now.getSeconds())
  );
}

function clearScreen(enabled: boolean): void {
  if (enabled && process.stdout.isTTY === true) {
    process.stdout.write("\u001b[2J\u001b[H");
  }
}

function relativeTo(base: string, path: string): string {
  const prefix = base.endsWith("/") ? base : base + "/";
  if (path.startsWith(prefix)) return path.slice(prefix.length);
  return path;
}

function renderHeader(options: Options, roots: string[]): void {
  const title = paint(options.color, ANSI.bold + ANSI.cyan, "yack watch");
  console.log(title + paint(options.color, ANSI.dim, "  " + timestamp()));
  console.log(
    paint(options.color, ANSI.dim, "entry  ") +
      relativeTo(process.cwd(), options.entry),
  );
  console.log(
    paint(options.color, ANSI.dim, "watch  ") +
      roots.map((root) => relativeTo(process.cwd(), root)).join(", "),
  );
  console.log(
    paint(options.color, ANSI.dim, "yack   ") + options.yack,
  );
  console.log("");
}

async function checkOnce(
  options: Options,
  roots: string[],
  changed: string[],
  waiting: boolean,
): Promise<number> {
  clearScreen(options.clear);
  renderHeader(options, roots);

  const check = await run(options.yack, ["check", options.entry], process.cwd());
  const errors = countDiagnostics(check.output);

  if (check.status === 0 && check.output === "") {
    console.log(
      paint(options.color, ANSI.bold + ANSI.green, "OK") +
        paint(
          options.color,
          ANSI.dim,
          "  " + check.durationMs + " ms  aucune erreur",
        ),
    );
  } else {
    const label =
      check.status === 0
        ? paint(options.color, ANSI.bold + ANSI.green, "OK")
        : paint(options.color, ANSI.bold + ANSI.red, "ERREURS");
    console.log(
      label +
        paint(
          options.color,
          ANSI.dim,
          "  " + check.durationMs + " ms  " + errors + " diagnostic(s)",
        ),
    );
    if (check.output !== "") console.log(check.output);
  }

  if (options.fmt) {
    const yackFiles = changed.filter((path) => extname(path) === ".yack");
    let formatOutput = "";
    for (const file of yackFiles) {
      const format = await run(
        options.yack,
        ["fmt", "--check", file],
        process.cwd(),
      );
      if (format.status !== 0 && format.output !== "") {
        formatOutput += format.output + "\n";
      }
    }
    if (formatOutput !== "") {
      console.log("");
      console.log(paint(options.color, ANSI.bold + ANSI.yellow, "FORMAT"));
      console.log(formatOutput.replace(/\s+$/, ""));
    }
  }

  if (waiting) {
    console.log("");
    console.log(
      paint(
        options.color,
        ANSI.dim,
        "en attente de modifications… (Ctrl-C pour quitter)",
      ),
    );
  }

  return check.status === 0 ? 0 : 1;
}

async function main(): Promise<void> {
  let options: Options | null;
  try {
    options = parseArgs(process.argv.slice(2));
  } catch (error) {
    console.error("yack-watch: " + (error as Error).message);
    console.error("");
    console.error(usage());
    process.exit(2);
    return;
  }
  if (options === null) return;

  const roots = options.roots;
  const cwd = process.cwd();

  if (!exists(options.entry)) {
    console.error("yack-watch: fichier d'entrée introuvable : " + options.entry);
    process.exit(2);
  }

  if (options.once) {
    const status = await checkOnce(options, roots, [options.entry], false);
    process.exit(status);
  }

  let previous = snapshot(roots);
  clearScreen(options.clear);
  renderHeader(options, roots);
  console.log(
    paint(options.color, ANSI.dim, "surveillance démarrée, Ctrl-C pour quitter"),
  );

  let pending: NodeJS.Timeout | null = null;
  let running = false;
  let queued = false;

  const schedule = () => {
    if (pending !== null) clearTimeout(pending);
    pending = setTimeout(() => {
      pending = null;
      void trigger();
    }, options.debounce);
  };

  const trigger = async () => {
    if (running) {
      queued = true;
      return;
    }
    running = true;
    const current = snapshot(roots);
    const changed = changedFiles(previous, current);
    previous = current;
    if (changed.length === 0) {
      running = false;
      return;
    }
    await checkOnce(options, roots, changed, true);
    running = false;
    if (queued) {
      queued = false;
      schedule();
    }
  };

  setInterval(() => {
    const current = snapshot(roots);
    if (changedFiles(previous, current).length > 0) schedule();
  }, options.interval);

  process.on("SIGINT", () => {
    clearScreen(false);
    console.log("\nyack-watch: arrêt");
    process.exit(0);
  });
}

void main();
