#!/usr/bin/env node












import { execFileSync } from "node:child_process";
import { readdirSync, readFileSync, writeFileSync, mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const dropsDir = join(root, "test", "nll", "drops");
const refDir = join(root, "test", "nll", "ref");
const yack = process.env.YACK ?? join(root, "yack");

const args = process.argv.slice(2);
const update = args.includes("--update");
const summary = args.includes("--summary");
const files = args.filter((arg) => !arg.startsWith("--"));
const fixtures =
  files.length > 0 ? files : readdirSync(dropsDir).filter((f) => f.endsWith(".yack")).sort().map((f) => join(dropsDir, f));

function run(tool, file) {
  try {
    return execFileSync(yack, [tool, file], { cwd: root, encoding: "utf8" });
  } catch (error) {
    process.stderr.write(String(error.stdout ?? "") + String(error.stderr ?? ""));
    process.exit(1);
  }
}

function programFunctions(file) {
  const names = new Set(["yack_main"]);
  for (const line of run("parse", file).split("\n")) {
    const match = /^fun ([^\s/]+)\//.exec(line);
    if (match) names.add(`yack_${match[1]}`);
  }
  return names;
}




function defineBlocks(ir) {
  const blocks = [];
  let current = null;
  for (const line of ir.split("\n")) {
    if (line.startsWith("define ")) current = [line];
    else if (current) {
      current.push(line);
      if (line === "}") {
        const name = /@([^\s(]+)\(/.exec(current[0])?.[1];
        blocks.push({ name, text: current.join("\n") });
        current = null;
      }
    }
  }
  return blocks;
}

function dropSummary(functions) {
  return functions.map((fn) => {
    const calls = fn.text.split("\n").filter((line) => /call void @yack_drop_/.test(line));
    return { name: fn.name, count: calls.length };
  });
}

mkdirSync(refDir, { recursive: true });
let failures = 0;
const summaries = [];
const buildDir = mkdtempSync(join(tmpdir(), "yack-nll-ir-"));
process.on("exit", () => rmSync(buildDir, { recursive: true, force: true }));






function runFixture(name, file) {
  const binary = join(buildDir, name);
  try {
    execFileSync(yack, ["build", "--memory=heap", "--arena-debug=poison", file, binary], {
      cwd: root,
      stdio: "pipe",
    });
    execFileSync(binary, [], {
      cwd: root,
      stdio: "pipe",
      env: { ...process.env, ASAN_OPTIONS: "detect_leaks=1" },
    });
  } catch (error) {
    process.stderr.write(String(error.stdout ?? "") + String(error.stderr ?? ""));
    console.error(`FAIL ${name}: drop fixture did not run cleanly`);
    failures += 1;
  }
}

for (const file of fixtures) {
  const name = basename(file, ".yack");
  const wanted = programFunctions(file);
  const functions = defineBlocks(run("llvm", file)).filter((fn) => wanted.has(fn.name));
  const text = functions.map((fn) => fn.text).join("\n\n") + "\n";
  const refPath = join(refDir, `${name}.ll`);

  summaries.push({ name, functions: dropSummary(functions) });
  runFixture(name, file);

  if (update) {
    writeFileSync(refPath, text);
    console.log(`updated ${refPath}`);
    continue;
  }

  let reference;
  try {
    reference = readFileSync(refPath, "utf8");
  } catch {
    console.error(`FAIL ${name}: missing ${refPath} (run with --update)`);
    failures += 1;
    continue;
  }
  if (reference === text) {
    console.log(`ok   drop_plan_${name}`);
  } else {
    failures += 1;
    console.error(`FAIL ${name}: LLVM reference differs`);
    const expected = reference.split("\n");
    const actual = text.split("\n");
    for (let i = 0; i < Math.max(expected.length, actual.length); i += 1) {
      if (expected[i] !== actual[i]) {
        console.error(`     line ${i + 1}:`);
        console.error(`       expected: ${expected[i] ?? "<eof>"}`);
        console.error(`       actual:   ${actual[i] ?? "<eof>"}`);
        break;
      }
    }
  }
}

if (summary) {
  for (const entry of summaries) {
    const parts = entry.functions.map((fn) => `${fn.name}=${fn.count}`);
    console.log(`drops ${entry.name}: ${parts.join(" ")}`);
  }
}

if (failures > 0) {
  console.error(`\n${failures} LLVM reference check(s) failed`);
  process.exit(1);
}
