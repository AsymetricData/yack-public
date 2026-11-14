#!/usr/bin/env node










import { spawn, spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, join, relative, resolve } from "node:path";
import { readdirSync, readFileSync } from "node:fs";

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const yack = join(root, "yack");

const directories = [
  "test/reject",
  "test/linear",
  "test/guard",
  "test/ssa/opt",
  "examples",
];
const files = directories.flatMap((directory) =>
  readdirSync(join(root, directory))
    .filter((name) => name.endsWith(".yack"))
    .map((name) => join(root, directory, name)),
);

function cliDiagnostics(file) {
  const run = spawnSync(yack, ["check", file], { cwd: root, encoding: "utf8" });
  const text = `${run.stdout}${run.stderr}`;
  const lines = text.split("\n");
  const found = [];
  for (let i = 0; i < lines.length; i += 1) {
    const header = /^(error|warning)\[(\w+)\]/.exec(lines[i]);
    if (!header) continue;
    const location = /-->\s+(.*):(\d+):(\d+)/.exec(lines[i + 1] || "");
    if (!location) continue;
    if (resolve(root, location[1]) !== file) continue;
    found.push(`${header[1]} ${header[2]} ${location[2]}:${location[3]}`);
  }
  return found.sort();
}

class Client {
  constructor() {
    this.child = spawn(yack, ["lsp"], { cwd: root, stdio: ["pipe", "pipe", "pipe"] });
    this.buffer = Buffer.alloc(0);
    this.messages = [];
    this.nextId = 1;
    this.child.stdout.on("data", (chunk) => this.onData(chunk));
    this.child.stderr.on("data", () => {});
  }

  onData(chunk) {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    for (;;) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd < 0) return;
      const header = this.buffer.subarray(0, headerEnd).toString("utf8");
      const length = Number(/Content-Length:\s*(\d+)/i.exec(header)[1]);
      if (this.buffer.length < headerEnd + 4 + length) return;
      const body = this.buffer.subarray(headerEnd + 4, headerEnd + 4 + length);
      this.buffer = this.buffer.subarray(headerEnd + 4 + length);
      this.messages.push(JSON.parse(body.toString("utf8")));
    }
  }

  send(message) {
    const body = JSON.stringify(message);
    this.child.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
  }

  async wait(predicate, label, timeoutMs = 20000) {
    const started = Date.now();
    for (;;) {
      const index = this.messages.findIndex(predicate);
      if (index >= 0) return this.messages.splice(index, 1)[0];
      if (Date.now() - started > timeoutMs) throw new Error(`timeout: ${label}`);
      await new Promise((r) => setTimeout(r, 10));
    }
  }

  async request(method, params) {
    const id = this.nextId++;
    this.send({ jsonrpc: "2.0", id, method, params });
    return this.wait((m) => m.id === id, method);
  }

  async diagnostics(file) {
    const uri = `file://${file}`;
    this.send({
      jsonrpc: "2.0",
      method: "textDocument/didOpen",
      params: {
        textDocument: { uri, languageId: "yack", version: 1, text: readFileSync(file, "utf8") },
      },
    });
    const message = await this.wait(
      (m) => m.method === "textDocument/publishDiagnostics" && m.params.uri === uri,
      uri,
    );
    this.send({
      jsonrpc: "2.0",
      method: "textDocument/didClose",
      params: { textDocument: { uri } },
    });
    return message.params.diagnostics
      .map(
        (d) =>
          `${d.severity === 1 ? "error" : "warning"} ${d.code} ` +
          `${d.range.start.line + 1}:${d.range.start.character + 1}`,
      )
      .sort();
  }
}

const client = new Client();
let failures = 0;
try {
  await client.request("initialize", {
    processId: process.pid,
    rootUri: `file://${root}`,
    capabilities: {},
  });
  client.send({ jsonrpc: "2.0", method: "initialized", params: {} });
  for (const file of files) {
    const name = relative(root, file);
    const cli = cliDiagnostics(file);
    const lsp = await client.diagnostics(file);
    if (JSON.stringify(cli) === JSON.stringify(lsp)) {
      console.log(`ok   parity/${name}`);
    } else {
      failures += 1;
      console.log(`FAIL parity/${name}`);
      console.log(`     check: ${JSON.stringify(cli)}`);
      console.log(`     lsp:   ${JSON.stringify(lsp)}`);
    }
  }
} finally {
  client.child.kill();
}
if (failures > 0) {
  console.log(`lsp parity: ${failures} file(s) differ`);
  process.exit(1);
}
console.log(`lsp parity: ${files.length} files agree`);
