#!/usr/bin/env node

















import net from "node:net";
import fs from "node:fs";
import path from "node:path";

if (Number(process.versions.node.split(".")[0]) < 18) {
  console.error("http_fuzz: Node.js >= 18 is required");
  process.exit(2);
}

const HOST = "127.0.0.1";
const CLK_TCK = 100;
const HEALTH_TIMEOUT_MS = 2000;
const RECOVERY_LIMIT_MS = 5000;
const IDLE_BUSY_SECONDS = 0.5;

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, Math.max(0, ms)));

function mulberry32(seed) {
  let state = seed >>> 0;
  return function next() {
    state = (state + 0x6d2b79f5) >>> 0;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

function fnv1a(text) {
  let hash = 0x811c9dc5;
  for (let index = 0; index < text.length; index++) {
    hash ^= text.charCodeAt(index);
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return hash >>> 0;
}

const PROFILES = {
  quick: {
    slowloris: { connections: 300, holdMs: 5000 },
    slowBody: { connections: 16, holdMs: 5000 },
    slowRead: { connections: 8, requests: 4000 },
    churn: 2000,
    mutations: 2000,
  },
  aggressive: {
    slowloris: { connections: 1100, holdMs: 20000 },
    slowBody: { connections: 64, holdMs: 20000 },
    slowRead: { connections: 32, requests: 8000 },
    churn: 20000,
    mutations: 50000,
  },
};

const SCENARIO_NAMES = [
  "slowloris",
  "slow_body",
  "slow_read",
  "oversized",
  "malformed",
  "pipeline_flood",
  "churn",
  "mutate",
];

function failUsage(message) {
  console.error(`http_fuzz: ${message}`);
  console.error(
    "usage: node test/http_fuzz.mjs --port N [--pid PID] [--seed S] " +
      "[--profile quick|aggressive] [--only name,name] [--json out.json]",
  );
  process.exit(2);
}

function parseArgs(argv) {
  const options = {
    port: null,
    pid: null,
    seed: 1,
    profile: "quick",
    only: null,
    json: null,
    out: null,
  };
  for (let index = 0; index < argv.length; index++) {
    const argument = argv[index];
    const value = () => {
      index++;
      if (index >= argv.length) failUsage(`missing value for ${argument}`);
      return argv[index];
    };
    const integer = (text, name) => {
      if (!/^-?\d+$/.test(text)) failUsage(`invalid ${name}: ${text}`);
      const parsed = Number(text);
      if (!Number.isSafeInteger(parsed)) failUsage(`invalid ${name}: ${text}`);
      return parsed;
    };
    if (argument === "--port") options.port = integer(value(), "port");
    else if (argument === "--pid") options.pid = integer(value(), "pid");
    else if (argument === "--seed") options.seed = integer(value(), "seed") >>> 0;
    else if (argument === "--profile") options.profile = value();
    else if (argument === "--only") {
      options.only = value()
        .split(",")
        .map((name) => name.trim())
        .filter(Boolean);
    } else if (argument === "--json") options.json = value();
    else if (argument === "--out") options.out = value();
    else if (argument === "--host") failUsage("only 127.0.0.1 is attacked; --host is not supported");
    else failUsage(`unknown argument: ${argument}`);
  }
  if (options.port === null || options.port < 1 || options.port > 65535) {
    failUsage("--port is required and must be between 1 and 65535");
  }
  if (!Object.prototype.hasOwnProperty.call(PROFILES, options.profile)) {
    failUsage(`unknown profile: ${options.profile} (use quick or aggressive)`);
  }
  if (options.pid !== null && options.pid < 1) failUsage("--pid must be a positive integer");
  if (options.only !== null) {
    for (const name of options.only) {
      if (!SCENARIO_NAMES.includes(name)) {
        failUsage(`unknown scenario in --only: ${name} (known: ${SCENARIO_NAMES.join(",")})`);
      }
    }
  }
  return options;
}

const opts = parseArgs(process.argv.slice(2));
const settings = PROFILES[opts.profile];
const artifactsDir =
  opts.out ??
  (opts.json
    ? path.dirname(path.resolve(opts.json))
    : path.join(process.env.TMPDIR ?? "/tmp", "yack-http-fuzz-artifacts"));

const rngFor = (name) => mulberry32((opts.seed ^ fnv1a(name)) >>> 0);






const openSockets = new Set();

function track(socket) {
  openSockets.add(socket);
  socket.on("error", () => {});
  socket.once("close", () => openSockets.delete(socket));
  return socket;
}

function connect(timeoutMs = 2000) {
  return new Promise((resolve, reject) => {
    const socket = track(net.createConnection({ host: HOST, port: opts.port }));
    let settled = false;
    const timer = setTimeout(() => {
      if (settled) return;
      settled = true;
      socket.destroy();
      reject(Object.assign(new Error("connect timeout"), { code: "ETIMEDOUT" }));
    }, timeoutMs);
    socket.once("connect", () => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      socket.setNoDelay(true);
      resolve(socket);
    });
    socket.once("error", (error) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      socket.destroy();
      reject(error);
    });
  });
}

async function closeAll() {
  for (const socket of openSockets) socket.destroy();
  for (let attempt = 0; attempt < 100 && openSockets.size > 0; attempt++) await sleep(10);
}

function safeWrite(socket, payload) {
  if (socket.destroyed) return false;
  try {
    socket.write(payload);
    return true;
  } catch {
    return false;
  }
}




function parseStatus(buffer) {
  const match = /^HTTP\/1\.[01] (\d{3})/.exec(buffer.toString("latin1"));
  return match ? Number(match[1]) : null;
}

function completeResponse(buffer) {
  const end = buffer.indexOf("\r\n\r\n");
  if (end < 0) return null;
  const head = buffer.subarray(0, end).toString("latin1");
  const status = /^HTTP\/1\.[01] (\d{3})/.exec(head);
  if (!status) return null;
  const length = /content-length:\s*(\d+)/i.exec(head);
  const total = end + 4 + (length ? Number(length[1]) : 0);
  if (buffer.length < total) return null;
  return { status: Number(status[1]), total };
}

function bucketOf(kind, status) {
  if (kind === "tool_limit") return "tool_limit";
  if (kind === "status" && status !== null) {
    if (status >= 200 && status < 300) return String(status);
    if (status >= 300 && status < 400) return "3xx";
    if (status >= 400 && status < 500) return status === 404 || status === 405 ? String(status) : "4xx";
    if (status >= 500) return "5xx";
  }
  return kind;
}

function probe(payload, timeoutMs = 1000) {
  const data = Buffer.isBuffer(payload) ? payload : Buffer.from(payload, "latin1");
  return new Promise((resolve) => {
    let buffer = Buffer.alloc(0);
    let settled = false;
    let timer = null;
    const socket = track(net.createConnection({ host: HOST, port: opts.port }));
    const finish = (kind, error = null) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      const status = parseStatus(buffer);
      const result = {
        kind,
        status,
        bucket: bucketOf(kind === "closed" && status !== null ? "status" : kind, status),
        bytes: buffer.length,
        error,
      };
      socket.destroy();
      resolve(result);
    };
    timer = setTimeout(() => finish("timeout"), timeoutMs);
    socket.once("connect", () => safeWrite(socket, data));
    socket.on("data", (chunk) => {
      buffer = Buffer.concat([buffer, chunk]);
      const response = completeResponse(buffer);
      if (response !== null) finish("status");
    });
    socket.once("error", (error) => {
      const code = error.code ?? error.message;
      const kind = code === "ECONNRESET" ? "reset" : code === "EMFILE" || code === "ENFILE" ? "tool_limit" : "error";
      finish(kind, code);
    });
    socket.once("close", () => finish("closed"));
  });
}




function serverAlive() {
  if (opts.pid === null) return null;
  try {
    fs.statSync(`/proc/${opts.pid}`);
    return true;
  } catch {
    return false;
  }
}

function rssKb() {
  if (opts.pid === null) return null;
  try {
    const status = fs.readFileSync(`/proc/${opts.pid}/status`, "utf8");
    const match = /^VmRSS:\s+(\d+) kB/m.exec(status);
    return match ? Number(match[1]) : null;
  } catch {
    return null;
  }
}

function fdCount() {
  if (opts.pid === null) return null;
  try {
    return fs.readdirSync(`/proc/${opts.pid}/fd`).length;
  } catch {
    return null;
  }
}

function cpuTicks() {
  if (opts.pid === null) return null;
  try {
    const stat = fs.readFileSync(`/proc/${opts.pid}/stat`, "utf8");
    const close = stat.lastIndexOf(")");
    if (close < 0) return null;


    const fields = stat.slice(close + 2).trim().split(/\s+/);
    return Number(fields[11]) + Number(fields[12]);
  } catch {
    return null;
  }
}

function healthCheck(timeoutMs = HEALTH_TIMEOUT_MS) {
  const started = performance.now();
  return new Promise((resolve) => {
    let buffer = "";
    let settled = false;
    let timer = null;
    const socket = track(net.createConnection({ host: HOST, port: opts.port }));
    const finish = (ok, detail) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      const elapsed = Math.round(performance.now() - started);
      socket.destroy();
      resolve({ ok, ms: elapsed, detail });
    };
    timer = setTimeout(() => finish(false, "timeout"), timeoutMs);
    socket.once("connect", () => safeWrite(socket, "GET / HTTP/1.1\r\nHost: x\r\n\r\n"));
    socket.on("data", (chunk) => {
      if (buffer.length < 8192) buffer += chunk.toString("latin1");
      const status = /^HTTP\/1\.[01] \d{3}[^\r\n]*\r\n/.exec(buffer);
      if (status) finish(true, status[0].trim());
    });
    socket.once("error", (error) => finish(false, error.code ?? error.message));
    socket.once("close", () => finish(false, "closed"));
  });
}

async function recoverHealth(limitMs = RECOVERY_LIMIT_MS) {
  const started = performance.now();
  let last = { ok: false, detail: "not attempted" };
  for (;;) {
    const remaining = limitMs - (performance.now() - started);
    if (remaining <= 0) break;
    last = await healthCheck(Math.min(HEALTH_TIMEOUT_MS, remaining));
    if (last.ok) return { ok: true, ms: Math.round(performance.now() - started), detail: last.detail, last };
    const pause = Math.min(100, limitMs - (performance.now() - started));
    if (pause <= 0) break;
    await sleep(pause);
  }
  return { ok: false, ms: null, detail: last.detail, last };
}

async function idleCpu() {
  if (opts.pid === null) return { percent: null, busy: false };
  const first = cpuTicks();
  if (first === null) return { percent: null, busy: false };
  await sleep(1000);
  const second = cpuTicks();
  if (second === null) return { percent: null, busy: false };
  const seconds = Math.max(0, second - first) / CLK_TCK;
  return { percent: Math.round(seconds * 1000) / 10, busy: seconds > IDLE_BUSY_SECONDS };
}




function bump(map, key, amount = 1) {
  map.set(key, (map.get(key) ?? 0) + amount);
}

function formatCounts(map) {
  if (map.size === 0) return "-";
  return [...map.entries()]
    .sort((left, right) => right[1] - left[1] || left[0].localeCompare(right[0]))
    .map(([key, value]) => `${key}=${value}`)
    .join(" ");
}

function formatDuring(during) {
  if (!during) return "-";
  if (during.ok) return `ok ${during.ms}ms`;
  return `fail ${during.detail}`;
}

function summaryOf(counters) {
  const status = [];
  let closed = 0;
  let reset = 0;
  let timeout = 0;
  let toolLimit = 0;
  let errors = 0;
  for (const [key, value] of counters) {
    if (/^\d{3}$/.test(key) || /^\dxx$/.test(key)) status.push(`${key}=${value}`);
    else if (key === "closed") closed = value;
    else if (key === "reset") reset = value;
    else if (key === "timeout") timeout = value;
    else if (key === "tool_limit") toolLimit = value;
    else errors += value;
  }
  const parts = [...status, `closed=${closed}`, `reset=${reset}`, `timeout=${timeout}`];
  if (errors > 0) parts.push(`error=${errors}`);
  if (toolLimit > 0) parts.push(`tool_limit=${toolLimit}`);
  return parts.join(" ");
}

function noteToolLimits(counters, scenario, prefix = "") {
  for (const code of ["EMFILE", "ENFILE", "EADDRNOTAVAIL"]) {
    if (counters.has(`${prefix}${code}`)) {
      warnings.push(`${scenario}: client resource limit ${code} (tool limit, not a server bug)`);
    }
  }
}

const COLUMNS = [
  ["scenario", 15],
  ["alive", 6],
  ["health_during", 20],
  ["recovery_ms", 11],
  ["rss_delta_kb", 12],
  ["fd_delta", 8],
  ["idle_cpu", 10],
  ["outcomes", 0],
];

function signed(value) {
  if (value === null) return "-";
  return value > 0 ? `+${value}` : String(value);
}

function formatRow(row) {
  const values = [
    row.name,
    row.alive === null ? "-" : row.alive ? "yes" : "NO",
    formatDuring(row.health_during),
    row.recovery_ms === null ? "-" : String(row.recovery_ms),
    signed(row.rss_delta_kb),
    signed(row.fd_delta),
    row.idle_percent === null ? "-" : row.idle_busy ? `BUSY ${row.idle_percent}%` : `${row.idle_percent}%`,
    row.outcomes,
  ];
  return COLUMNS.map(([, width], index) =>
    width > 0 ? String(values[index]).padEnd(width) : values[index],
  ).join("  ");
}

function printHeader() {
  console.log(COLUMNS.map(([name, width]) => (width > 0 ? name.padEnd(width) : name)).join("  "));
  console.log(COLUMNS.map(([, width]) => "-".repeat(width > 0 ? width : 8)).join("  "));
}

function saveArtifact(name, payload, meta) {
  fs.mkdirSync(artifactsDir, { recursive: true });
  const base = path.join(artifactsDir, `${name}-seed${opts.seed}-${Date.now()}`);
  const body = Buffer.isBuffer(payload) ? payload : Buffer.from(payload ?? "", "latin1");
  fs.writeFileSync(`${base}.bin`, body);
  const hex = body.toString("hex").replace(/(.{64})/g, "$1\n");
  fs.writeFileSync(`${base}.hex`, `${hex}\n`);
  fs.writeFileSync(
    `${base}.json`,
    `${JSON.stringify({ ...meta, bytes: body.length, artifact: base }, null, 2)}\n`,
  );
  return base;
}

async function mapLimit(items, limit, worker) {
  let next = 0;
  const runners = Array.from({ length: Math.min(limit, items.length) }, async () => {
    for (;;) {
      const index = next++;
      if (index >= items.length) return;
      await worker(items[index], index);
    }
  });
  await Promise.all(runners);
}




function oversizedCases() {
  const text = (value) => Buffer.from(value, "latin1");
  const cases = [];
  cases.push(["request_line_8k", text(`GET /${"A".repeat(8 * 1024)} HTTP/1.1\r\nHost: x\r\n\r\n`)]);
  cases.push(["request_line_64k", text(`GET /${"A".repeat(64 * 1024)} HTTP/1.1\r\nHost: x\r\n\r\n`)]);
  cases.push(["request_line_1m", text(`GET /${"A".repeat(1024 * 1024)} HTTP/1.1\r\nHost: x\r\n\r\n`)]);
  cases.push(["header_8k", text(`GET / HTTP/1.1\r\nHost: x\r\nX-Big: ${"B".repeat(8 * 1024)}\r\n\r\n`)]);
  cases.push([
    "headers_1000",
    text(
      `GET / HTTP/1.1\r\nHost: x\r\n${Array.from({ length: 1000 }, (_, index) => `X-H${index}: v`).join("\r\n")}\r\n\r\n`,
    ),
  ]);
  cases.push(["url_100k", text(`GET /${"u".repeat(100 * 1024)} HTTP/1.1\r\nHost: x\r\n\r\n`)]);
  cases.push([
    "body_over_content_length",
    text('POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\n{"items":[1,2,3]}'),
  ]);
  cases.push([
    "content_length_huge",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: 99999999999999999999\r\n\r\n{}"),
  ]);
  cases.push([
    "content_length_negative",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: -1\r\n\r\n{}"),
  ]);
  cases.push([
    "content_length_text",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: abc\r\n\r\n{}"),
  ]);
  cases.push([
    "content_length_duplicate",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\nContent-Length: 10\r\n\r\n0123456789"),
  ]);
  cases.push([
    "transfer_encoding_invalid",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\nzzz\r\nhello\r\n0\r\n\r\n"),
  ]);
  cases.push([
    "transfer_encoding_and_content_length",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\nContent-Length: 5\r\n\r\nhello"),
  ]);
  cases.push([
    "content_length_zero_with_body",
    text("POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: 0\r\n\r\nhello"),
  ]);
  return cases;
}

function malformedCases() {
  const text = (value) => Buffer.from(value, "latin1");
  const bytes = (...values) => Buffer.from(values);
  const cases = [];
  cases.push(["bare_lf", text("GET / HTTP/1.1\nHost: x\n\n")]);
  cases.push([
    "nul_bytes",
    Buffer.concat([text("GET / HTTP/1.1\r\nHost: x\r\nX-Nul: "), bytes(0, 0, 0, 0), text("\r\n\r\n")]),
  ]);
  cases.push([
    "high_bytes_method",
    Buffer.concat([bytes(0x80, 0x81, 0x82, 0x83), text(" / HTTP/1.1\r\nHost: x\r\n\r\n")]),
  ]);
  cases.push([
    "high_bytes_path",
    Buffer.concat([text("GET /"), bytes(0xff, 0xfe, 0xfd), text(" HTTP/1.1\r\nHost: x\r\n\r\n")]),
  ]);
  cases.push([
    "high_bytes_headers",
    Buffer.concat([
      text("GET / HTTP/1.1\r\nHost: x\r\nX-High: "),
      bytes(0x80, 0x8f, 0xf0, 0xff),
      text("\r\n\r\n"),
    ]),
  ]);
  cases.push(["missing_host", text("GET / HTTP/1.1\r\n\r\n")]);
  cases.push(["http_0_9", text("GET /\r\n")]);
  cases.push(["http_2_preface", text("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")]);
  cases.push(["invalid_method", text("FROB / HTTP/1.1\r\nHost: x\r\n\r\n")]);
  cases.push(["empty_request", text("\r\n\r\n")]);
  cases.push(["header_without_colon", text("GET / HTTP/1.1\r\nHost x\r\n\r\n")]);
  cases.push(["percent", text("GET /% HTTP/1.1\r\nHost: x\r\n\r\n")]);
  cases.push(["percent_zz", text("GET /%zz HTTP/1.1\r\nHost: x\r\n\r\n")]);
  cases.push(["percent_00", text("GET /%00 HTTP/1.1\r\nHost: x\r\n\r\n")]);
  cases.push(["dot_dot", text("GET /../../etc/passwd HTTP/1.1\r\nHost: x\r\n\r\n")]);
  cases.push(["deep_path", text(`GET /${"a/".repeat(2048)}x HTTP/1.1\r\nHost: x\r\n\r\n`)]);
  return cases;
}

const CORPUS = [
  "GET / HTTP/1.1\r\nHost: x\r\n\r\n",
  "GET /missing HTTP/1.1\r\nHost: x\r\n\r\n",
  'POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: 17\r\n\r\n{"items":[1,2,3]}',
  "HEAD / HTTP/1.1\r\nHost: x\r\n\r\n",
  "GET / HTTP/1.1\r\nHost: x\r\n\r\nGET / HTTP/1.1\r\nHost: x\r\n\r\n",
  "GET / HTTP/1.0\r\nHost: x\r\n\r\n",
  "GET /?a=1&b=2 HTTP/1.1\r\nHost: x\r\nConnection: keep-alive\r\n\r\n",
  'POST /sum HTTP/1.1\r\nHost: x\r\nContent-Length: 12\r\nContent-Type: application/json\r\n\r\n{"items":[]}',
  "OPTIONS / HTTP/1.1\r\nHost: x\r\n\r\n",
  "GET http://x/ HTTP/1.1\r\nHost: x\r\n\r\n",
].map((value) => Buffer.from(value, "latin1"));

const DICTIONARY = [
  "\r\n",
  "\r\n\r\n",
  ":",
  " ",
  "Content-Length: ",
  "Transfer-Encoding: chunked",
  "chunked",
  "%",
  "%00",
  "\0",
  "HTTP/1.1",
  "A".repeat(64),
].map((value) => Buffer.from(value, "latin1"));

function mutateBuffer(rand, input) {
  let buffer = Buffer.from(input);
  const operations = 1 + Math.floor(rand() * 3);
  for (let step = 0; step < operations; step++) {
    const choice = Math.floor(rand() * 6);
    if (choice === 0) {
      if (buffer.length > 0) {
        buffer[Math.floor(rand() * buffer.length)] ^= 1 << Math.floor(rand() * 8);
      }
    } else if (choice === 1) {
      const position = Math.floor(rand() * (buffer.length + 1));
      buffer = Buffer.concat([
        buffer.subarray(0, position),
        Buffer.from([Math.floor(rand() * 256)]),
        buffer.subarray(position),
      ]);
    } else if (choice === 2) {
      if (buffer.length > 1) {
        const position = Math.floor(rand() * buffer.length);
        buffer = Buffer.concat([buffer.subarray(0, position), buffer.subarray(position + 1)]);
      }
    } else if (choice === 3) {
      if (buffer.length > 1) {
        const start = Math.floor(rand() * buffer.length);
        const length = 1 + Math.floor(rand() * Math.min(64, buffer.length - start));
        const piece = buffer.subarray(start, start + length);
        const position = Math.floor(rand() * (buffer.length + 1));
        buffer = Buffer.concat([buffer.subarray(0, position), piece, buffer.subarray(position)]);
      }
    } else if (choice === 4) {
      const token = DICTIONARY[Math.floor(rand() * DICTIONARY.length)];
      const position = Math.floor(rand() * (buffer.length + 1));
      buffer = Buffer.concat([buffer.subarray(0, position), token, buffer.subarray(position)]);
    } else {
      const length = 1 + Math.floor(rand() * 256);
      const position = Math.floor(rand() * (buffer.length + 1));
      buffer = Buffer.concat([
        buffer.subarray(0, position),
        Buffer.alloc(length, 0x41),
        buffer.subarray(position),
      ]);
    }
    if (buffer.length > 65536) buffer = buffer.subarray(0, 65536);
  }
  return buffer;
}




async function scenarioSlowloris(config) {
  const rand = rngFor("slowloris");
  const sockets = [];
  const connectErrors = new Map();
  await mapLimit(Array.from({ length: config.connections }, (_, index) => index), 128, async () => {
    try {
      sockets.push(await connect(3000));
    } catch (error) {
      bump(connectErrors, error.code ?? error.message);
    }
  });
  const prefix = Buffer.from(`GET / HTTP/1.1\r\nHost: x\r\nX-Pad: ${"A".repeat(200)}`, "latin1");
  let sent = 0;
  const timers = sockets.map((socket) => {
    let position = 0;
    const period = 1000 + Math.floor(rand() * 1000);
    return setInterval(() => {
      if (socket.destroyed) return;
      const offset = position % prefix.length;
      position++;
      if (safeWrite(socket, prefix.subarray(offset, offset + 1))) sent++;
    }, period);
  });
  const hold = sleep(config.holdMs);
  await sleep(config.holdMs * 0.7);
  const during = await healthCheck(1500);
  await hold;
  for (const timer of timers) clearInterval(timer);
  let held = 0;
  for (const socket of sockets) {
    if (!socket.destroyed) held++;
    socket.destroy();
  }
  const limits = connectErrors.size === 0 ? "" : ` connect_errors=${formatCounts(connectErrors)}`;
  noteToolLimits(connectErrors, "slowloris");
  return {
    during,
    outcomes: `${config.connections} conns: established=${sockets.length} held=${held} dripped_bytes=${sent}${limits}`,
  };
}

async function scenarioSlowBody(config) {
  const rand = rngFor("slow_body");
  const sockets = [];
  const connectErrors = new Map();
  await mapLimit(Array.from({ length: config.connections }, (_, index) => index), 64, async () => {
    try {
      sockets.push(await connect(3000));
    } catch (error) {
      bump(connectErrors, error.code ?? error.message);
    }
  });
  const headers = Buffer.from(
    "POST /sum HTTP/1.1\r\nHost: x\r\nContent-Type: application/json\r\nContent-Length: 100000\r\n\r\n",
    "latin1",
  );
  const body = Buffer.from(`{"items":[1,2,3]}${" ".repeat(100000)}`, "latin1");
  for (const socket of sockets) safeWrite(socket, headers);
  let sent = 0;
  const timers = sockets.map((socket) => {
    let position = 0;
    const period = 1000 + Math.floor(rand() * 1000);
    return setInterval(() => {
      if (socket.destroyed) return;
      const offset = position % body.length;
      position++;
      if (safeWrite(socket, body.subarray(offset, offset + 1))) sent++;
    }, period);
  });
  const hold = sleep(config.holdMs);
  await sleep(config.holdMs * 0.7);
  const during = await healthCheck(1500);
  await hold;
  for (const timer of timers) clearInterval(timer);
  for (const socket of sockets) socket.destroy();
  const limits = connectErrors.size === 0 ? "" : ` connect_errors=${formatCounts(connectErrors)}`;
  noteToolLimits(connectErrors, "slow_body");
  return {
    during,
    outcomes: `${config.connections} conns: established=${sockets.length} dripped_bytes=${sent}${limits}`,
  };
}

async function scenarioSlowRead(config) {
  const sockets = [];
  const connectErrors = new Map();
  await mapLimit(Array.from({ length: config.connections }, (_, index) => index), 32, async () => {
    try {
      const socket = await connect(3000);
      socket.pause();
      sockets.push(socket);
    } catch (error) {
      bump(connectErrors, error.code ?? error.message);
    }
  });
  const request = Buffer.from("GET / HTTP/1.1\r\nHost: x\r\n\r\n", "latin1");
  let written = 0;
  for (const socket of sockets) {
    const payload = Buffer.concat(Array.from({ length: config.requests }, () => request));
    if (safeWrite(socket, payload)) written += payload.length;
  }
  await sleep(500);
  const during = await healthCheck(HEALTH_TIMEOUT_MS);
  await sleep(500);
  for (const socket of sockets) socket.destroy();
  const limits = connectErrors.size === 0 ? "" : ` connect_errors=${formatCounts(connectErrors)}`;
  noteToolLimits(connectErrors, "slow_read");
  return {
    during,
    outcomes: `${config.connections} conns: requests=${config.connections * config.requests} bytes_written=${written}${limits}`,
  };
}

async function scenarioTable(name, cases) {
  const counters = new Map();
  const detail = [];
  let saved = null;
  let executed = 0;
  for (const [caseName, payload] of cases) {
    const result = await probe(payload, 1200);
    executed++;
    bump(counters, result.bucket);
    detail.push({ name: caseName, ...result });
    if (serverAlive() === false) {
      saved = saveArtifact(caseName, payload, {
        seed: opts.seed,
        profile: opts.profile,
        scenario: name,
        case: caseName,
        outcome: result,
      });
      break;
    }
  }
  noteProbeToolLimit(counters, name);
  return {
    during: null,
    outcomes: `${executed}/${cases.length} cases: ${summaryOf(counters)}${saved ? ` saved=${saved}` : ""}`,
    detail,
    saved,
  };
}

function noteProbeToolLimit(counters, scenario) {
  if (counters.has("tool_limit")) {
    warnings.push(`${scenario}: client resource limit EMFILE/ENFILE (tool limit, not a server bug)`);
  }
}
async function scenarioPipelineFlood() {
  const socket = await connect(3000);
  const payload = Buffer.from(("GET / HTTP/1.1\r\nHost: x\r\n\r\n").repeat(2000), "latin1");
  let buffer = Buffer.alloc(0);
  let closed = false;
  socket.on("data", (chunk) => {
    buffer = Buffer.concat([buffer, chunk]);
  });
  socket.once("close", () => {
    closed = true;
  });
  await new Promise((resolve) => {
    try {
      socket.write(payload, () => resolve());
    } catch {
      resolve();
    }
  });
  const started = Date.now();
  let stable = 0;
  let lastLength = -1;
  while (Date.now() - started < 5000) {
    await sleep(200);
    if (closed) break;
    if (buffer.length === lastLength) {
      stable++;
      if (stable >= 3) break;
    } else {
      stable = 0;
      lastLength = buffer.length;
    }
  }
  const text = buffer.toString("latin1");
  let responses = 0;
  let index = 0;
  while ((index = text.indexOf("HTTP/1.", index)) !== -1) {
    responses++;
    index += 7;
  }
  socket.destroy();
  return {
    during: null,
    outcomes: `written=2000 responses=${responses} bytes_read=${buffer.length}${closed ? " closed" : ""}`,
  };
}

async function scenarioChurn(total) {
  const rand = rngFor("churn");
  const counters = new Map();
  const request = Buffer.from("GET / HTTP/1.1\r\nHost: x\r\n\r\n", "latin1");
  const modes = ["immediate", "half_close", "reset_partial", "request_then_reset"];
  await mapLimit(Array.from({ length: total }, (_, index) => index), 32, async (index) => {
    const mode = modes[index % modes.length];
    let socket;
    try {
      socket = await connect(2000);
    } catch (error) {
      bump(counters, `connect_${error.code ?? "error"}`);
      return;
    }
    try {
      if (mode === "immediate") {
        socket.destroy();
      } else if (mode === "half_close") {
        socket.end("GET / HTT");
      } else if (mode === "reset_partial") {
        safeWrite(socket, "GET / HTT");
        socket.resetAndDestroy();
      } else {
        await new Promise((resolve) => {
          try {
            socket.write(request, () => resolve());
          } catch {
            resolve();
          }
        });
        socket.resetAndDestroy();
      }
      bump(counters, mode);
    } catch (error) {
      bump(counters, `write_${error.code ?? "error"}`);
    }
    if (rand() < 0.25) await sleep(0);
  });
  await sleep(300);
  noteToolLimits(counters, "churn", "connect_");
  return { during: null, outcomes: `${total} conns: ${formatCounts(counters)}` };
}

async function scenarioMutate(config) {
  const rand = rngFor("mutate");
  const counters = new Map();
  let saved = null;
  let executed = 0;
  for (let start = 0; start < config.mutations; start += 200) {
    const stop = Math.min(start + 200, config.mutations);
    const payloads = [];
    for (let index = start; index < stop; index++) {
      let payload = CORPUS[Math.floor(rand() * CORPUS.length)];
      const rounds = 1 + Math.floor(rand() * 3);
      for (let round = 0; round < rounds; round++) payload = mutateBuffer(rand, payload);
      payloads.push(payload);
    }
    let died = false;
    let offending = null;
    await mapLimit(payloads, 64, async (payload) => {
      if (died) return;
      const result = await probe(payload, 1000);
      executed++;
      bump(counters, result.bucket);
      if (serverAlive() === false) {
        died = true;
        offending = payload;
      }
    });
    if (died) {
      saved = saveArtifact("mutate", offending, {
        seed: opts.seed,
        profile: opts.profile,
        scenario: "mutate",
        corpus: offending.toString("latin1"),
      });
      break;
    }
  }
  noteProbeToolLimit(counters, "mutate");
  return {
    during: null,
    outcomes: `${executed}/${config.mutations} cases: ${summaryOf(counters)}${saved ? ` saved=${saved}` : ""}`,
    saved,
  };
}




const baseline = {
  rss_kb: rssKb(),
  fds: fdCount(),
  ticks: cpuTicks(),
};
const rows = [];
const failures = [];
const warnings = [];

function aliveLabel(alive) {
  return alive === null ? "-" : alive ? "yes" : "NO";
}

async function runScenario(name, body) {
  const started = Date.now();
  let detail = { during: null, outcomes: "-" };
  try {
    detail = await body();
  } catch (error) {
    failures.push(`${name}: tool error: ${error.code ?? error.message}`);
    detail = { during: null, outcomes: `tool_error=${error.code ?? error.message}` };
  }
  await closeAll();
  await sleep(150);
  const alive = serverAlive();
  const recovery = alive === false ? { ok: false, ms: null, detail: "dead" } : await recoverHealth();
  await sleep(100);
  const idle = await idleCpu();
  if (alive !== false && !recovery.ok) {
    failures.push(`${name}: no recovery within ${RECOVERY_LIMIT_MS} ms (${recovery.detail})`);
  }
  if (alive === false) failures.push(`${name}: server died`);
  if (idle.busy) failures.push(`${name}: idle CPU ${idle.percent}% over 1 s (busy loop)`);
  if (detail.during && !detail.during.ok) {
    const label = name === "slowloris" ? "slowloris health degraded during hold" : "health degraded during";
    warnings.push(`${name}: ${label} (${detail.during.detail})`);
  }
  const rss = rssKb();
  const fds = fdCount();
  const row = {
    name,
    alive: alive === null ? null : alive,
    raw_alive: aliveLabel(alive),
    health_during: detail.during ?? null,
    recovery_ms: recovery.ok ? recovery.ms : null,
    rss_delta_kb: rss !== null && baseline.rss_kb !== null ? rss - baseline.rss_kb : null,
    fd_delta: fds !== null && baseline.fds !== null ? fds - baseline.fds : null,
    idle_percent: idle.percent,
    idle_busy: idle.busy,
    outcomes: detail.outcomes,
    duration_ms: Date.now() - started,
    detail: detail.detail ?? null,
    saved: detail.saved ?? null,
  };
  rows.push(row);
  console.log(formatRow(row));
  return row;
}

async function main() {
  const startedAt = new Date();
  console.log(
    `http_fuzz: host=${HOST} port=${opts.port} pid=${opts.pid ?? "-"} seed=${opts.seed} profile=${opts.profile}`,
  );
  if (baseline.rss_kb === null && opts.pid !== null) {
    warnings.push("cannot read /proc/PID/status; RSS and CPU oracles are unavailable");
  }

  const initialAlive = serverAlive();
  if (initialAlive === false) {
    failures.push("server is not running before the run");
  }
  let startup = { ok: false, ms: null, detail: "skipped" };
  if (initialAlive !== false) {
    startup = await recoverHealth(RECOVERY_LIMIT_MS);
    if (!startup.ok) failures.push(`server unhealthy before the run (${startup.detail})`);
  }
  console.log(
    `http_fuzz: startup alive=${aliveLabel(initialAlive)} health=${formatDuring(startup)} baseline_rss_kb=${
      baseline.rss_kb ?? "-"
    } baseline_fds=${baseline.fds ?? "-"}`,
  );

  const selected = new Set(opts.only ?? SCENARIO_NAMES);
  printHeader();

  if (failures.length === 0) {
    if (selected.has("slowloris")) await runScenario("slowloris", () => scenarioSlowloris(settings.slowloris));
    if (selected.has("slow_body")) await runScenario("slow_body", () => scenarioSlowBody(settings.slowBody));
    if (selected.has("slow_read")) await runScenario("slow_read", () => scenarioSlowRead(settings.slowRead));
    if (selected.has("oversized")) {
      await runScenario("oversized", () => scenarioTable("oversized", oversizedCases()));
    }
    if (selected.has("malformed")) {
      await runScenario("malformed", () => scenarioTable("malformed", malformedCases()));
    }
    if (selected.has("pipeline_flood")) {
      await runScenario("pipeline_flood", () => scenarioPipelineFlood());
    }
    if (selected.has("churn")) await runScenario("churn", () => scenarioChurn(settings.churn));
    if (selected.has("mutate")) await runScenario("mutate", () => scenarioMutate(settings));
  }

  await closeAll();
  await sleep(200);
  const finalAlive = serverAlive();
  const finalIdle = await idleCpu();
  const finalHealth = finalAlive === false ? { ok: false, detail: "dead" } : await healthCheck();
  const finalRss = rssKb();
  const finalFds = fdCount();
  const finalFdDelta = finalFds !== null && baseline.fds !== null ? finalFds - baseline.fds : null;
  const finalRssDelta = finalRss !== null && baseline.rss_kb !== null ? finalRss - baseline.rss_kb : null;

  if (finalAlive === false) failures.push("server died during the run");
  if (finalAlive !== false && !finalHealth.ok) failures.push(`server unhealthy at the end (${finalHealth.detail})`);
  if (finalFdDelta !== null && finalFdDelta > 2) {
    failures.push(`file descriptor leak at the end: ${signed(finalFdDelta)} (allowed +2)`);
  }
  if (finalIdle.busy) failures.push(`server busy at the end while idle (${finalIdle.percent}%)`);

  console.log("");
  console.log(
    `final: alive=${aliveLabel(finalAlive)} health=${formatDuring(finalHealth)} rss_delta_kb=${signed(
      finalRssDelta,
    )} fd_delta=${signed(finalFdDelta)} idle_cpu=${finalIdle.percent === null ? "-" : `${finalIdle.percent}%`}`,
  );
  for (const warning of warnings) console.log(`WARN ${warning}`);
  if (failures.length === 0) {
    console.log(`verdict: PASS (${rows.length} scenarios, seed ${opts.seed})`);
  } else {
    for (const failure of failures) console.log(`FAIL ${failure}`);
    console.log(`verdict: FAIL (${failures.length} problem(s), seed ${opts.seed})`);
  }
  if (rows.some((row) => row.saved)) console.log(`artifacts: ${artifactsDir}`);

  const report = {
    tool: "http_fuzz",
    host: HOST,
    port: opts.port,
    pid: opts.pid,
    seed: opts.seed,
    profile: opts.profile,
    started_at: startedAt.toISOString(),
    duration_ms: Date.now() - startedAt.getTime(),
    baseline: { rss_kb: baseline.rss_kb, fds: baseline.fds, ticks: baseline.ticks },
    startup,
    rows,
    warnings,
    failures,
    final: {
      alive: finalAlive,
      health: finalHealth,
      rss_kb: finalRss,
      rss_delta_kb: finalRssDelta,
      fds: finalFds,
      fd_delta: finalFdDelta,
      idle_percent: finalIdle.percent,
      idle_busy: finalIdle.busy,
    },
    verdict: failures.length === 0 ? "PASS" : "FAIL",
  };
  if (opts.json) {
    fs.mkdirSync(path.dirname(path.resolve(opts.json)), { recursive: true });
    fs.writeFileSync(opts.json, `${JSON.stringify(report, null, 2)}\n`);
  }
  process.exitCode = failures.length === 0 ? 0 : 1;
}

main().catch((error) => {
  console.error(`http_fuzz: fatal tool error: ${error.stack ?? error.message}`);
  process.exit(1);
});
