









import net from "node:net";
import fs from "node:fs";

const port = Number(process.argv[2] ?? "18140");
const scenario = process.argv[3] ?? "";
const pid = process.argv[4] ? Number(process.argv[4]) : null;
const host = "127.0.0.1";

let failures = 0;

function check(name, condition, detail) {
  if (condition) {
    console.log(`ok ${name}`);
  } else {
    failures += 1;
    console.log(`FAIL ${name}: ${detail}`);
  }
}

function delay(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function connect() {
  return new Promise((resolve, reject) => {
    const socket = net.createConnection({ host, port });
    const state = { buffer: Buffer.alloc(0), closed: false, last: 0 };
    socket.on("data", (chunk) => {
      state.buffer = Buffer.concat([state.buffer, chunk]);
      state.last = Date.now();
    });
    socket.on("close", () => {
      state.closed = true;
    });
    socket.on("error", () => {});
    socket.once("connect", () => resolve({ socket, state }));
    socket.once("error", reject);
  });
}



async function collect(state, idleMs = 250, totalMs = 4000) {
  const started = Date.now();
  state.buffer = Buffer.alloc(0);
  state.last = Date.now();
  while (!state.closed) {
    if (Date.now() - started > totalMs) break;
    if (state.buffer.length > 0 && Date.now() - state.last > idleMs) break;
    if (state.buffer.length === 0 && Date.now() - started > totalMs) break;
    await delay(10);
  }
  const text = state.buffer.toString("latin1");
  state.buffer = Buffer.alloc(0);
  return text;
}


function readOne(text, offset, head) {
  const rel = text.indexOf("\r\n\r\n", offset);
  if (rel < 0) return null;
  const end = rel;
  const lines = text.slice(offset, end).split("\r\n");
  const match = /^HTTP\/1\.[01] (\d{3})/.exec(lines[0]);
  if (!match) return null;
  const status = Number(match[1]);
  const headers = new Map();
  for (const line of lines.slice(1)) {
    const colon = line.indexOf(":");
    if (colon < 0) continue;
    headers.set(line.slice(0, colon).toLowerCase(), line.slice(colon + 1).trim());
  }
  const bodyless = head || (status >= 100 && status < 200) || status === 204 || status === 304;
  const length = bodyless ? 0 : Number(headers.get("content-length") ?? "0");
  const stop = end + 4 + length;
  if (text.length < stop) return null;
  return {
    status,
    headers,
    body: text.slice(end + 4, stop),
    head: text.slice(offset, end),
    next: stop,
  };
}

function readAll(text, head = false) {
  const responses = [];
  let offset = 0;
  for (;;) {
    const one = readOne(text, offset, head);
    if (one === null) break;
    const copy = { ...one };
    delete copy.next;
    responses.push(copy);
    offset = one.next;
  }
  return { responses, rest: text.slice(offset) };
}

const IMF_FIXDATE =
  /^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), \d{2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \d{4} \d{2}:\d{2}:\d{2} GMT$/;

const dated = [];

function expectDate(name, response) {
  const value = response?.headers.get("date");
  check(`${name} date`, typeof value === "string" && IMF_FIXDATE.test(value), `got ${JSON.stringify(value)}`);
  if (value) dated.push(value);
}


async function exchange(payload, { head = false, idleMs = 250 } = {}) {
  const { socket, state } = await connect();
  socket.write(payload);
  const text = await collect(state, idleMs);
  const parsed = readAll(text, head);
  socket.destroy();
  return { ...parsed, closed: state.closed, raw: text };
}

async function caseStatus(
  name,
  payload,
  expectedStatus,
  { closed = null, head = false, body = null, idleMs = 250 } = {},
) {
  const result = await exchange(payload, { head, idleMs });
  const first = result.responses[0];
  check(`${name} status`, first?.status === expectedStatus, `got ${first?.status ?? "no response"}`);
  check(`${name} single`, result.responses.length === 1, `got ${result.responses.length} responses`);
  if (closed !== null) {
    check(`${name} close`, result.closed === closed, `closed=${result.closed}`);
  }
  if (body !== null) {
    check(`${name} body`, first?.body === body, `got ${JSON.stringify(first?.body)}`);
  }
  if (first) expectDate(name, first);
  return result;
}

function get(path, extra = "") {
  return `GET ${path} HTTP/1.1\r\nHost: localhost\r\n${extra}\r\n`;
}


function post(path, headers) {
  return `POST ${path} HTTP/1.1\r\nHost: localhost\r\n${headers}`;
}

function fdCount() {
  if (pid === null) return null;
  try {
    return fs.readdirSync(`/proc/${pid}/fd`).length;
  } catch {
    return null;
  }
}

async function conformance() {

  {
    const result = await caseStatus("keepalive", get("/"), 200, { closed: false });
    check("keepalive body", result.responses[0]?.body === "hello", "wrong body");
  }


  await caseStatus("no-host", "GET / HTTP/1.1\r\n\r\n", 400, { closed: true });


  await caseStatus(
    "two-host",
    "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n",
    400,
    { closed: true },
  );


  await caseStatus("http10", "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n", 200, { closed: true });


  await caseStatus(
    "http10-keepalive",
    "GET / HTTP/1.0\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n",
    200,
    { closed: false },
  );


  {
    const result = await caseStatus(
      "close-token",
      get("/", "Connection: close\r\n"),
      200,
      { closed: true },
    );
    check(
      "close-token header",
      /connection:[^\r\n]*close/i.test(result.responses[0]?.head ?? ""),
      "no Connection: close header",
    );
  }


  await caseStatus("http20", "GET / HTTP/2.0\r\nHost: x\r\n\r\n", 505, { closed: true });


  await caseStatus("garbage", "GARBAGE\r\n\r\n", 400, { closed: true });


  await caseStatus(
    "chunked",
    post("/echo", "Transfer-Encoding: chunked\r\n\r\n") + "5\r\nhello\r\n0\r\n\r\n",
    200,
    { body: "hello" },
  );


  await caseStatus(
    "chunked-ext",
    post("/echo", "Transfer-Encoding: chunked\r\n\r\n") +
      "5;name=value\r\nhello\r\n0\r\nX-Trailer: t\r\n\r\n",
    200,
    { body: "hello" },
  );


  await caseStatus(
    "chunked-bad-size",
    post("/echo", "Transfer-Encoding: chunked\r\n\r\n") + "zz\r\nhello\r\n0\r\n\r\n",
    400,
    { closed: true },
  );


  await caseStatus(
    "chunked-huge-size",
    post("/echo", "Transfer-Encoding: chunked\r\n\r\n") + "ffffffffff\r\nhello\r\n0\r\n\r\n",
    400,
    { closed: true },
  );


  {
    const chunk = "x".repeat(20000);
    await caseStatus(
      "chunked-too-big",
      post("/echo-len", "Transfer-Encoding: chunked\r\n\r\n") +
        `${(20000).toString(16)}\r\n${chunk}\r\n0\r\n\r\n`,
      413,
      { closed: true },
    );
  }


  {
    const payload =
      post("/echo", "Transfer-Encoding: chunked\r\nContent-Length: 5\r\n\r\n") +
      "0\r\n\r\n" +
      "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
    const result = await caseStatus("te-cl", payload, 400, { closed: true });
    check(
      "te-cl no smuggled response",
      !result.responses.slice(1).some((response) => response.status === 200),
      "second response emitted",
    );
  }


  await caseStatus(
    "te-gzip",
    post("/echo", "Transfer-Encoding: gzip\r\n\r\n") + "hello",
    501,
    { closed: true },
  );


  for (const [name, value] of [
    ["cl-list-diff", "5, 6"],
    ["cl-negative", "-1"],
    ["cl-plus", "+5"],
    ["cl-overflow", "99999999999"],
    ["cl-space", "5 5"],
  ]) {
    await caseStatus(
      name,
      post("/echo", `Content-Length: ${value}\r\n\r\n`) + "hello",
      400,
      { closed: true },
    );
  }


  await caseStatus(
    "cl-repeat",
    post("/echo", "Content-Length: 5\r\nContent-Length: 5\r\n\r\n") + "hello",
    200,
    { body: "hello" },
  );


  {
    const result = await caseStatus(
      "get-with-body",
      "GET / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\nGET /",
      200,
      { closed: false, idleMs: 400 },
    );
    check(
      "get-with-body no smuggling",
      result.responses.length === 1,
      `got ${result.responses.length} responses`,
    );
  }


  await caseStatus(
    "obs-fold",
    "GET / HTTP/1.1\r\nHost: localhost\r\nX-A: a\r\n b\r\n\r\n",
    400,
    { closed: true },
  );
  await caseStatus(
    "space-before-colon",
    "GET / HTTP/1.1\r\nHost: localhost\r\nX-A : a\r\n\r\n",
    400,
    { closed: true },
  );


  {
    const result = await exchange(get("/") + get("/nocontent"), { idleMs: 400 });
    check(
      "pipeline two responses",
      result.responses.length === 2,
      `got ${result.responses.length}`,
    );
    check(
      "pipeline order",
      result.responses[0]?.status === 200 && result.responses[1]?.status === 204,
      `got ${result.responses.map((r) => r.status).join(",")}`,
    );
    for (const [index, response] of result.responses.entries()) expectDate(`pipeline-${index}`, response);
  }


  {
    const result = await exchange("HEAD / HTTP/1.1\r\nHost: localhost\r\n\r\n", {
      head: true,
      idleMs: 300,
    });
    const response = result.responses[0];
    check("head status", response?.status === 200, `got ${response?.status}`);
    check(
      "head content-length",
      Number(response?.headers.get("content-length") ?? "0") > 0,
      `got ${response?.headers.get("content-length")}`,
    );
    check("head empty body", (response?.body ?? "") === "", `got ${JSON.stringify(response?.body)}`);
    check("head no extra bytes", result.rest === "", `trailing ${JSON.stringify(result.rest)}`);
    if (response) expectDate("head", response);
  }


  {
    const { socket, state } = await connect();
    socket.write(
      post("/echo", "Content-Length: 5\r\nExpect: 100-continue\r\n\r\n"),
    );
    const interim = readAll(await collect(state, 400));
    check("expect-100 interim", interim.responses[0]?.status === 100, `got ${interim.responses[0]?.status ?? "nothing"}`);
    check("expect-100 single", interim.responses.length === 1, `got ${interim.responses.length}`);
    socket.write("hello");
    const final = readAll(await collect(state, 400));
    check("expect-100 final", final.responses[0]?.status === 200, `got ${final.responses[0]?.status ?? "nothing"}`);
    check("expect-100 body", final.responses[0]?.body === "hello", `got ${JSON.stringify(final.responses[0]?.body)}`);
    if (final.responses[0]) expectDate("expect-100", final.responses[0]);
    socket.destroy();
  }


  await caseStatus(
    "expect-other",
    post("/echo", "Content-Length: 5\r\nExpect: foo\r\n\r\n") + "hello",
    417,
    { closed: true },
  );


  {
    const result = await caseStatus("no-content", get("/nocontent"), 204, { closed: false });
    const response = result.responses[0];
    check(
      "no-content no length",
      response !== undefined && !response.headers.has("content-length"),
      "Content-Length present",
    );
    check("no-content empty body", (response?.body ?? "") === "", "body present");
  }


  if (pid !== null) {
    const before = fdCount();
    const batch = [];
    for (let index = 0; index < 100; index++) {
      batch.push("GET / HTTP/1.1\r\n\r\n");
      batch.push(post("/echo", "Content-Length: 5, 6\r\n\r\n") + "hello");
      batch.push("GET / HTTP/2.0\r\nHost: x\r\n\r\n");
      batch.push(post("/echo", "Transfer-Encoding: chunked\r\nContent-Length: 5\r\n\r\n"));
    }
    for (const payload of batch) {
      const { socket, state } = await connect();
      socket.write(payload);
      await collect(state, 60, 1500);
      socket.destroy();
    }
    await delay(300);
    const after = fdCount();
    check(
      "fd delta",
      before !== null && after !== null && after - before === 0,
      `before=${before} after=${after}`,
    );
  }

  check("date sample", dated.length >= 5, `only ${dated.length} dates seen`);
}



async function bigChunked() {
  const total = 2000000;
  const sizes = [1, 2, 3, 7, 64, 1000, 4096, 65536];
  const parts = [Buffer.from(post("/echo-len", "Transfer-Encoding: chunked\r\n\r\n"), "latin1")];
  let sent = 0;
  let index = 0;
  while (sent < total) {
    const size = Math.min(sizes[index % sizes.length], total - sent);
    index += 1;
    const payload = Buffer.alloc(size, 0x61);
    parts.push(Buffer.from(`${size.toString(16)}\r\n`, "latin1"), payload, Buffer.from("\r\n", "latin1"));
    sent += size;
  }
  parts.push(Buffer.from("0\r\n\r\n", "latin1"));
  const request = Buffer.concat(parts);

  const { socket, state } = await connect();
  const started = Date.now();
  const written = new Promise((resolve) => socket.write(request, resolve));
  const text = await collect(state, 500, 60000);
  await written;
  const elapsed = Date.now() - started;
  const result = readAll(text);
  const response = result.responses[0];
  check("big-chunked status", response?.status === 200, `got ${response?.status ?? "no response"}`);
  check(
    "big-chunked length",
    response?.body === String(total),
    `got ${JSON.stringify(response?.body)}`,
  );
  check("big-chunked linear", elapsed < 20000, `took ${elapsed} ms`);
  socket.destroy();
}







async function slowLarge() {
  const size = 2621440;
  const expected = "0123456789".repeat(size / 10);


  {
    const { socket, state } = await connect();
    socket.pause();
    socket.write(get("/huge", "Connection: close\r\n"));
    await delay(200);
    socket.resume();
    const text = await collect(state, 500, 30000);
    const response = readAll(text).responses[0];
    check("slow-large close status", response?.status === 200, `got ${response?.status ?? "no response"}`);
    check(
      "slow-large close length",
      response?.body.length === size,
      `got ${response?.body.length ?? 0}`,
    );
    check("slow-large close body", response?.body === expected, "wrong body");
    check("slow-large close closed", state.closed, `closed=${state.closed}`);
    socket.destroy();
  }


  {
    const { socket, state } = await connect();
    socket.pause();
    socket.write(get("/huge") + get("/nocontent"));
    await delay(200);
    socket.resume();
    const text = await collect(state, 500, 30000);
    const responses = readAll(text).responses;
    check("slow-large pipeline two responses", responses.length === 2, `got ${responses.length}`);
    check(
      "slow-large pipeline order",
      responses[0]?.status === 200 && responses[1]?.status === 204,
      `got ${responses.map((r) => r.status).join(",")}`,
    );
    check(
      "slow-large pipeline big body",
      responses[0]?.body === expected,
      "wrong first body",
    );
    check("slow-large pipeline closed", !state.closed, `closed=${state.closed}`);
    socket.destroy();
  }
}

async function run() {
  switch (scenario) {
    case "conformance":
      await conformance();
      break;
    case "big-chunked":
      await bigChunked();
      break;
    case "slow-large":
      await slowLarge();
      break;
    default:
      console.log(`FAIL http11_conformance: unknown scenario '${scenario}'`);
      process.exit(1);
  }
}

run()
  .then(() => {
    if (failures > 0) process.exit(1);
  })
  .catch((error) => {
    console.log(`FAIL ${scenario}: ${error.message}`);
    process.exit(1);
  });
