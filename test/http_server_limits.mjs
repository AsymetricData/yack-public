








import net from "node:net";

const port = Number(process.argv[2] ?? "18120");
const scenario = process.argv[3] ?? "";
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

function parseResponse(buffer) {
  const end = buffer.indexOf("\r\n\r\n");
  if (end < 0) return null;
  const head = buffer.subarray(0, end).toString("latin1");
  const lines = head.split("\r\n");
  const status = Number(lines[0].split(" ")[1]);
  const headers = new Map();
  for (const line of lines.slice(1)) {
    const colon = line.indexOf(":");
    if (colon < 0) continue;
    headers.set(line.slice(0, colon).toLowerCase(), line.slice(colon + 1).trim());
  }
  const length = Number(headers.get("content-length") ?? "0");
  const total = end + 4 + length;
  if (buffer.length < total) return null;
  return {
    status,
    headers,
    body: buffer.subarray(end + 4, total).toString("utf8"),
    rest: buffer.subarray(total),
  };
}

function connect() {
  return new Promise((resolve, reject) => {
    const socket = net.createConnection({ host, port });
    const state = { buffer: Buffer.alloc(0), closed: false };
    socket.on("data", (chunk) => {
      state.buffer = Buffer.concat([state.buffer, chunk]);
    });
    socket.on("close", () => {
      state.closed = true;
    });
    socket.once("connect", () => resolve({ socket, state }));
    socket.once("error", reject);
  });
}

async function response(state, timeoutMs = 3000) {
  const started = Date.now();
  while (true) {
    const parsed = parseResponse(state.buffer);
    if (parsed) {
      state.buffer = parsed.rest;
      return parsed;
    }
    if (state.closed) throw new Error("connection closed before response");
    if (Date.now() - started > timeoutMs) throw new Error("timeout waiting for response");
    await delay(5);
  }
}

async function waitClosed(state, timeoutMs = 2000) {
  const started = Date.now();
  while (!state.closed) {
    if (Date.now() - started > timeoutMs) return false;
    await delay(10);
  }
  return true;
}


function paddedGet(path, totalBytes) {
  const head = `GET ${path} HTTP/1.1\r\nHost: localhost\r\nX-Pad: `;
  const tail = "\r\n\r\n";
  const pad = Math.max(0, totalBytes - head.length - tail.length);
  return head + "a".repeat(pad) + tail;
}

function get(path) {
  return `GET ${path} HTTP/1.1\r\nHost: localhost\r\n\r\n`;
}

async function checkRequest(name, request, expectedStatus, expectedBody) {
  const { socket, state } = await connect();
  socket.write(request);
  const parsed = await response(state);
  check(`${name} status`, parsed.status === expectedStatus, `got ${parsed.status}`);
  if (expectedBody !== undefined) {
    check(`${name} body`, parsed.body === expectedBody, `got ${JSON.stringify(parsed.body)}`);
  }
  socket.destroy();
  return { parsed, state };
}

async function run() {
  switch (scenario) {
    case "headers-small": {

      const { socket, state } = await connect();
      socket.write(paddedGet("/", 6000));
      const parsed = await response(state);
      check("headers-small status", parsed.status === 200, `got ${parsed.status}`);
      check("headers-small body", parsed.body === "hello", `got ${JSON.stringify(parsed.body)}`);
      socket.write(get("/"));
      const second = await response(state);
      check("headers-small keep-alive", second.status === 200, `got ${second.status}`);
      socket.destroy();
      break;
    }

    case "headers-large": {

      const { socket, state } = await connect();
      socket.write(paddedGet("/", 20000));
      const parsed = await response(state);
      check("headers-large status", parsed.status === 431, `got ${parsed.status}`);
      check("headers-large close", await waitClosed(state), "connection stayed open");
      socket.destroy();
      break;
    }

    case "headers-raised": {
      const { socket, state } = await connect();
      socket.write(paddedGet("/", 20000));
      const parsed = await response(state);
      check("headers-raised status", parsed.status === 200, `got ${parsed.status}`);
      check("headers-raised body", parsed.body === "hello", `got ${JSON.stringify(parsed.body)}`);
      socket.destroy();
      break;
    }

    case "body-large": {


      const { socket, state } = await connect();
      const started = Date.now();
      socket.write(
        "POST /sum HTTP/1.1\r\nHost: localhost\r\nContent-Length: 100000\r\n\r\n",
      );
      const parsed = await response(state, 1500);
      const elapsed = Date.now() - started;
      check("body-large status", parsed.status === 413, `got ${parsed.status}`);
      check("body-large prompt", elapsed < 1000, `took ${elapsed} ms`);
      check("body-large close", await waitClosed(state), "connection stayed open");
      socket.destroy();
      break;
    }

    case "big-default": {
      const { socket, state } = await connect();
      socket.write(get("/big"));
      const parsed = await response(state, 5000);
      check("big-default status", parsed.status === 200, `got ${parsed.status}`);
      check(
        "big-default content-length",
        parsed.headers.get("content-length") === "20000",
        `got ${parsed.headers.get("content-length")}`,
      );
      check("big-default body", parsed.body.length === 20000, `got ${parsed.body.length} bytes`);
      socket.destroy();
      break;
    }

    case "big-limit": {
      const { socket, state } = await connect();
      socket.write(get("/big"));
      const parsed = await response(state);
      check("big-limit status", parsed.status === 500, `got ${parsed.status}`);
      check("big-limit close", await waitClosed(state), "connection stayed open");
      socket.destroy();
      break;
    }

    case "survives-431": {


      const rejected = await connect();
      rejected.socket.write(paddedGet("/", 20000));
      const first = await response(rejected.state);
      check("survives-431 rejected", first.status === 431, `got ${first.status}`);
      check("survives-431 rejected close", await waitClosed(rejected.state), "connection stayed open");
      rejected.socket.destroy();

      const { socket, state } = await connect();
      socket.write(get("/"));
      const hello = await response(state);
      check("survives-431 hello", hello.status === 200 && hello.body === "hello", `got ${hello.status} ${JSON.stringify(hello.body)}`);
      socket.write(get("/big"));
      const big = await response(state, 5000);
      check("survives-431 big", big.status === 200 && big.body.length === 20000, `got ${big.status}`);
      socket.destroy();
      break;
    }

    default:
      console.log(`FAIL http_server_limits: unknown scenario '${scenario}'`);
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
