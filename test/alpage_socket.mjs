import net from "node:net";

const port = Number(process.argv[2] ?? "18092");
const host = "127.0.0.1";

function fail(message) {
  console.error(`FAIL alpage socket: ${message}`);
  process.exit(1);
}

function findHeaderEnd(buffer) {
  return buffer.indexOf("\r\n\r\n");
}

function parseResponse(buffer) {
  const end = findHeaderEnd(buffer);
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
    const socket = net.createConnection({ host, port }, () => resolve(socket));
    socket.once("error", reject);
  });
}

function waitForResponse(socket, state, timeoutMs = 2000) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      cleanup();
      reject(new Error("timeout waiting for response"));
    }, timeoutMs);
    const onData = (chunk) => {
      state.buffer = Buffer.concat([state.buffer, chunk]);
      const parsed = parseResponse(state.buffer);
      if (parsed) {
        cleanup();
        state.buffer = parsed.rest;
        resolve(parsed);
      }
    };
    const onError = (error) => {
      cleanup();
      reject(error);
    };
    const onClose = () => {
      cleanup();
      reject(new Error("connection closed before response"));
    };
    const cleanup = () => {
      clearTimeout(timer);
      socket.off("data", onData);
      socket.off("error", onError);
      socket.off("close", onClose);
    };
    socket.on("data", onData);
    socket.once("error", onError);
    socket.once("close", onClose);
  });
}

function send(socket, text) {
  socket.write(text);
}

function assert(condition, message) {
  if (!condition) fail(message);
}

async function main() {
  const socket = await connect();
  const state = { buffer: Buffer.alloc(0) };


  send(
    socket,
    'POST /sum HTTP/1.1\r\nHost: localhost\r\nContent-Length: 17\r\n\r\n{"items":[1,2,',
  );
  await new Promise((resolve) => setTimeout(resolve, 50));
  send(socket, "3]}");
  const fragmented = await waitForResponse(socket, state);
  assert(fragmented.status === 200, `fragmented POST status ${fragmented.status}`);
  assert(fragmented.body === '{"sum":6}', `fragmented POST body ${fragmented.body}`);


  send(socket, "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n");
  const first = await waitForResponse(socket, state);
  assert(first.status === 200, `keep-alive GET status ${first.status}`);
  assert(first.body === "hello", `keep-alive GET body ${first.body}`);

  send(socket, "GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n");
  const missing = await waitForResponse(socket, state);
  assert(missing.status === 404, `missing status ${missing.status}`);

  send(socket, "GET /sum HTTP/1.1\r\nHost: localhost\r\n\r\n");
  const wrong = await waitForResponse(socket, state);
  assert(wrong.status === 405, `wrong method status ${wrong.status}`);
  assert((wrong.headers.get("allow") ?? "").includes("POST"), "405 missing Allow: POST");

  socket.end();
  console.log("ok alpage socket");
}

main().catch((error) => fail(error.message));
