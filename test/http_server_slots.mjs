import net from "node:net";

const port = Number(process.argv[2] ?? "18092");
const host = "127.0.0.1";

function fail(message) {
  console.error(`FAIL http_server slots: ${message}`);
  process.exit(1);
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

function assert(condition, message) {
  if (!condition) fail(message);
}

async function get(socket, state, path, expectedStatus, expectedBody) {
  socket.write(`GET ${path} HTTP/1.1\r\nHost: localhost\r\n\r\n`);
  const response = await waitForResponse(socket, state);
  assert(response.status === expectedStatus, `GET ${path} status ${response.status}`);
  if (expectedBody !== undefined) {
    assert(response.body === expectedBody, `GET ${path} body ${JSON.stringify(response.body)}`);
  }
}

async function main() {

  const first = await connect();
  const firstState = { buffer: Buffer.alloc(0) };
  await get(first, firstState, "/", 200, "hello");
  await get(first, firstState, "/missing", 404);



  const sockets = [first];
  for (let i = 0; i < 95; i++) sockets.push(await connect());
  const high = sockets[sockets.length - 1];
  const highState = { buffer: Buffer.alloc(0) };

  await get(high, highState, "/", 200, "hello");
  await get(high, highState, "/missing", 404);


  high.write("HEAD / HTTP/1.1\r\nHost: localhost\r\n\r\n");
  const head = await waitForResponse(high, highState);
  assert(head.status === 405 || head.status === 200, `HEAD status ${head.status}`);


  high.write(
    'POST /sum HTTP/1.1\r\nHost: localhost\r\nContent-Length: 17\r\n\r\n{"items":[1,2,',
  );
  await new Promise((resolve) => setTimeout(resolve, 30));
  high.write("3]}");
  const fragmented = await waitForResponse(high, highState);
  assert(fragmented.status === 200, `fragmented POST status ${fragmented.status}`);
  assert(fragmented.body === '{"sum":6}', `fragmented POST body ${JSON.stringify(fragmented.body)}`);


  await get(high, highState, "/", 200, "hello");

  for (const socket of sockets) socket.end();
  console.log("ok http_server slots");
}

main().catch((error) => fail(error.message));
