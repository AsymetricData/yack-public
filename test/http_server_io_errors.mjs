









import net from "node:net";

const port = Number(process.argv[2] ?? "18140");
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

function get() {
  return "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
}

async function peers() {


  for (let i = 0; i < 200; i += 1) {
    const { socket } = await connect();
    socket.on("error", () => {});
    socket.write("GET / HTTP/1.1\r\nHost: localhost\r\nX-Partial: 1\r\n");
    socket.resetAndDestroy();
  }


  for (let i = 0; i < 200; i += 1) {
    const { socket } = await connect();
    socket.on("error", () => {});
    socket.write("GET /big HTTP/1.1\r\nHost: localhost\r\n\r\n");
    socket.resetAndDestroy();
  }

  await delay(300);


  const { socket, state } = await connect();
  socket.write(get());
  const parsed = await response(state, 5000);
  check("peers still serves", parsed.status === 200, `got ${parsed.status}`);
  check("peers hello", parsed.body === "hello", `got ${JSON.stringify(parsed.body)}`);
  socket.destroy();
}

async function emfile() {
  const sockets = [];
  const connecting = [];
  for (let i = 0; i < 100; i += 1) {
    connecting.push(
      new Promise((resolve) => {
        const socket = net.createConnection({ host, port });
        socket.on("error", () => {});
        socket.once("connect", () => resolve(socket));
      }),
    );
  }
  const opened = await Promise.all(connecting);
  for (const socket of opened) sockets.push(socket);


  await delay(1500);
  for (const socket of sockets) socket.destroy();
  await delay(300);


  const started = Date.now();
  let parsed = null;
  while (Date.now() - started < 5000) {
    try {
      const { socket, state } = await connect();
      socket.write(get());
      parsed = await response(state, 1000);
      socket.destroy();
      break;
    } catch (error) {
      await delay(50);
    }
  }
  check("emfile serves after release", parsed !== null && parsed.status === 200,
    parsed === null ? "no response" : `got ${parsed.status}`);
  if (parsed !== null) {
    check("emfile hello", parsed.body === "hello", `got ${JSON.stringify(parsed.body)}`);
  }
}

async function main() {
  switch (scenario) {
    case "peers":
      await peers();
      console.log("ok io_errors peers");
      break;
    case "emfile":
      await emfile();
      console.log("ok io_errors emfile");
      break;
    default:
      console.log(`FAIL io_errors: unknown scenario '${scenario}'`);
      process.exit(1);
  }
}

main()
  .then(() => {
    if (failures > 0) process.exit(1);
  })
  .catch((error) => {
    console.log(`FAIL ${scenario}: ${error.message}`);
    process.exit(1);
  });
