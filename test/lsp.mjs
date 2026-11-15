#!/usr/bin/env node








import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const yack = join(root, "yack");
const stdlibFile = join(root, "examples/stdlib/src/main.yack");

let failures = 0;
function check(name, condition, detail = "") {
  if (condition) {
    console.log(`ok   lsp/${name}`);
  } else {
    failures += 1;
    console.log(`FAIL lsp/${name}${detail ? ` (${detail})` : ""}`);
  }
}

class Client {
  constructor() {
    this.child = spawn(yack, ["lsp"], { cwd: root, stdio: ["pipe", "pipe", "pipe"] });
    this.buffer = Buffer.alloc(0);
    this.messages = [];
    this.nextId = 1;
    this.stderr = "";
    this.child.stdout.on("data", (chunk) => this.onData(chunk));
    this.child.stderr.on("data", (chunk) => {
      this.stderr += chunk.toString("utf8");
    });
  }

  onData(chunk) {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    for (;;) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd < 0) return;
      const header = this.buffer.subarray(0, headerEnd).toString("utf8");
      const match = /Content-Length:\s*(\d+)/i.exec(header);
      if (!match) throw new Error(`missing Content-Length in: ${header}`);
      const length = Number(match[1]);
      if (this.buffer.length < headerEnd + 4 + length) return;
      const body = this.buffer
        .subarray(headerEnd + 4, headerEnd + 4 + length)
        .toString("utf8");
      this.buffer = this.buffer.subarray(headerEnd + 4 + length);
      this.messages.push(JSON.parse(body));
    }
  }

  send(message) {
    const body = JSON.stringify(message);
    this.child.stdin.write(
      `Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`,
    );
  }

  notify(method, params) {
    this.send({ jsonrpc: "2.0", method, params });
  }

  async request(method, params, timeoutMs = 15000) {
    const id = this.nextId++;
    this.send({ jsonrpc: "2.0", id, method, params });
    return await this.wait((message) => message.id === id, `response ${method}`, timeoutMs);
  }

  async wait(predicate, label, timeoutMs = 15000) {
    const started = Date.now();
    for (;;) {
      const found = this.messages.find(predicate);
      if (found) return found;
      if (Date.now() - started > timeoutMs) {
        throw new Error(`timeout waiting for ${label}; stderr=${this.stderr}`);
      }
      await new Promise((resolve) => setTimeout(resolve, 20));
    }
  }

  async open(uri, text) {
    this.notify("textDocument/didOpen", {
      textDocument: { uri, languageId: "yack", version: 1, text },
    });
    return await this.wait(
      (message) =>
        message.method === "textDocument/publishDiagnostics" &&
        message.params.uri === uri,
      `diagnostics for ${uri}`,
    );
  }

  stop() {
    this.child.kill();
  }
}

function markdownContents(result) {
  if (!result || typeof result !== "object") return "";
  if (result.contents && typeof result.contents.value === "string") {
    return result.contents.value;
  }
  return "";
}

const client = new Client();
try {
  const initialize = await client.request("initialize", {
    processId: process.pid,
    rootUri: `file://${root}`,
    capabilities: {},
  });
  const capabilities = initialize.result.capabilities;
  check("initialize", capabilities.textDocumentSync.openClose === true);
  check("inlay hints capability", capabilities.inlayHintProvider === true);
  check(
    "completion capability",
    capabilities.completionProvider.triggerCharacters.includes("."),
  );
  check(
    "signature help capability",
    capabilities.signatureHelpProvider.triggerCharacters.includes("("),
  );
  check(
    "navigation capabilities",
    capabilities.documentSymbolProvider === true &&
      capabilities.foldingRangeProvider === true &&
      capabilities.documentFormattingProvider === true &&
      capabilities.definitionProvider === true &&
      capabilities.declarationProvider === true &&
      capabilities.typeDefinitionProvider === true &&
      capabilities.referencesProvider === true &&
      capabilities.documentHighlightProvider === true &&
      capabilities.documentLinkProvider === true &&
      capabilities.semanticTokensProvider.legend.tokenTypes.includes("function") &&
      capabilities.workspaceSymbolProvider === true,
    JSON.stringify(capabilities),
  );
  client.notify("initialized", {});

  const simpleUri = "file:///tmp/yack-lsp-test.yack";
  const goodText = `fun main () =\n    let val x = 1 + 2\n    in print_int (x)\n    end\n`;
  const goodDiagnostics = await client.open(simpleUri, goodText);
  check("valid file has no diagnostics", goodDiagnostics.params.diagnostics.length === 0);

  const incrementalUri = "file:///tmp/yack-lsp-incremental.yack";
  await client.open(incrementalUri, "fun main () =\n    print_int (1)\n");
  client.notify("textDocument/didChange", {
    textDocument: { uri: incrementalUri, version: 2 },
    contentChanges: [
      {
        range: {
          start: { line: 1, character: 15 },
          end: { line: 1, character: 16 },
        },
        text: '"x"',
      },
    ],
  });
  const incremental = await client.wait(
    (message) =>
      message.method === "textDocument/publishDiagnostics" &&
      message.params.uri === incrementalUri &&
      message.params.diagnostics.some(
        (diagnostic) => diagnostic.code === "E3004",
      ),
    "incremental change diagnostic",
  );
  check(
    "incremental didChange applies ranges",
    incremental.params.diagnostics.some(
      (diagnostic) => diagnostic.code === "E3004",
    ),
  );

  const unusedUri = "file:///tmp/yack-lsp-unused.yack";
  const unusedText = `fun unused_helper (x : int) : int = x + 1

fun main () =
    print_int (1)
`;
  const unusedDiagnostics = await client.open(unusedUri, unusedText);
  const unusedWarning = unusedDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "W3001",
  );
  check(
    "unused function warning",
    Boolean(unusedWarning) && unusedWarning.severity === 2,
  );

  const containerUri = "file:///tmp/yack-lsp-container.yack";
  const containerText = `fun length_of (values : int array) : int =
    array_length (values)

fun main () =
    let val values : int array = array_make (1, 3)
    in print_int (length_of (values)) end
`;
  const containerDiagnostics = await client.open(containerUri, containerText);
  const readOnlyWarning = containerDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "W3102",
  );
  check(
    "read-only container parameter warning",
    Boolean(readOnlyWarning) && readOnlyWarning.severity === 2,
  );
  const containerActions = await client.request("textDocument/codeAction", {
    textDocument: { uri: containerUri },
    range: readOnlyWarning.range,
    context: { diagnostics: [readOnlyWarning] },
  });
  const borrowAction = containerActions.result.find((action) =>
    action.title.includes("borrow"),
  );
  const borrowEdits = borrowAction?.edit?.changes?.[containerUri] ?? [];
  check(
    "quick fix adds borrow to parameter and call site",
    borrowEdits.some((edit) => edit.newText === "borrow (int array)") &&
      borrowEdits.some((edit) => edit.newText === "borrow (values)"),
    JSON.stringify(containerActions.result),
  );



  const outsideCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: simpleUri },
    position: { line: 1, character: 8 },
  });
  check(
    "completion works outside a package",
    Array.isArray(outsideCompletion.result.items),
  );

  const hints = await client.request("textDocument/inlayHint", {
    textDocument: { uri: simpleUri },
    range: { start: { line: 0, character: 0 }, end: { line: 9, character: 0 } },
  });
  check(
    "inferred type inlay hint",
    hints.result.some((hint) => hint.label === ": int"),
    JSON.stringify(hints.result),
  );
  check(
    "function return type inlay hint",
    hints.result.some(
      (hint) =>
        hint.label === ": unit" &&
        hint.position.line === 0 &&
        hint.position.character === 11,
    ),
    JSON.stringify(hints.result),
  );



  const importsUri = `file://${join(root, "test/lsp-imports.yack")}`;
  const importsText = `import yack/list
import yack/option
import yack/string

fun main () =
    let val x = 1 + 2
    in print_int (x)
    end
`;
  await client.open(importsUri, importsText);
  const importsHints = await client.request("textDocument/inlayHint", {
    textDocument: { uri: importsUri },
    range: { start: { line: 0, character: 0 }, end: { line: 9, character: 0 } },
  });
  check(
    "inlay hints survive several imported modules",
    Array.isArray(importsHints.result) &&
      importsHints.result.some((hint) => hint.label === ": int"),
    JSON.stringify(importsHints.result ?? importsHints.error),
  );

  const argHintUri = "file:///tmp/yack-lsp-arg-hints.yack";
  const argHintText = `fun combine (left : int, right : int) : int = left + right

fun main () =
    print_int (combine (1, 2))
`;
  await client.open(argHintUri, argHintText);
  const argHints = await client.request("textDocument/inlayHint", {
    textDocument: { uri: argHintUri },
    range: { start: { line: 0, character: 0 }, end: { line: 6, character: 0 } },
  });
  check(
    "call-site hints name the arguments",
    argHints.result.some((hint) => hint.label === "left:") &&
      argHints.result.some((hint) => hint.label === "right:"),
    JSON.stringify(argHints.result),
  );

  const detailUri = "file:///tmp/yack-lsp-type-detail.yack";
  const detailText = `fun main () =
    let val amount = 1 + 2
    in amou
    end
`;
  await client.open(detailUri, detailText);
  const detailLine = detailText.split("\n")[2];
  const detailCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: detailUri },
    position: { line: 2, character: detailLine.length },
  });
  const amountItem = detailCompletion.result.items.find(
    (item) => item.label === "amount",
  );
  check(
    "local completion shows the inferred type",
    amountItem?.detail === "int",
    JSON.stringify(amountItem),
  );



  const featureFile = join(root, "examples/stdlib/src/lsp-features.yack");
  const featureUri = `file://${featureFile}`;
  const featureText = `import yack/json
import yack/list

type pair : Copy = { left : int, right : int }

fun combine (left : int, right : int) : int = left + right

fun main () =
    let val total = combine (1, 2)
    in list.map (x => combine (x, total), Nil)
    end
`;
  await client.open(featureUri, featureText);
  const featureLines = featureText.split("\n");
  const featurePosition = (line, needle, extra = 0) => ({
    line,
    character: featureLines[line].indexOf(needle) + needle.length + extra,
  });

  const signature = await client.request("textDocument/signatureHelp", {
    textDocument: { uri: featureUri },
    position: featurePosition(8, "combine (1, "),
  });
  const signatureItem = signature.result?.signatures?.[0];
  check(
    "signature help shows the callee and its parameters",
    signatureItem?.label.includes("combine") &&
      signatureItem.parameters?.length === 2 &&
      signature.result.activeParameter === 1,
    JSON.stringify(signature.result),
  );




  const builtinSignatureUri = "file:///tmp/yack-lsp-builtin-signature.yack";
  await client.open(
    builtinSignatureUri,
    "fun main () =\n    print_int (1)\n",
  );
  client.notify("textDocument/didChange", {
    textDocument: { uri: builtinSignatureUri, version: 2 },
    contentChanges: [{ text: "fun main () =\n    print_int (\n" }],
  });
  const builtinSignature = await client.request("textDocument/signatureHelp", {
    textDocument: { uri: builtinSignatureUri },
    position: { line: 1, character: 15 },
  });
  check(
    "signature help covers builtins from the last checked analysis",
    builtinSignature.result?.signatures?.[0]?.label.includes("print_int") ===
      true &&
      builtinSignature.result?.signatures?.[0]?.label.includes("value : int") ===
        true,
    JSON.stringify(builtinSignature.result),
  );
  const builtinCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: builtinSignatureUri },
    position: { line: 1, character: 15 },
  });
  const builtinItem = builtinCompletion.result.items.find(
    (item) => item.label === "print_int",
  );
  check(
    "builtin completions show named parameters",
    builtinItem?.detail.includes("value : int") === true,
    JSON.stringify(builtinItem),
  );

  const documentSymbols = await client.request("textDocument/documentSymbol", {
    textDocument: { uri: featureUri },
  });
  const symbolNames = (documentSymbols.result ?? []).map((symbol) => symbol.name);
  check(
    "document symbols list functions and types",
    symbolNames.includes("combine") &&
      symbolNames.includes("main") &&
      symbolNames.includes("pair") &&
      !symbolNames.includes(""),
    symbolNames.join(","),
  );

  const workspaceSymbols = await client.request("workspace/symbol", {
    query: "parse",
  });
  check(
    "workspace symbols search the module index",
    (workspaceSymbols.result ?? []).some(
      (symbol) =>
        symbol.name === "parse" &&
        typeof symbol.containerName === "string" &&
        symbol.location.uri.endsWith(".yack"),
    ),
    JSON.stringify(workspaceSymbols.result?.slice(0, 3)),
  );

  const semantic = await client.request("textDocument/semanticTokens/full", {
    textDocument: { uri: featureUri },
  });
  const legend = capabilities.semanticTokensProvider.legend.tokenTypes;
  const semanticTokens = [];
  {
    const data = semantic.result?.data ?? [];
    let line = 0;
    let character = 0;
    for (let i = 0; i + 4 < data.length; i += 5) {
      line += data[i];
      character = data[i] === 0 ? character + data[i + 1] : data[i + 1];
      semanticTokens.push({
        line,
        character,
        length: data[i + 2],
        type: legend[data[i + 3]],
      });
    }
  }
  check(
    "semantic tokens classify functions and keywords",
    semantic.result?.data.length % 5 === 0 &&
      semanticTokens.some(
        (token) => token.type === "function" && token.line === 5,
      ) &&
      semanticTokens.some((token) => token.type === "keyword") &&
      semanticTokens.some((token) => token.type === "struct"),
    JSON.stringify(semanticTokens.slice(0, 6)),
  );

  const links = await client.request("textDocument/documentLink", {
    textDocument: { uri: featureUri },
  });
  check(
    "document links target imported modules",
    links.result?.length === 2 &&
      links.result.some((link) => link.target.endsWith("/lib/yack/src/json.yack")) &&
      links.result.some((link) => link.target.endsWith("/lib/yack/src/list.yack")),
    JSON.stringify(links.result),
  );

  const importDefinition = await client.request("textDocument/definition", {
    textDocument: { uri: featureUri },
    position: {
      line: 1,
      character: featureLines[1].indexOf("list"),
    },
  });
  check(
    "definition on an import path opens the module",
    String(importDefinition.result?.uri).endsWith("/lib/yack/src/list.yack") &&
      importDefinition.result?.range.start.line === 0,
    JSON.stringify(importDefinition.result),
  );

  const qualifierDefinition = await client.request("textDocument/definition", {
    textDocument: { uri: featureUri },
    position: { line: 9, character: featureLines[9].indexOf("list") + 1 },
  });
  check(
    "definition on a module qualifier opens the module",
    String(qualifierDefinition.result?.uri).endsWith("/lib/yack/src/list.yack") &&
      qualifierDefinition.result?.range.start.line === 0,
    JSON.stringify(qualifierDefinition.result),
  );

  const declaration = await client.request("textDocument/declaration", {
    textDocument: { uri: featureUri },
    position: { line: 9, character: featureLines[9].indexOf("map") + 1 },
  });
  check(
    "declaration follows the same targets as definition",
    String(declaration.result?.uri).endsWith("/lib/yack/src/list.yack"),
    JSON.stringify(declaration.result),
  );



  const crossUri = `file://${join(root, "examples/stdlib/src/lsp-cross-rename.yack")}`;
  const crossText = `import yack/json

fun main () =
    case json.parse ("{}") of
        _ => ()
`;
  await client.open(crossUri, crossText);
  const crossLines = crossText.split("\n");
  const crossPosition = {
    line: 3,
    character: crossLines[3].indexOf("parse") + 1,
  };
  const crossPrepare = await client.request("textDocument/prepareRename", {
    textDocument: { uri: crossUri },
    position: crossPosition,
  });
  check(
    "prepare rename uses the source spelling",
    crossPrepare.result?.placeholder === "parse",
    JSON.stringify(crossPrepare.result),
  );
  const crossRename = await client.request("textDocument/rename", {
    textDocument: { uri: crossUri },
    position: crossPosition,
    newName: "parse_json",
  });
  const crossChanges = crossRename.result?.changes ?? {};
  const crossUris = Object.keys(crossChanges);
  const crossEdits = crossChanges[crossUri] ?? [];
  const jsonEdits =
    crossChanges[
      crossUris.find((candidate) => candidate.endsWith("/lib/yack/src/json.yack"))
    ] ?? [];


  check(
    "rename updates the declaration and the qualified call across files",
    crossUris.length === 2 &&
      crossEdits.length === 1 &&
      crossEdits[0].newText === "parse_json" &&
      crossEdits[0].range.start.character === crossLines[3].indexOf("parse") &&
      jsonEdits.length >= 1 &&
      jsonEdits.every((edit) => edit.newText === "parse_json"),
    JSON.stringify(crossChanges),
  );
  const crossReferences = await client.request("textDocument/references", {
    textDocument: { uri: crossUri },
    position: crossPosition,
    context: { includeDeclaration: true },
  });
  const referencedUris = new Set(
    (crossReferences.result ?? []).map((location) => location.uri),
  );
  check(
    "references include the module declaration and the caller",
    referencedUris.size >= 2 &&
      [...referencedUris].some((candidate) =>
        candidate.endsWith("/lib/yack/src/json.yack"),
      ),
    JSON.stringify(crossReferences.result),
  );

  const semanticRange = await client.request(
    "textDocument/semanticTokens/range",
    {
      textDocument: { uri: featureUri },
      range: {
        start: { line: 5, character: 0 },
        end: { line: 5, character: 100 },
      },
    },
  );
  {
    const data = semanticRange.result?.data ?? [];
    let line = 0;
    let character = 0;
    let onlyRequestedLine = true;
    for (let i = 0; i + 4 < data.length; i += 5) {
      line += data[i];
      character = data[i] === 0 ? character + data[i + 1] : data[i + 1];
      if (line !== 5) onlyRequestedLine = false;
    }
    check(
      "semantic tokens range returns only the requested lines",
      data.length > 0 && onlyRequestedLine,
      JSON.stringify(data.slice(0, 10)),
    );
  }

  const rangeFormatUri = "file:///tmp/yack-lsp-range-format.yack";
  const rangeFormatText = "fun main () = let val x = 1 in print_int (x) end\n";
  await client.open(rangeFormatUri, rangeFormatText);
  const rangeFormatted = await client.request(
    "textDocument/rangeFormatting",
    {
      textDocument: { uri: rangeFormatUri },
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: rangeFormatText.length - 1 },
      },
      options: { tabSize: 4, insertSpaces: true },
    },
  );
  check(
    "range formatting edits only the changed region",
    rangeFormatted.result?.length === 1 &&
      rangeFormatted.result[0].newText.includes("let") &&
      rangeFormatted.result[0].range.start.line === 0,
    JSON.stringify(rangeFormatted.result),
  );

  const callHierarchy = await client.request(
    "textDocument/prepareCallHierarchy",
    {
      textDocument: { uri: featureUri },
      position: { line: 5, character: featureLines[5].indexOf("combine") + 1 },
    },
  );
  const combineItem = callHierarchy.result?.[0];
  check(
    "prepare call hierarchy finds the function",
    combineItem?.name === "combine" && combineItem?.kind === 12,
    JSON.stringify(callHierarchy.result),
  );
  const incomingCalls = await client.request("callHierarchy/incomingCalls", {
    item: combineItem,
  });
  check(
    "incoming calls name the caller",
    incomingCalls.result?.length === 1 &&
      incomingCalls.result[0].from.name === "main",
    JSON.stringify(incomingCalls.result),
  );
  const mainHierarchy = await client.request(
    "textDocument/prepareCallHierarchy",
    {
      textDocument: { uri: featureUri },
      position: { line: 7, character: featureLines[7].indexOf("main") + 1 },
    },
  );
  const outgoingCalls = await client.request("callHierarchy/outgoingCalls", {
    item: mainHierarchy.result?.[0],
  });
  const outgoingNames = (outgoingCalls.result ?? []).map((call) => call.to.name);
  check(
    "outgoing calls include local and imported functions",
    outgoingNames.includes("combine") &&
      outgoingNames.includes("map") &&
      (outgoingCalls.result ?? []).some((call) =>
        call.to.uri.endsWith("/lib/yack/src/list.yack"),
      ),
    JSON.stringify(outgoingNames),
  );

  async function actionsFor(uri, text, code) {
    const published = await client.open(uri, text);
    const diagnostic =
      published.params.diagnostics.find((item) => item.code === code) ??
      published.params.diagnostics[0];
    const response = await client.request("textDocument/codeAction", {
      textDocument: { uri },
      range: diagnostic?.range ?? {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 0 },
      },
      context: { diagnostics: diagnostic ? [diagnostic] : [] },
    });
    return { diagnostic, actions: response.result ?? [] };
  }

  const typoFile = `file://${join(root, "examples/stdlib/src/lsp-typo.yack")}`;
  const typo = await actionsFor(
    typoFile,
    "fun main () = print_nt (1)\n",
    "E3002",
  );
  check(
    "code action suggests the closest builtin",
    typo.actions.some((action) => action.title === "Change to `print_int`"),
    JSON.stringify(typo.actions.map((action) => action.title)),
  );

  const importFile = `file://${join(root, "examples/stdlib/src/lsp-import-action.yack")}`;
  const imported = await actionsFor(
    importFile,
    'fun main () = trim ("x")\n',
    "E3002",
  );
  const importAction = imported.actions.find((action) =>
    action.title.startsWith("Import `trim` from `yack/string`"),
  );
  check(
    "code action imports an exported function",
    importAction !== undefined &&
      JSON.stringify(importAction.edit.changes[importFile]).includes(
        "import yack/string.{trim}",
      ),
    JSON.stringify(imported.actions.map((action) => action.title)),
  );

  const callbackFile = `file://${join(root, "examples/stdlib/src/lsp-create-fn.yack")}`;
  const createdCallback = await actionsFor(
    callbackFile,
    "import yack/list\n\nfun main () = list.map (transform, Nil)\n",
    "E3001",
  );
  const createAction = createdCallback.actions.find((action) =>
    action.title.startsWith("Create function `transform ("),
  );
  check(
    "code action creates a missing callback with its arguments",
    createAction !== undefined &&
      createAction.title.includes(" : ") &&
      JSON.stringify(createAction.edit.changes[callbackFile]).includes(
        "fun transform (x :",
      ),
    JSON.stringify(createdCallback.actions.map((action) => action.title)),
  );

  const unusedFile = `file://${join(root, "examples/stdlib/src/lsp-unused.yack")}`;
  const unused = await actionsFor(
    unusedFile,
    "fun unused_helper (x : int) : int = x\n\nfun main () = print_int (1)\n",
    "W3001",
  );
  check(
    "code action removes an unused function",
    unused.actions.some(
      (action) =>
        action.title === "Remove unused function `unused_helper`" &&
        action.edit.changes[unusedFile]?.[0]?.newText === "",
    ),
    JSON.stringify(unused.actions.map((action) => action.title)),
  );

  const annotateFile = `file://${join(root, "examples/stdlib/src/lsp-annotate.yack")}`;
  const annotateText = `fun main () =
    let val count = 1
    in print_int (count)
    end
`;
  const annotatePublished = await client.open(annotateFile, annotateText);
  const annotatePosition = {
    line: 1,
    character: annotateText.split("\n")[1].indexOf("count") + 1,
  };
  const annotate = await client.request("textDocument/codeAction", {
    textDocument: { uri: annotateFile },
    range: { start: annotatePosition, end: annotatePosition },
    context: { diagnostics: annotatePublished.params.diagnostics },
  });
  check(
    "refactor action adds an inferred type annotation",
    (annotate.result ?? []).some(
      (action) =>
        action.title === "Add type annotation `: int`" &&
        action.kind === "refactor.rewrite" &&
        action.edit.changes[annotateFile]?.[0]?.newText === " : int",
    ),
    JSON.stringify(annotate.result),
  );

  const patternFile = `file://${join(root, "examples/stdlib/src/lsp-pattern.yack")}`;
  const patternText = `datatype colour = Red | Blue

fun main () =
    let val colour_value = Red
    in
        colour_value
    end
`;
  await client.open(patternFile, patternText);
  const patternPosition = {
    line: 5,
    character: patternText.split("\n")[5].indexOf("colour_value") + 1,
  };
  const pattern = await client.request("textDocument/codeAction", {
    textDocument: { uri: patternFile },
    range: { start: patternPosition, end: patternPosition },
    context: { diagnostics: [] },
  });
  const patternAction = (pattern.result ?? []).find((action) =>
    action.title.includes("Pattern match"),
  );
  const patternEdit = patternAction?.edit?.changes?.[patternFile]?.[0]?.newText;
  check(
    "refactor action expands a value into a full case",
    typeof patternEdit === "string" &&
      patternEdit.includes("case colour_value of") &&
      patternEdit.includes("Red => ()") &&
      patternEdit.includes("| Blue => ()"),
    JSON.stringify(pattern.result),
  );

  const arityFile = `file://${join(root, "examples/stdlib/src/lsp-arity.yack")}`;
  const arity = await actionsFor(
    arityFile,
    "fun main () = print_int ()\n",
    "E3003",
  );
  check(
    "code action fills missing call arguments",
    arity.actions.some(
      (action) =>
        action.title === "Add 1 missing argument" &&
        action.edit.changes[arityFile]?.[0]?.newText === "0",
    ),
    JSON.stringify(arity.actions.map((action) => action.title)),
  );

  const convertFile = `file://${join(root, "examples/stdlib/src/lsp-convert.yack")}`;
  const convert = await actionsFor(
    convertFile,
    "fun main () = print_string (1)\n",
    "E3004",
  );
  check(
    "code action wraps an int argument for a string parameter",
    convert.actions.some(
      (action) =>
        action.title === "Convert with `int_to_string`" &&
        action.edit.changes[convertFile]?.[0]?.newText === "int_to_string (1)",
    ),
    JSON.stringify(convert.actions.map((action) => action.title)),
  );

  const borrowWrapFile = `file://${join(root, "examples/stdlib/src/lsp-borrow-wrap.yack")}`;
  const borrowWrap = await actionsFor(
    borrowWrapFile,
    'fun take (text : borrow string) : int = 0\n\nfun main () = take ("x")\n',
    "E3004",
  );
  check(
    "code action borrows an owned argument",
    borrowWrap.actions.some(
      (action) =>
        action.title === "Wrap in `borrow (...)`" &&
        action.edit.changes[borrowWrapFile]?.[0]?.newText === 'borrow ("x")',
    ),
    JSON.stringify(borrowWrap.actions.map((action) => action.title)),
  );

  const recordFile = `file://${join(root, "examples/stdlib/src/lsp-record-field.yack")}`;
  const record = await actionsFor(
    recordFile,
    "type point : Copy = { x : int, y : int }\n\nfun main () = let val p : point = { x = 1 } in p.x end\n",
    "E3004",
  );
  check(
    "code action adds a missing record field",
    record.actions.some(
      (action) =>
        action.title === "Add missing record field `y`" &&
        action.edit.changes[recordFile]?.[0]?.newText === ", y = 0",
    ),
    JSON.stringify(record.actions.map((action) => action.title)),
  );

  const tupleFile = `file://${join(root, "examples/stdlib/src/lsp-tuple-pattern.yack")}`;
  const tuple = await actionsFor(
    tupleFile,
    "fun first (pair : int * int * int) : int = pair.0\n\nfun main () = let val (a, b) = (1, 2, 3) in a end\n",
    "E3019",
  );
  check(
    "code action completes a tuple pattern",
    tuple.actions.some(
      (action) =>
        action.title.includes("missing tuple element") &&
        action.edit.changes[tupleFile]?.[0]?.newText === ", _",
    ),
    JSON.stringify(tuple.actions.map((action) => action.title)),
  );

  const constructorFile = `file://${join(root, "examples/stdlib/src/lsp-constructor.yack")}`;
  const constructor = await actionsFor(
    constructorFile,
    "datatype colour = Red | Blue\n\nfun main () = let val c = Red (1) in c end\n",
    "E3026",
  );
  check(
    "code action removes an unexpected constructor argument",
    constructor.actions.some(
      (action) =>
        action.title === "Remove the constructor argument" &&
        action.edit.changes[constructorFile]?.[0]?.newText === "Red",
    ),
    JSON.stringify(constructor.actions.map((action) => action.title)),
  );

  const destructureFile = `file://${join(root, "examples/stdlib/src/lsp-destructure.yack")}`;
  const destructureText = `type point : Copy = { x : int, y : int }

fun make () : point = { x = 1, y = 2 }

fun pair () : int * string = (1, "a")

fun main () =
    let val p = make ()
        val q = pair ()
    in print_int (p.x)
    end
`;
  await client.open(destructureFile, destructureText);
  async function destructureAt(line) {
    const position = {
      line,
      character: destructureText.split("\n")[line].indexOf("val ") + 5,
    };
    const response = await client.request("textDocument/codeAction", {
      textDocument: { uri: destructureFile },
      range: { start: position, end: position },
      context: { diagnostics: [] },
    });
    return (response.result ?? []).find((action) =>
      action.title.startsWith("Destructure into"),
    );
  }
  const recordDestructure = await destructureAt(7);
  const tupleDestructure = await destructureAt(8);
  check(
    "refactor action destructures a record",
    recordDestructure?.title === "Destructure into `{ x = x, y = y }`" &&
      recordDestructure?.edit.changes[destructureFile]?.[0]?.newText ===
        "{ x = x, y = y }",
    JSON.stringify(recordDestructure),
  );
  check(
    "refactor action destructures a tuple",
    tupleDestructure?.title === "Destructure into `(x, y)`" &&
      tupleDestructure?.edit.changes[destructureFile]?.[0]?.newText ===
        "(x, y)",
    JSON.stringify(tupleDestructure),
  );


  const deltaUri = "file:///tmp/yack-lsp-semantic-delta.yack";
  await client.open(deltaUri, "fun main () =\n    print_int (1)\n");
  const fullTokens1 = await client.request(
    "textDocument/semanticTokens/full",
    { textDocument: { uri: deltaUri } },
  );
  client.notify("textDocument/didChange", {
    textDocument: { uri: deltaUri, version: 2 },
    contentChanges: [
      {
        range: {
          start: { line: 1, character: 15 },
          end: { line: 1, character: 16 },
        },
        text: "42",
      },
    ],
  });
  const delta = await client.request(
    "textDocument/semanticTokens/full/delta",
    {
      textDocument: { uri: deltaUri },
      previousResultId: fullTokens1.result?.resultId,
    },
  );
  const appliedTokens = [...(fullTokens1.result?.data ?? [])];
  for (const edit of delta.result?.edits ?? []) {
    appliedTokens.splice(edit.start, edit.deleteCount, ...(edit.data ?? []));
  }
  const fullTokens2 = await client.request(
    "textDocument/semanticTokens/full",
    { textDocument: { uri: deltaUri } },
  );
  check(
    "semantic token delta applies to the previous array",
    Array.isArray(delta.result?.edits) &&
      (delta.result.edits.length > 0) &&
      JSON.stringify(appliedTokens) ===
        JSON.stringify(fullTokens2.result?.data),
    JSON.stringify(delta.result),
  );


  const resolveUri = "file:///tmp/yack-lsp-resolve.yack";
  const resolveText = `fun combine (left : int, right : int) : int = left + right

fun main () = print_int (combine (1, 2))
`;
  await client.open(resolveUri, resolveText);
  const resolveCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: resolveUri },
    position: { line: 2, character: resolveText.split("\n")[2].indexOf("(1") + 1 },
  });
  const rawCombine = resolveCompletion.result.items.find(
    (item) => item.label === "combine",
  );
  const stripped = { ...rawCombine };
  delete stripped.documentation;
  const resolved = await client.request("completionItem/resolve", stripped);
  check(
    "completion item resolve fills documentation",
    resolved.result?.documentation?.value?.includes("fun combine") === true,
    JSON.stringify(resolved.result),
  );


  const unsavedUri = `file://${join(root, "examples/stdlib/src/lsp-unsaved.yack")}`;
  await client.open(
    unsavedUri,
    "fun unsaved_magic () : int = 42\n",
  );
  const unsavedSymbols = await client.request("workspace/symbol", {
    query: "unsaved_magic",
  });
  check(
    "workspace symbols include unsaved buffers",
    (unsavedSymbols.result ?? []).some(
      (symbol) =>
        symbol.name === "unsaved_magic" &&
        symbol.location.uri === unsavedUri,
    ),
    JSON.stringify(unsavedSymbols.result),
  );

  const typeDefUri = `file://${join(root, "examples/stdlib/src/lsp-type-def.yack")}`;
  const typeDefText = `type pair : Copy = { left : int, right : int }

fun use_pair (p : pair) : int = p.left
`;
  await client.open(typeDefUri, typeDefText);
  const typeDef = await client.request("textDocument/typeDefinition", {
    textDocument: { uri: typeDefUri },
    position: { line: 2, character: typeDefText.split("\n")[2].indexOf("(p") + 1 },
  });
  check(
    "type definition jumps to the declared type",
    typeDef.result?.uri === typeDefUri && typeDef.result?.range.start.line === 0,
    JSON.stringify(typeDef.result),
  );

  const fieldDefinition = await client.request("textDocument/definition", {
    textDocument: { uri: typeDefUri },
    position: {
      line: 2,
      character: typeDefText.split("\n")[2].indexOf("p.left") + 2,
    },
  });
  check(
    "definition on a record field targets the field declaration",
    fieldDefinition.result?.uri === typeDefUri &&
      fieldDefinition.result?.range.start.line === 0 &&
      fieldDefinition.result?.range.start.character ===
        typeDefText.split("\n")[0].indexOf("left"),
    JSON.stringify(fieldDefinition.result),
  );

  const folding = await client.request("textDocument/foldingRange", {
    textDocument: { uri: featureUri },
  });
  check(
    "folding range covers the multi-line function",
    (folding.result ?? []).some(
      (range) => range.startLine === 7 && range.endLine >= 9,
    ),
    JSON.stringify(folding.result),
  );

  const localDefinition = await client.request("textDocument/definition", {
    textDocument: { uri: featureUri },
    position: featurePosition(8, "combine"),
  });
  check(
    "definition jumps to the declaration in the file",
    localDefinition.result?.uri === featureUri &&
      localDefinition.result?.range.start.line === 5,
    JSON.stringify(localDefinition.result),
  );

  const moduleDefinition = await client.request("textDocument/definition", {
    textDocument: { uri: featureUri },
    position: featurePosition(9, "list.map"),
  });
  check(
    "definition follows a qualified module member",
    String(moduleDefinition.result?.uri).endsWith("/lib/yack/src/list.yack"),
    JSON.stringify(moduleDefinition.result),
  );

  const qualifiedHover = await client.request("textDocument/hover", {
    textDocument: { uri: featureUri },
    position: featurePosition(9, "list.map"),
  });
  check(
    "hover resolves a qualified module member",
    markdownContents(qualifiedHover.result).includes("fun map"),
    markdownContents(qualifiedHover.result),
  );

  const references = await client.request("textDocument/references", {
    textDocument: { uri: featureUri },
    position: featurePosition(5, "combine"),
    context: { includeDeclaration: true },
  });
  check(
    "references list the declaration and its calls",
    references.result?.length === 3 &&
      references.result.every((location) => location.uri === featureUri),
    JSON.stringify(references.result),
  );

  const highlights = await client.request("textDocument/documentHighlight", {
    textDocument: { uri: featureUri },
    position: featurePosition(5, "combine"),
  });
  check(
    "document highlights mark the binding occurrences",
    highlights.result?.length === 3 &&
      highlights.result.every((highlight) => highlight.kind === 1),
    JSON.stringify(highlights.result),
  );

  const formatted = await client.request("textDocument/formatting", {
    textDocument: { uri: featureUri },
    options: { tabSize: 4, insertSpaces: true },
  });
  const formatEdit = formatted.result?.[0];
  check(
    "formatting returns one full-document edit",
    formatted.result?.length === 1 &&
      formatEdit.newText !== featureText &&
      formatEdit.newText.includes("list.map (") &&
      formatEdit.range.start.line === 0,
    JSON.stringify(formatted.result),
  );

  const hover = await client.request("textDocument/hover", {
    textDocument: { uri: simpleUri },
    position: { line: 1, character: 13 },
  });
  check("hover inferred type", markdownContents(hover.result).includes("int"));

  const brokenText = `fun classify (n) =\n    case n of\n        0 => 10\n      | 1 => 11\n\nfun main () =\n    print_int (classify (0) + "x")\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: simpleUri, version: 2 },
    contentChanges: [{ text: brokenText }],
  });
  const broken = await client.wait(
    (message) =>
      message.method === "textDocument/publishDiagnostics" &&
      message.params.uri === simpleUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === "E3021"),
    "type and coverage diagnostics",
  );
  check(
    "check diagnostics published",
    broken.params.diagnostics.some((diagnostic) => diagnostic.code === "E3004"),
  );
  const missingCase = broken.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E3021",
  );
  const actions = await client.request("textDocument/codeAction", {
    textDocument: { uri: simpleUri },
    range: missingCase.range,
    context: { diagnostics: [missingCase] },
  });
  const wildcard = actions.result.find((action) =>
    action.title.includes("wildcard"),
  );
  const edit = wildcard?.edit?.changes?.[simpleUri]?.[0];
  check(
    "quick fix adds wildcard branch",
    edit !== undefined && edit.newText.includes("| _ => 0"),
    JSON.stringify(actions.result),
  );

  const affineText = `type handle : Affine = { id : token }\nfun main () = ()\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: simpleUri, version: 3 },
    contentChanges: [{ text: affineText }],
  });
  const affine = await client.wait(
    (message) =>
      message.method === "textDocument/publishDiagnostics" &&
      message.params.uri === simpleUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === "E3016"),
    "affine linear diagnostic",
  );
  const linear = affine.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E3016",
  );
  const linearActions = await client.request("textDocument/codeAction", {
    textDocument: { uri: simpleUri },
    range: linear.range,
    context: { diagnostics: [linear] },
  });
  const linearEdit =
    linearActions.result[0]?.edit?.changes?.[simpleUri]?.[0];
  check(
    "quick fix declares a Linear type",
    linearEdit !== undefined && linearEdit.newText === "Linear",
    JSON.stringify(linearActions.result),
  );

  const callbackText = `fun eat (text : string) : unit = ()\nfun apply (f : string -> unit, text : string) : unit = f (text)\nfun main () =\n    apply (eat, "a" ^ "b")\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: simpleUri, version: 4 },
    contentChanges: [{ text: callbackText }],
  });
  const callback = await client.wait(
    (message) =>
      message.method === "textDocument/publishDiagnostics" &&
      message.params.uri === simpleUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === "E0513"),
    "callback ownership diagnostic",
  );
  const callbackDiagnostic = callback.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E0513",
  );
  const callbackActions = await client.request("textDocument/codeAction", {
    textDocument: { uri: simpleUri },
    range: callbackDiagnostic.range,
    context: { diagnostics: [callbackDiagnostic] },
  });
  const consumeAction = callbackActions.result.find((action) =>
    action.title.includes("consume"),
  );
  const consumeEdit = consumeAction?.edit?.changes?.[simpleUri]?.[0];
  check(
    "quick fix marks callback consume",
    consumeEdit !== undefined &&
      consumeEdit.newText === "consume " &&
      consumeEdit.range.start.line === 1,
    JSON.stringify(callbackActions.result),
  );

  const lambdaText = `fun eat (text : string) : unit = ()\nfun apply (f : string -> unit, text : string) : unit = f (text)\nfun main () =\n    apply (x => eat (x), "a" ^ "b")\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: simpleUri, version: 5 },
    contentChanges: [{ text: lambdaText }],
  });
  const lambda = await client.wait(
    (message) =>
      message.method === "textDocument/publishDiagnostics" &&
      message.params.uri === simpleUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === "E0512"),
    "lambda ownership diagnostic",
  );
  const lambdaDiagnostic = lambda.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E0512",
  );
  const lambdaActions = await client.request("textDocument/codeAction", {
    textDocument: { uri: simpleUri },
    range: lambdaDiagnostic.range,
    context: { diagnostics: [lambdaDiagnostic] },
  });
  const lambdaConsume = lambdaActions.result.find((action) =>
    action.title.includes("consume"),
  );
  const lambdaEdit = lambdaConsume?.edit?.changes?.[simpleUri]?.[0];
  check(
    "quick fix marks lambda callback consume",
    lambdaEdit !== undefined &&
      lambdaEdit.newText === "consume " &&
      lambdaEdit.range.start.line === 1,
    JSON.stringify(lambdaActions.result),
  );

  const stdlibUri = `file://${stdlibFile}`;
  const stdlibText = `import yack/json\nimport yack/string\n\nfun main () =\n    let val t = "x"\n    in json.\n`;
  await client.open(stdlibUri, stdlibText);
  const members = await client.request("textDocument/completion", {
    textDocument: { uri: stdlibUri },
    position: { line: 5, character: 12 },
  });
  const parseItem = members.result.items.find((item) => item.label === "parse");
  check(
    "qualified module completion",
    parseItem !== undefined,
    members.result.items.slice(0, 8).map((item) => item.label).join(","),
  );
  check(
    "member completion replaces after the dot",
    parseItem?.textEdit?.newText?.startsWith("parse ("),
    JSON.stringify(parseItem?.textEdit),
  );
  check(
    "function completion inserts a named-argument call snippet",
    parseItem?.insertTextFormat === 2 &&
      parseItem?.textEdit?.newText?.includes("${1:"),
    JSON.stringify(parseItem),
  );

  const optionFile = join(root, "examples/stdlib/src/option-completion.yack");
  const optionUri = `file://${optionFile}`;
  const optionText = `import yack/option\n\nfun main () =\n    let val value = option.\n    in ()\n    end\n`;
  await client.open(optionUri, optionText);
  const optionColumn = optionText.split("\n")[3].indexOf("option.") + 7;
  const optionMembers = await client.request("textDocument/completion", {
    textDocument: { uri: optionUri },
    position: { line: 3, character: optionColumn },
  });
  const unwrapItem = optionMembers.result.items.find(
    (item) => item.label === "unwrap_or",
  );
  check(
    "function call snippet uses parameter names",
    unwrapItem?.insertTextFormat === 2 &&
      unwrapItem?.textEdit?.newText === "unwrap_or (${1:value}, ${2:default})",
    JSON.stringify(unwrapItem?.textEdit),
  );

  const hofFile = join(root, "examples/stdlib/src/hof-completion.yack");
  const hofUri = `file://${hofFile}`;
  const hofText = `import yack/list\n\nfun main () =\n    let val values = 1 :: Nil\n    in list.map (\n    end\n`;
  await client.open(hofUri, hofText);
  const hofLine = hofText.split("\n")[4];
  const hofMembers = await client.request("textDocument/completion", {
    textDocument: { uri: hofUri },
    position: { line: 4, character: hofLine.length },
  });
  const inlineItem = hofMembers.result.items.find(
    (item) => item.label === "x => …",
  );
  check(
    "callback position suggests inline function shape",
    inlineItem?.insertText === "x => ",
    hofMembers.result.items.slice(0, 8).map((item) => item.label).join(","),
  );

  const autoImportFile = join(root, "examples/stdlib/src/autoimport.yack");
  const autoImportUri = `file://${autoImportFile}`;
  const autoImportText = `fun main () =\n    let val name = os.\n    in ()\n    end\n`;
  await client.open(autoImportUri, autoImportText);
  const autoImportColumn = autoImportText.split("\n")[1].indexOf("os.") + 3;
  const autoImportMembers = await client.request("textDocument/completion", {
    textDocument: { uri: autoImportUri },
    position: { line: 1, character: autoImportColumn },
  });
  const autoHome = autoImportMembers.result.items.find(
    (item) => item.label === "home",
  );
  check(
    "unimported qualifier completion",
    autoHome !== undefined,
    autoImportMembers.result.items.slice(0, 8).map((item) => item.label).join(","),
  );
  check(
    "completion auto-imports the module",
    autoHome?.additionalTextEdits?.[0]?.newText === "import yack/os\n",
    JSON.stringify(autoHome?.additionalTextEdits),
  );

  const importedFile = join(root, "examples/stdlib/src/imported-os.yack");
  const importedUri = `file://${importedFile}`;
  const importedText = `import yack/os\n\nfun main () =\n    let val name = os.\n    in ()\n    end\n`;
  await client.open(importedUri, importedText);
  const importedColumn = importedText.split("\n")[3].indexOf("os.") + 3;
  const importedMembers = await client.request("textDocument/completion", {
    textDocument: { uri: importedUri },
    position: { line: 3, character: importedColumn },
  });
  const importedHome = importedMembers.result.items.find(
    (item) => item.label === "home",
  );
  check(
    "already imported module adds no import edit",
    importedHome !== undefined && importedHome.additionalTextEdits === undefined,
    JSON.stringify(importedHome),
  );



  const dependencyFile = join(
    root,
    "examples/alpage_tutorial/src/module-suggestion.yack",
  );
  const dependencyUri = `file://${dependencyFile}`;
  const dependencyText = `fun main () =\n    let val v = Ct\n    in ()\n    end\n`;
  await client.open(dependencyUri, dependencyText);
  const dependencyCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: dependencyUri },
    position: { line: 1, character: dependencyText.split("\n")[1].length },
  });
  const ctxModule = dependencyCompletion.result.items.find(
    (item) => item.label === "Ctx",
  );
  check(
    "unimported dependency module is suggested with an auto-import",
    ctxModule?.additionalTextEdits?.[0]?.newText === "import alpage/Ctx\n" &&
      ctxModule?.textEdit?.newText === "Ctx",
    JSON.stringify(ctxModule),
  );

  client.notify("textDocument/didChange", {
    textDocument: { uri: stdlibUri, version: 2 },
    contentChanges: [{ text: `import yack/\n\nfun main () =\n    ()\n` }],
  });
  const imports = await client.request("textDocument/completion", {
    textDocument: { uri: stdlibUri },
    position: { line: 0, character: 12 },
  });
  const jsonImport = imports.result.items.find(
    (item) => item.label === "yack/json",
  );
  check("import path completion", jsonImport !== undefined);
  check(
    "import completion replaces the whole module path",
    jsonImport?.textEdit?.newText === "yack/json",
    JSON.stringify(jsonImport?.textEdit),
  );

  const caseUri = "file:///tmp/yack-lsp-case.yack";
  const caseText = `datatype role = Simple of { name : string } | Admin of { name : string, level : int }\n\nfun label (r : role) : string =\n    case r of\n        \n`;
  await client.open(caseUri, caseText);
  const caseItems = await client.request("textDocument/completion", {
    textDocument: { uri: caseUri },
    position: { line: 4, character: 8 },
  });
  const simpleBranch = caseItems.result.items.find(
    (item) => item.label === "Simple",
  );
  const adminBranch = caseItems.result.items.find(
    (item) => item.label === "Admin",
  );
  check(
    "case completion proposes constructors",
    simpleBranch !== undefined &&
      adminBranch !== undefined &&
      !caseItems.result.items.some((item) => item.label === "None"),
    caseItems.result.items.map((item) => item.label).join(","),
  );
  check(
    "case completion inserts record pattern",
    simpleBranch?.textEdit?.newText?.includes("{name = name}") &&
      adminBranch?.textEdit?.newText?.includes("level = level"),
    JSON.stringify([simpleBranch?.textEdit, adminBranch?.textEdit]),
  );

  const recordText = `datatype role = Simple of { name : string } | Admin of { name : string, level : int }\n\nfun main () =\n    let val u = Simple ({ na\n    in ()\n    end\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: caseUri, version: 2 },
    contentChanges: [{ text: recordText }],
  });
  const recordItems = await client.request("textDocument/completion", {
    textDocument: { uri: caseUri },
    position: { line: 3, character: 28 },
  });
  const nameField = recordItems.result.items.find(
    (item) => item.label === "name",
  );
  check(
    "record payload completion proposes fields",
    nameField !== undefined &&
      nameField.textEdit?.newText === "name = " &&
      !recordItems.result.items.some((item) => item.label === "level"),
    recordItems.result.items.map((item) => item.label).join(","),
  );

  const aliasUri = "file:///tmp/yack-lsp-alias.yack";
  const aliasText = `type LanguageProp = { desc : string }
type Other = { name : string }

datatype Language =
    Yack of LanguageProp
  | Ocaml of LanguageProp

fun show (l : Language) : string =
    case l of
        Yack prop => prop.desc
      | Ocaml prop => prop.desc

fun main () =
    let val v = Yack ({ desc = "Coucou" })
    in print_string (show (v))
    end
`;
  await client.open(aliasUri, aliasText);
  const aliasItems = await client.request("textDocument/completion", {
    textDocument: { uri: aliasUri },
    position: { line: 13, character: 20 },
  });
  const aliasConstructor = aliasItems.result.items.find(
    (item) => item.label === "Yack",
  );
  check(
    "constructor completion expands a named record argument",
    aliasConstructor?.insertTextFormat === 2 &&
      aliasConstructor?.textEdit?.newText?.includes("{ desc = $1 }"),
    JSON.stringify(aliasConstructor),
  );
  const aliasFields = await client.request("textDocument/completion", {
    textDocument: { uri: aliasUri },
    position: { line: 13, character: 25 },
  });
  check(
    "named record payload completion proposes only its fields",
    aliasFields.result.items.some((item) => item.label === "desc") &&
      !aliasFields.result.items.some((item) => item.label === "name"),
    aliasFields.result.items.map((item) => item.label).join(","),
  );
  const aliasHints = await client.request("textDocument/inlayHint", {
    textDocument: { uri: aliasUri },
    range: { start: { line: 0, character: 0 }, end: { line: 20, character: 0 } },
  });
  check(
    "inlay hint expands a named record payload",
    aliasHints.result.some((hint) =>
      hint.label?.includes("LanguageProp = { desc : string }"),
    ),
    JSON.stringify(aliasHints.result),
  );
  const aliasHover = await client.request("textDocument/hover", {
    textDocument: { uri: aliasUri },
    position: { line: 9, character: 14 },
  });
  check(
    "hover expands a named record payload",
    markdownContents(aliasHover.result).includes(
      "LanguageProp = { desc : string }",
    ),
    JSON.stringify(aliasHover.result),
  );

  const aliasCaseUri = "file:///tmp/yack-lsp-alias-case.yack";
  const aliasCaseText = `type LanguageProp = { desc : string }
datatype Language = Yack of LanguageProp | Ocaml of LanguageProp

fun show (l : Language) =
    case l of

`;
  await client.open(aliasCaseUri, aliasCaseText);
  const aliasCaseItems = await client.request("textDocument/completion", {
    textDocument: { uri: aliasCaseUri },
    position: { line: 5, character: 8 },
  });
  const aliasBranch = aliasCaseItems.result.items.find(
    (item) => item.label === "Yack",
  );
  check(
    "case completion expands a named record pattern",
    aliasBranch?.textEdit?.newText?.includes("{desc = desc}"),
    JSON.stringify(aliasBranch),
  );

  const fieldUri = "file:///tmp/yack-lsp-fields.yack";
  const fieldText = `type LanguageProp = { desc : string }

datatype Language =
    Yack of LanguageProp
  | Ocaml of LanguageProp

fun print_desc (langs : Language list) =
    case langs of
        [] => ()
      | lang :: rest =>
          let val _ = print_string (lang.)
          in print_desc (rest)
          end

fun main () =
    let val v = Yack ({ desc = "Coucou" })
    in print_string (v.)
    end
`;
  await client.open(fieldUri, fieldText);
  const listFieldItems = await client.request("textDocument/completion", {
    textDocument: { uri: fieldUri },
    position: { line: 10, character: 41 },
  });
  check(
    "field completion follows a list element type",
    listFieldItems.result.items.some((item) => item.label === "desc"),
    JSON.stringify(listFieldItems.result.items),
  );
  const valueFieldItems = await client.request("textDocument/completion", {
    textDocument: { uri: fieldUri },
    position: { line: 16, character: 23 },
  });
  check(
    "field completion follows a constructor binding",
    valueFieldItems.result.items.some((item) => item.label === "desc"),
    JSON.stringify(valueFieldItems.result.items),
  );

  const docsText = `datatype role = Simple of { name : string } | Admin of { name : string, level : int }\n\n(* Build a short label for a role.\n\nReturns the name, with the admin level when present. *)\nfun label (r : role) : string =\n    case r of\n        Simple ({ name = name }) => name\n      | Admin ({ name = name, level = level }) =>\n            name ^ " (" ^ int_to_string (level) ^ ")"\n\nfun main () =\n    let val x = label (Simple ({ name = "Ada" }))\n    in print_string (x)\n    end\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: caseUri, version: 4 },
    contentChanges: [{ text: docsText }],
  });
  const docsCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: caseUri },
    position: { line: 12, character: 18 },
  });
  const labelItem = docsCompletion.result.items.find(
    (item) => item.label === "label",
  );
  check(
    "completion shows function signature and doc comment",
    labelItem?.detail === "fun label (r : role) : string" &&
      labelItem?.documentation?.value?.includes("Build a short label"),
    JSON.stringify(labelItem),
  );
  const docsHover = await client.request("textDocument/hover", {
    textDocument: { uri: caseUri },
    position: { line: 12, character: 18 },
  });
  check(
    "hover shows function signature and doc comment",
    docsHover.result?.contents?.value?.includes("Build a short label") &&
      docsHover.result?.contents?.value?.includes("fun label"),
    JSON.stringify(docsHover.result),
  );

  const pipeUri = "file:///tmp/yack-lsp-pipe-completion.yack";
  const pipeText = `fun inc_one (x : int) : int = x + 1\nfun add_two (x : int, y : int) : int = x + y\n\nfun main () =\n    let val n = 1\n    in n |> \n    end\n`;
  await client.open(pipeUri, pipeText);
  const pipeLine = pipeText.split("\n")[5];
  const pipeItems = await client.request("textDocument/completion", {
    textDocument: { uri: pipeUri },
    position: { line: 5, character: pipeLine.length },
  });
  const unaryItem = pipeItems.result.items.find(
    (item) => item.label === "inc_one",
  );
  check(
    "pipe completion does not add the piped argument",
    unaryItem?.textEdit?.newText === "inc_one",
    JSON.stringify(unaryItem?.textEdit),
  );
  const binaryItem = pipeItems.result.items.find(
    (item) => item.label === "add_two",
  );
  check(
    "pipe completion keeps only the remaining arguments",
    binaryItem?.insertTextFormat === 2 &&
      binaryItem?.textEdit?.newText?.startsWith("add_two (") &&
      binaryItem?.textEdit?.newText?.includes("${1:") &&
      !binaryItem?.textEdit?.newText?.includes("${2:"),
    JSON.stringify(binaryItem?.textEdit),
  );

  const pipeMemberUri = `file://${join(root, "examples/stdlib/src/pipe-member-completion.yack")}`;
  const pipeMemberText = `import yack/list\n\nfun main () =\n    let val xs = [1, 2]\n    in xs |> list.\n    end\n`;
  await client.open(pipeMemberUri, pipeMemberText);
  const pipeMemberLine = pipeMemberText.split("\n")[4];
  const pipeMembers = await client.request("textDocument/completion", {
    textDocument: { uri: pipeMemberUri },
    position: { line: 4, character: pipeMemberLine.length },
  });
  const reverseItem = pipeMembers.result.items.find(
    (item) => item.label === "reverse",
  );
  check(
    "pipe member completion omits the call snippet",
    reverseItem?.textEdit?.newText === "reverse",
    reverseItem
      ? JSON.stringify(reverseItem.textEdit)
      : pipeMembers.result.items.map((item) => item.label).join(","),
  );

  const missingText = `datatype role = Simple of { name : string } | Admin of { name : string, level : int }\n\nfun label (r : role) : string =\n    case r of\n        Simple ({ name = name }) => name\n\nfun main () = ()\n`;
  client.notify("textDocument/didChange", {
    textDocument: { uri: caseUri, version: 3 },
    contentChanges: [{ text: missingText }],
  });
  const missing = await client.wait(
    (message) =>
      message.method === "textDocument/publishDiagnostics" &&
      message.params.uri === caseUri &&
      message.params.diagnostics.some((diagnostic) => diagnostic.code === "E3021"),
    "missing case diagnostics",
  );
  const nonExhaustive = missing.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E3021",
  );
  const missingActions = await client.request("textDocument/codeAction", {
    textDocument: { uri: caseUri },
    range: nonExhaustive.range,
    context: { diagnostics: [nonExhaustive] },
  });
  const branchesAction = missingActions.result.find((action) =>
    action.title.startsWith("Add missing case branches"),
  );
  const branchesEdit =
    branchesAction?.edit?.changes?.[caseUri]?.[0];
  check(
    "quick fix adds the missing constructor branches",
    branchesEdit !== undefined &&
      branchesEdit.newText.includes("Admin") &&
      branchesEdit.newText.includes("level = level"),
    JSON.stringify(missingActions.result),
  );

  const renameUri = "file:///tmp/yack-lsp-rename.yack";
  const renameText = `type Point = { x : int, y : int }
type Named = Point

fun norm (v : Named) =
    let val ax = v.x
    in ax + ax
    end

fun main () = print_int (norm ({ x = 1, y = 2 }))
`;
  await client.open(renameUri, renameText);
  const prepareLocal = await client.request("textDocument/prepareRename", {
    textDocument: { uri: renameUri },
    position: { line: 4, character: 12 },
  });
  check(
    "prepare rename returns a placeholder and range",
    prepareLocal.result?.placeholder === "ax" &&
      prepareLocal.result?.range?.start?.line === 4 &&
      prepareLocal.result?.range?.start?.character === 12,
    JSON.stringify(prepareLocal.result),
  );
  const renameLocal = await client.request("textDocument/rename", {
    textDocument: { uri: renameUri },
    position: { line: 4, character: 12 },
    newName: "bx",
  });
  const localEdits = renameLocal.result?.changes?.[renameUri] ?? [];
  check(
    "rename local updates definition and uses",
    localEdits.length === 3 &&
      localEdits.every((edit) => edit.newText === "bx"),
    JSON.stringify(renameLocal.result),
  );
  const renameFunction = await client.request("textDocument/rename", {
    textDocument: { uri: renameUri },
    position: { line: 8, character: 26 },
    newName: "measure",
  });
  const functionEdits = renameFunction.result?.changes?.[renameUri] ?? [];
  check(
    "rename function updates declaration and call",
    functionEdits.length === 2 &&
      functionEdits.some((edit) => edit.range.start.line === 3) &&
      functionEdits.some((edit) => edit.range.start.line === 8),
    JSON.stringify(renameFunction.result),
  );
  const renameType = await client.request("textDocument/rename", {
    textDocument: { uri: renameUri },
    position: { line: 0, character: 6 },
    newName: "Coord",
  });
  const typeEdits = renameType.result?.changes?.[renameUri] ?? [];
  check(
    "rename record type updates declaration and annotations",
    typeEdits.length === 2 &&
      typeEdits.every((edit) => edit.newText === "Coord"),
    JSON.stringify(renameType.result),
  );
  const aliasRecordHover = await client.request("textDocument/hover", {
    textDocument: { uri: renameUri },
    position: { line: 1, character: 6 },
  });
  check(
    "hover resolves an alias to its record structure",
    markdownContents(aliasRecordHover.result).includes(
      "type Named = { x : int, y : int }",
    ),
    JSON.stringify(aliasRecordHover.result),
  );



  const unknown = await client.request("textDocument/notARealMethod", {
    textDocument: { uri: caseUri },
  });
  check(
    "unknown method gets a valid JSON-RPC error",
    typeof unknown.error?.code === "number" && unknown.error.code < 0,
    JSON.stringify(unknown.error),
  );



  const localClosureUri = "file:///tmp/yack-lsp-local-closure.yack";
  const localClosureText = `fun make_adder (n : int) : int -> int =
    let fun add (x) = x + n
    in add
    end

fun main () =
    let val f = make_adder (2)
    in print_int (f (3))
    end
`;
  const localClosureDiagnostics = await client.open(
    localClosureUri,
    localClosureText,
  );
  const localClosureDiagnostic = localClosureDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E0518",
  );
  check(
    "capturing local function closure diagnostic",
    Boolean(localClosureDiagnostic) &&
      localClosureDiagnostic.message.includes("no capturing closures") &&
      localClosureDiagnostic.message.includes("call it directly"),
    JSON.stringify(localClosureDiagnostics.params.diagnostics),
  );

  const localShadowUri = "file:///tmp/yack-lsp-local-shadow.yack";
  const localShadowText = `fun main () =
    let val n = 1
        fun f () = n
        val n = 2
    in print_int (f ())
    end
`;
  const localShadowDiagnostics = await client.open(
    localShadowUri,
    localShadowText,
  );
  const localShadowDiagnostic = localShadowDiagnostics.params.diagnostics.find(
    (diagnostic) => diagnostic.code === "E0519",
  );
  check(
    "shadowing a captured local binding diagnostic",
    Boolean(localShadowDiagnostic) &&
      localShadowDiagnostic.message.includes("shadows") &&
      localShadowDiagnostic.message.includes("rename"),
    JSON.stringify(localShadowDiagnostics.params.diagnostics),
  );




  const arenaUri = `file://${join(root, "test/lsp-arena.yack")}`;
  const arenaText = `fun label (n : int) : string =
    "n=" ^ int_to_string (n)

fun report (n : int) : string =
    arena
        val head = "total: " ^ int_to_string (n)
        val tags = array_new ()
        val _ = array_push (tags, label (n) ^ "!")
    in
        head ^ label (n)
    end

fun main () =
    print_string (report (3))
`;
  await client.open(arenaUri, arenaText);
  const arenaHints = await client.request("textDocument/inlayHint", {
    textDocument: { uri: arenaUri },
    range: { start: { line: 0, character: 0 }, end: { line: 20, character: 0 } },
  });
  const arenaHint = (arenaHints.result ?? []).find(
    (hint) => hint.position.line === 4 && hint.position.character === 9,
  );
  check(
    "arena inlay hint counts freed, stored and outliving allocations",
    Boolean(arenaHint) &&
      arenaHint.label ===
        "3 strings freed at end · 1 stored to root · 1 outlive the block · 1 call allocate here",
    JSON.stringify(arenaHints.result),
  );
  const arenaHover = await client.request("textDocument/hover", {
    textDocument: { uri: arenaUri },
    position: { line: 4, character: 6 },
  });
  const arenaMarkdown = markdownContents(arenaHover.result);
  check(
    "arena hover lists sites with links and the kept value",
    arenaMarkdown.includes("**Freed at `end` (3)**") &&
      arenaMarkdown.includes(`[6:20](${arenaUri}#L6,20) \`^\` concatenation`) &&
      arenaMarkdown.includes("value of `array_push`") &&
      arenaMarkdown.includes("`array_new` — heap, freed when its owner is dropped") &&
      arenaMarkdown.includes("Strings allocated by `label`") &&
      arenaMarkdown.includes("copied out of the arena"),
    arenaMarkdown,
  );



  const brokenInlay = await client.request("textDocument/inlayHint", {
    textDocument: { uri: simpleUri },
    range: { start: { line: 0, character: 0 }, end: { line: 40, character: 0 } },
  });
  check(
    "inlay hints answer with a list on a buffer with errors",
    Array.isArray(brokenInlay.result),
    JSON.stringify(brokenInlay),
  );

  const letUri = "file:///tmp/yack-lsp-let.yack";
  const letText = `fun main () =\n    let\n`;
  await client.open(letUri, letText);
  const letCompletion = await client.request("textDocument/completion", {
    textDocument: { uri: letUri },
    position: { line: 1, character: 7 },
  });
  const letItem = letCompletion.result.items.find(
    (item) => item.label === "let",
  );
  check(
    "let completion inserts a let … in … end snippet",
    letItem?.insertTextFormat === 2 &&
      letItem?.textEdit?.newText?.includes("in") &&
      letItem?.textEdit?.newText?.trimEnd().endsWith("end"),
    JSON.stringify(letItem),
  );

  const rankFile = join(root, "examples/stdlib/src/rank-completion.yack");
  const rankUri = `file://${rankFile}`;
  const rankText = `import yack/option

fun helper (a : int) : int = a

fun other () : int =
    let val hidden_name = 1
    in hidden_name
    end

fun main () =
    let val local_value = 1
        fun local_fun (x : int) : int = x
    in
        print_int (local_fun (local_value))
    end
`;
  await client.open(rankUri, rankText);
  const ranked = await client.request("textDocument/completion", {
    textDocument: { uri: rankUri },
    position: { line: 12, character: 7 },
  });
  const rankLabels = ranked.result.items.map((item) => item.label);
  const rankItem = (label) =>
    ranked.result.items.find((item) => item.label === label);
  const rankIndex = (label) => rankLabels.indexOf(label);
  check(
    "completion puts locals, file functions, modules and keywords in that order",
    rankIndex("local_value") >= 0 &&
      rankIndex("local_value") < rankIndex("local_fun") &&
      rankIndex("local_fun") < rankIndex("helper") &&
      rankIndex("helper") < rankIndex("option") &&
      rankIndex("option") < rankIndex("let"),
    rankLabels.slice(0, 16).join(","),
  );
  check(
    "completion hides locals from another function",
    !rankLabels.includes("hidden_name"),
    rankLabels.slice(0, 16).join(","),
  );
  check(
    "completion sortText follows the same relevance order",
    rankItem("local_value")?.sortText < rankItem("local_fun")?.sortText &&
      rankItem("local_fun")?.sortText < rankItem("helper")?.sortText &&
      rankItem("helper")?.sortText < rankItem("option")?.sortText &&
      rankItem("option")?.sortText < rankItem("let")?.sortText,
    JSON.stringify(
      ["local_value", "local_fun", "helper", "option", "let"].map(
        (label) => [label, rankItem(label)?.sortText],
      ),
    ),
  );




  client.child.stdin.write("X-No-Length: 1\r\n\r\n");
  client.child.stdin.write("Content-Length: abc\r\n\r\n");
  client.child.stdin.write("Content-Length: 99999999999999999999\r\n\r\n");
  client.child.stdin.write("Content-Length: -5\r\n\r\n");
  const notJson = "{this is not json";
  client.child.stdin.write(
    `Content-Length: ${Buffer.byteLength(notJson)}\r\n\r\n${notJson}`,
  );
  const parseError = await client.wait(
    (message) => message.error?.code === -32700,
    "parse error response",
  );
  check(
    "non-JSON body answers -32700 with id null",
    parseError.id === null && parseError.error.message === "Parse error",
    JSON.stringify(parseError),
  );
  const afterFraming = await client.request("initialize", {
    processId: process.pid,
    rootUri: `file://${root}`,
    capabilities: {},
  });
  check(
    "server still answers after malformed framing",
    afterFraming.result?.capabilities?.textDocumentSync?.openClose === true,
    JSON.stringify(afterFraming.error),
  );
  check(
    "malformed framing is logged on stderr",
    client.stderr.includes("missing Content-Length header") &&
      client.stderr.includes("invalid Content-Length"),
    client.stderr,
  );



  const truncated = spawn(yack, ["lsp"], {
    cwd: root,
    stdio: ["pipe", "pipe", "pipe"],
  });
  let truncatedErr = "";
  let truncatedOut = "";
  truncated.stdout.on("data", (chunk) => {
    truncatedOut += chunk.toString("utf8");
  });
  truncated.stderr.on("data", (chunk) => {
    truncatedErr += chunk.toString("utf8");
  });
  const truncatedExit = new Promise((resolve) => {
    truncated.on("exit", (code, signal) => resolve({ code, signal }));
  });
  truncated.stdin.write('Content-Length: 100\r\n\r\n{"jsonrpc"');
  truncated.stdin.end();
  const truncatedResult = await truncatedExit;
  check(
    "truncated body exits cleanly",
    truncatedResult.code === 0 &&
      truncatedOut === "" &&
      truncatedErr.includes("unexpected end of message body") &&
      !/uncaught exception|Exception|Fail/i.test(truncatedErr),
    JSON.stringify({ ...truncatedResult, stderr: truncatedErr }),
  );




  const multiParseUri = `file://${join(root, "test/reject/multi_parse_type.yack")}`;
  const multiParse = await client.open(
    multiParseUri,
    `fun broken (x : int) : int = x +

fun main () =
    let val c : bool = 1
    in if c then print_int (0) else print_int (1)
    end
`,
  );
  const multiParseCodes = new Set(
    multiParse.params.diagnostics.map((diagnostic) => diagnostic.code),
  );
  check(
    "parse error and type error in different functions are both published",
    multiParseCodes.has("E2002") && multiParseCodes.has("E3004"),
    JSON.stringify([...multiParseCodes]),
  );

  const multiOwnershipUri = `file://${join(root, "test/reject/multi_type_ownership.yack")}`;
  const multiOwnership = await client.open(
    multiOwnershipUri,
    `fun b () : bool = 3

fun c () : unit =
    let val s = "text"
        val t = s
    in print_string (s)
    end

fun main () = print_bool (b ())
`,
  );
  const multiOwnershipCodes = new Set(
    multiOwnership.params.diagnostics.map((diagnostic) => diagnostic.code),
  );
  check(
    "type error and ownership error in different functions are both published",
    multiOwnershipCodes.has("E3004") && multiOwnershipCodes.has("E0402"),
    JSON.stringify([...multiOwnershipCodes]),
  );

  await client.request("shutdown", {});
  client.notify("exit", {});
} catch (error) {
  failures += 1;
  console.error(`FAIL lsp/crash: ${error.message}`);
} finally {
  client.stop();
}

if (failures > 0) {
  console.error(`\n${failures} LSP test(s) failed`);
  process.exit(1);
}
console.log("\nLSP smoke test passed");
