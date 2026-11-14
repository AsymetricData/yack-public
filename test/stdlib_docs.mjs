#!/usr/bin/env node






import { readdirSync, readFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const sourceDir = join(root, "lib/yack/src");

let failures = 0;
let declarations = 0;

function fail(file, line, message) {
  failures += 1;
  console.log(`FAIL docs/${file}:${line} ${message}`);
}

function commentBlock(lines, declarationLine) {
  if (declarationLine === 0) return null;
  const previous = lines[declarationLine - 1].trim();
  if (!previous.endsWith("*)")) return null;
  const block = [];
  let index = declarationLine - 1;
  while (index >= 0) {
    block.push(lines[index]);
    if (lines[index].trimStart().startsWith("(*")) break;
    index -= 1;
  }
  return block.reverse().join("\n");
}

function splitParameters(text) {
  const parts = [];
  let depth = 0;
  let start = 0;
  for (let index = 0; index < text.length; index += 1) {
    const character = text[index];
    if (character === "(" || character === "{" || character === "[") depth += 1;
    else if (character === ")" || character === "}" || character === "]") depth -= 1;
    else if (character === "," && depth === 0) {
      parts.push(text.slice(start, index));
      start = index + 1;
    }
  }
  parts.push(text.slice(start));
  return parts.map((part) => part.trim()).filter((part) => part !== "");
}

function parameterName(part) {
  let depth = 0;
  for (let index = 0; index < part.length; index += 1) {
    const character = part[index];
    if (character === "(" || character === "{" || character === "[") depth += 1;
    else if (character === ")" || character === "}" || character === "]") depth -= 1;
    else if (character === ":" && depth === 0) {
      return part.slice(0, index).trim();
    }
  }
  return part.trim();
}

function functionParameters(lines, start) {
  let text = "";
  let offset = 0;
  let depth = 0;
  let open = -1;
  for (let index = start; index < lines.length; index += 1) {
    text += lines[index] + "\n";
    for (let column = 0; column < lines[index].length; column += 1) {
      const position = offset + column;
      const character = lines[index][column];
      if (character === "(") {
        if (open < 0) open = position;
        depth += 1;
      } else if (character === ")") {
        depth -= 1;
        if (open >= 0 && depth === 0) {
          return splitParameters(text.slice(open + 1, position)).map(parameterName);
        }
      }
    }
    offset += lines[index].length + 1;
  }
  return [];
}

const files = readdirSync(sourceDir).filter((name) => name.endsWith(".yack")).sort();
for (const name of files) {
  const lines = readFileSync(join(sourceDir, name), "utf8").split("\n");
  for (let index = 0; index < lines.length; index += 1) {
    const match = /^(fun|type|datatype)\s/.exec(lines[index]);
    if (!match) continue;
    declarations += 1;
    const doc = commentBlock(lines, index);
    if (doc === null) {
      fail(name, index + 1, `${lines[index].trim()} has no adjacent doc comment`);
      continue;
    }
    if (match[1] !== "fun") continue;
    for (const parameter of functionParameters(lines, index)) {
      if (parameter === "") continue;
      if (!doc.includes("`" + parameter + "`")) {
        fail(name, index + 1, `parameter \`${parameter}\` is missing from the doc`);
      }
    }
  }
}

if (failures > 0) {
  console.error(`\n${failures} stdlib documentation problem(s)`);
  process.exit(1);
}
console.log(`stdlib docs: ${declarations} declarations documented for the LSP`);
