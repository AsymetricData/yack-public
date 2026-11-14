



import { readFileSync, existsSync } from "node:fs";

const pages = ["site/alpage.html", "site/cli.html", "site/index.html"].filter(existsSync);
const unescape = (text) =>
  text.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&amp;/g, "&");

let checked = 0;
let failed = 0;
for (const page of pages) {
  const html = readFileSync(page, "utf8");
  const blocks = html.matchAll(/<code[^>]*data-from="([^"]+)"[^>]*>([\s\S]*?)<\/code>/g);
  for (const [, path, body] of blocks) {
    const source = readFileSync(path, "utf8");
    for (const paragraph of unescape(body).split("\n\n")) {
      checked += 1;
      if (!source.includes(paragraph.trim())) {
        failed += 1;
        console.log(`FAIL ${page}: snippet not in ${path}:\n${paragraph}\n`);
      }
    }
  }
}
console.log(`site snippets: ${checked} checked, ${failed} stale`);
process.exit(failed ? 1 : 0);
