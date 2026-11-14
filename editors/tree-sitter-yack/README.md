# tree-sitter-yack

Grammaire Tree-sitter pour Yack.

- commentaires imbriqués `(* ... *)` via `src/scanner.c` ;
- imports Gleam-style (`import a/b.{type T, f} as M`) ;
- types postfixes (`int list`, `'a option`), tuples avec `*` ou `,` ;
- types de fonction (`int -> int`, `(int, msg) -> state`), `consume T`,
  types paramétrés et alias `type 'a box = ...` ;
- portées `arena ... in ... end`, fonctions locales `let fun f (...) = ...`
  et fonctions anonymes `x => ...` / `(x, y) => ...` ;
- records, datatypes, patterns imbriqués ;
- listes `[1, 2, 3]`, `[]` et `head :: tail` en pattern ;
- bindings `guard condition else fallback` et
  `guard pattern <- value else fallback` ;
- appels qualifiés implicites des builtins (`io.read_file "path"`).

## Générer le parser

`src/parser.c` est commité pour que Helix puisse compiler la grammaire sans
installer le CLI Tree-sitter. Pour la régénérer après une modification de
`grammar.js` :

```sh
cd editors/tree-sitter-yack
npm install
npx tree-sitter generate
```

Le CLI épinglé est `tree-sitter-cli@0.25.10`. La génération utilise l'ABI 14
(compatible Helix 25.07). Un `tree-sitter.json` pour l'ABI 15 pourra être ajouté
quand Helix et le CLI seront alignés.

## Tester

```sh
npx tree-sitter parse ../../examples/datatype_list.yack
npx tree-sitter query queries/highlights.scm ../../examples/hello.yack
```

Tous les `.yack` de `examples/` et `lib/` doivent parser sans `ERROR` ni
`MISSING`.

## Queries

| Fichier | Usage |
|---|---|
| `queries/highlights.scm` | coloration syntaxique |
| `queries/locals.scm` | définitions/références locales |
| `queries/indents.scm` | indentation automatique |
| `queries/textobjects.scm` | textobjects (`af`, `if`, …) |
| `queries/folds.scm` | pliage |
| `queries/tags.scm` | symboles / picker |

## Helix

Voir `editors/helix/languages.toml`. En résumé :

```toml
[[language]]
name = "yack"
scope = "source.yack"
file-types = ["yack"]
comment-token = "(*"
indent = { tab-width = 4, unit = "    " }
auto-format = true
formatter = { command = "yack", args = ["fmt", "-"] }

[[grammar]]
name = "yack"
source = { path = "/chemin/vers/yack/editors/tree-sitter-yack" }
```

Puis :

```sh
mkdir -p ~/.config/helix/runtime/queries
ln -sfn /chemin/vers/yack/editors/tree-sitter-yack/queries \
        ~/.config/helix/runtime/queries/yack
hx --grammar build
hx --health yack
```

Helix cherche les queries dans `runtime/queries/yack/`, pas dans le dossier de
la grammaire : le symlink est nécessaire pour que les modifications de queries
soient prises en compte sans copie.
