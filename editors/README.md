# Éditeurs

- [`tree-sitter-yack/`](tree-sitter-yack/) : grammaire Tree-sitter, scanner de
  commentaires imbriqués et queries (highlights, locals, indents, textobjects,
  folds, tags).
- [`helix/`](helix/) : configuration Helix de référence : parser local,
  `yack lsp`, `yack fmt -` comme formatter, formatage à la sauvegarde.

Le language server est intégré au compilateur :

```sh
./yack lsp
```

Il fournit les diagnostics de `yack check`, les inlay hints des types inférés,
la complétion (prélude, builtins, modules et imports) et des quick fixes.
Voir [`docs/lsp.md`](../docs/lsp.md).

Pour un lint agrégé dans un terminal, sans client LSP, le watcher reste
disponible : [`tools/yack-watch.ts`](../tools/yack-watch.ts).
