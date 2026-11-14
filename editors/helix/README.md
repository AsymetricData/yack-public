# Helix + Yack

Configuration de référence pour Helix 25.07+ :

- grammaire Tree-sitter locale (`editors/tree-sitter-yack`) ;
- language server `yack lsp` (diagnostics, hints, complétion, quick fixes) ;
- formatter `yack fmt --best-effort -` sur stdin (passthrough si le buffer est
  syntaxiquement incomplet, pour ne pas faire échouer l'auto-format) ;
- formatage automatique à la sauvegarde ;
- largeur cible 88 colonnes.

## 1. `languages.toml`

Copier les blocs de [`languages.toml`](languages.toml) dans
`~/.config/helix/languages.toml`, en remplaçant `/chemin/vers/yack` par le
chemin absolu du dépôt.

Pour utiliser le binaire du dépôt plutôt qu'un `yack` installé :

```toml
[language-server.yack-lsp]
command = "/chemin/vers/yack/yack"
args = ["lsp"]

# dans [[language]]
language-servers = ["yack-lsp"]
formatter = { command = "/chemin/vers/yack/yack", args = ["fmt", "--best-effort", "-"] }
```

Yack n'a pas de commentaire de ligne : `block-comment-tokens` est utilisé à la
place de `comment-token`.

## 2. Queries

Helix cherche les queries dans `runtime/queries/yack/`, pas dans le dossier de
la grammaire. Faire un lien symbolique pour que les modifications de queries
soient prises en compte sans copie :

```sh
mkdir -p ~/.config/helix/runtime/queries
ln -sfn /chemin/vers/yack/editors/tree-sitter-yack/queries \
        ~/.config/helix/runtime/queries/yack
```

## 3. Construire la grammaire

```sh
hx --grammar build
hx --health yack
```

Attendu :

```text
Configured language server:
  ✓  yack-lsp
Configured formatter:
  ✓  /chemin/vers/yack/yack
Tree-sitter parser: ✓
Highlight queries: ✓
Textobject queries: ✓
Indent queries: ✓
```

## 4. Vérification pendant l'édition

Helix lance automatiquement `yack lsp` grâce à `language-servers`. Les
diagnostics apparaissent dans la marge, `space-k` affiche le hover, la
complétion s'ouvre sur `.` et `/`, et `space-a` propose les quick fixes.

Le watcher reste utile pour un panneau de lint agrégé ou la CI :

```sh
make watch WATCH_ENTRY=examples/stdlib/src/main.yack
# ou
./tools/yack-watch.ts examples/stdlib/src/main.yack --fmt
```

Le watcher relance `yack check` après chaque modification et affiche les
diagnostics. `--fmt` vérifie aussi `yack fmt --check` sur les fichiers modifiés.

Exemple de keybinding Helix (optionnel, dans `config.toml`) :

```toml
[keys.normal.space]
"y" = ":sh zellij run -f --width 40% --height 100% -- /chemin/vers/yack/tools/yack-watch.ts /chemin/vers/yack/examples/stdlib/src/main.yack"
```
