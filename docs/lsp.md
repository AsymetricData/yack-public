# Yack : language server (`yack lsp`)

> **Statut : intégré au compilateur. Les fonctionnalités principales sont là,
> les limites sont listées en bas.**

`yack lsp` est un serveur JSON-RPC 2.0 qui parle le Language Server Protocol
sur `stdin` / `stdout`. Il est compilé avec le reste de Yack (MLton) et ne
dépend d'aucun runtime externe.

```sh
./yack lsp
```

Le serveur charge les fichiers avec le même pipeline que `yack check` :

```text
lex → parse → modules/imports → resolve → check → ownership
```

Les buffers non sauvegardés du client remplacent le contenu du disque, y
compris quand ils sont importés par le fichier courant.

## Capacités annoncées

| Capacité | Valeur |
|---|---|
| `textDocumentSync` | `openClose: true`, `change: 2` (incremental), `save` |
| `completionProvider` | déclencheurs `.` et `/`, `resolveProvider: true` |
| `signatureHelpProvider` | déclencheurs `(` et `,` |
| `inlayHintProvider` | `true` |
| `semanticTokensProvider` | légende complète, `full` et `range` |
| `renameProvider` | `prepareProvider: true` |
| `hoverProvider` | `true` |
| `documentSymbolProvider` | `true` |
| `foldingRangeProvider` | `true` |
| `documentFormattingProvider` | `true` |
| `documentRangeFormattingProvider` | `true` |
| `definitionProvider` | `true` |
| `declarationProvider` | `true` (même cible que `definition`) |
| `typeDefinitionProvider` | `true` |
| `documentLinkProvider` | `true` |
| `referencesProvider` | `true` |
| `documentHighlightProvider` | `true` |
| `workspaceSymbolProvider` | `true` |
| `callHierarchyProvider` | `true` |
| `codeActionProvider` | `quickfix`, `refactor.rewrite` |

## Diagnostics

À chaque `didOpen`, `didChange` et `didSave`, le serveur analyse le document et
publie `textDocument/publishDiagnostics`. Les diagnostics proviennent des mêmes
structures `Diag.t` que la CLI, avec :

- code stable (`E2002`, `E3004`, `E0402`, `E0502`, …), documenté dans
  [`docs/errors.md`](errors.md) ;
- sévérité, portée exacte et source `yack` ;
- notes et hints ajoutés au message (`•` et `→`).

Les diagnostics des modules importés sont publiés sur leur URI. Les fichiers
synthétiques (`<prelude>`, `<builtins>`) ne sont jamais exposés.

Si une phase échoue, seuls les diagnostics de cette phase sont publiés ; les
phases suivantes n'ont pas de sens sur un AST invalide.

## Inlay hints

`textDocument/inlayHint` renvoie :

- le type inféré des liaisons **non annotées** :
  - paramètres de fonction ;
  - `val` simples et destructurants ;
  - variables de pattern (`case`, `val` destructurant) ;
  - type de retour des fonctions sans annotation, affiché après la parenthèse
    fermante des paramètres ;
- le nom de chaque argument à l'appel d'une fonction connue, façon gopls :
  `combine (left: 1, right: 2)`. Le hint est omis quand l'argument est un
  constructeur, quand l'arité ne correspond pas, et quand aucun nom de
  paramètre n'est connu (les builtins en ont via
  `CheckBuiltins.parameterNames`). La forme `x |> f y` n'est pas concernée :
  le désucrage déplace `x` dans la liste d'arguments, ce qui placerait le hint
  au mauvais endroit.

Un hint est placé juste après le nom de la liaison. Les annotations explicites
sont omises. Les types utilisent la syntaxe Yack (`'a list`, `borrow string`,
`int64`, …) et les records nommés affichent leurs champs :
`LanguageProp = { desc : string }`. Un type encore partiellement inféré
(variable de type libre, fréquent pendant la frappe) n'est pas affiché plutôt
que de montrer `'t9`.

## Hover

Le hover affiche la signature et le commentaire `(* ... *)` d'une déclaration.
Pour un type record, il affiche sa structure ; un alias est résolu jusqu'au
record sous-jacent (`type Named = LanguageProp` montre
`type Named = { desc : string }`).

## Rename

`textDocument/prepareRename` et `textDocument/rename` renomment :

- les fonctions, paramètres et liaisons locales **à travers les fichiers
  chargés** : la résolution relie chaque occurrence au même binding, y compris
  un `fun` déclaré dans un module importé. Un appel qualifié (`json.parse`)
  n'est réécrit que sur le membre (`parse`), jamais sur le qualificatif ;
- les imports sélectifs : `import yack/option.{unwrap_or as unwrap}` voit son
  nom d'origine renommé (`unwrap_or`) sans toucher l'alias ni les usages ;
- les types déclarés (`type`, `datatype`, alias) et leurs annotations, dans le
  fichier courant uniquement : les annotations n'ont pas encore de spans dans
  l'AST.

## Signature help

`textDocument/signatureHelp` s'affiche dès qu'une parenthèse d'appel est
ouverte, et se réactualise sur `,` :

```text
fun get (app : t, pattern : string, handler : (Ctx.t, consume Request.t) -> Reply.t)
                 ^^^^^
```

Le paramètre actif suit la virgule de premier niveau (les virgules des appels
imbriqués, tuples et records sont ignorées). La signature et son commentaire
`(* ... *)` viennent de la déclaration courante, puis du module qualifié
(`json.parse`), puis des modules importés. Les labels de paramètres sont ceux
de la signature, donc surlignables par le client. Les builtins, sans
déclaration AST, utilisent la dernière analyse typée du buffer
(`fun print_int (value : int) : unit`), ce qui les rend disponibles même quand
l'appel est encore incomplet.

## Définition et références

`textDocument/definition` suit :

1. un chemin d'`import` (`import yack/json` ouvre le fichier du module) ;
2. le qualificatif d'un nom (`Alpage` dans `Alpage.get`, `Ctx` dans `Ctx.t`)
   ouvre le fichier du module ;
3. une variable ou un paramètre local via la résolution des noms (le shadowing
   et les homonymes sont respectés) ;
4. un membre d'un module importé, qualifié (`json.parse`) ou importé
   sélectivement ;
5. un champ de record (`cfg.port` → déclaration du champ dans le type) ;
6. un constructeur (`Ok`, `Some`, …) → son identifiant dans le `datatype` ;
7. une déclaration du fichier courant (`fun`, `type`, `datatype`, alias).

`textDocument/declaration` renvoie la même cible, pour les clients qui
distinguent « aller à la déclaration » de « aller à la définition ».

La cible d'un module renvoie un `Location` dans le fichier du module, avec la
portée du nom déclaré. Les fichiers synthétiques (`<prelude>`, `<builtins>`)
n'ont pas de définition.

`textDocument/typeDefinition` part du type inféré sous le curseur (variable,
paramètre, expression) pour atteindre la déclaration du type record/datatype
correspondant. Sur un nom de type écrit dans une annotation, il retombe sur la
déclaration du fichier ou d'un module importé.

`textDocument/documentLink` transforme chaque chemin d'`import` en lien vers
le fichier du module (`import yack/json` → `lib/yack/src/json.yack`).

`textDocument/references` renvoie toutes les occurrences d'une liaison ou d'un
type **dans les fichiers chargés pour l'analyse courante** (le fichier et son
graphe d'imports), déclaration incluse ou non selon
`context.includeDeclaration`.

`textDocument/documentHighlight` renvoie les occurrences du fichier courant,
marquées comme lectures (`kind: 1`), pour le surlignage à la position du
curseur.

## Symboles et pliage

`textDocument/documentSymbol` renvoie un outline hiérarchique : fonctions
(`Function`), types record (`Struct`) avec leurs champs (`Field`), datatypes
(`Enum`) avec leurs constructeurs (`EnumMember`), alias (`Class`). Chaque
symbole porte sa signature en `detail`, `range` couvre la déclaration et
`selectionRange` le nom.

`textDocument/foldingRange` plie les déclarations multi-lignes et les blocs de
commentaires `(* ... *)`.

`workspace/symbol` cherche dans l'index du paquet racine (dépendances
comprises) **et dans les buffers ouverts**, y compris non sauvegardés : un
fichier ouvert remplace sa version disque, avec son `containerName` dérivé du
chemin. La recherche est insensible à la casse et plafonnée à 300 résultats.

## Semantic tokens

`textDocument/semanticTokens/full` colore, à partir du lexer et de la
résolution :

- mots-clés, chaînes, nombres, commentaires et opérateurs ;
- fonctions, paramètres, variables locales, champs, constructeurs
  (`enumMember`), types (`type`, `struct`, `enum`) et variables de type ;
- qualificatifs de module (`json` dans `json.parse`) en `namespace`.

Les offsets proviennent de la résolution pour les liaisons et les lectures :
un paramètre reste `parameter` même si un fonction du même nom existe ailleurs.
Les commentaires multi-lignes sont découpés en un token par ligne. `full`,
`full/delta` et `range` sont annoncés : le delta compare les tableaux plats
par groupes de 5 entiers (frontière de token obligatoire, car chaque delta est
relatif au token précédent), et `range` ne renvoie que les lignes demandées.

## Call hierarchy

`textDocument/prepareCallHierarchy` sur un appel ou une déclaration renvoie un
item de fonction (`kind: 12`) avec l'identité interne du binding dans
`data.name`.

- `callHierarchy/incomingCalls` liste les fonctions qui appellent la cible,
  avec les plages d'appel ;
- `callHierarchy/outgoingCalls` liste les fonctions appelées par la cible.

Le graphe vient du programme typé, qui contient tous les modules chargés :
un appel à `json.parse` apparaît donc avec un `to` dans `json.yack`. Les
fonctions locales synthétiques (`__yack_local_*`) sont ignorées, et un appel
à un builtin n'a pas d'item de destination.

## Formatage

`textDocument/formatting` passe le buffer par le même formateur que
`yack fmt` et renvoie un unique `TextEdit`. Un buffer lexicalement ou
syntaxiquement invalide n'est pas modifié : la réponse est une liste d'edits
vide plutôt qu'un remplacement partiel.

`textDocument/rangeFormatting` ne renvoie que la région minimale modifiée
(préfixe/suffixe communs entre buffer et sortie du formateur) et rien si elle
n'intersecte pas la plage demandée.

## Complétion

`textDocument/completion` renvoie un `CompletionList` avec un `data` par item
(URI, nom, genre). Chaque déclaration connue porte déjà sa signature et son
commentaire `(* ... *)` placé juste au-dessus. Les variables et paramètres
locaux affichent leur type inféré dans `detail` (`int`, `borrow string`, …) ;
une variable dont le type n'est pas encore résolu garde la mention
`in this file`.

`completionItem/resolve` reste disponible : un client qui a retiré la
documentation (ou un builtin sans signature immédiate) la fait recalculer
depuis `data`, sans refaire toute la liste.

Toutes les positions sont traitées de façon tolérante : la complétion continue
de fonctionner quand la ligne courante est syntaxiquement incomplète, par
exemple juste après `json.` ou `import yack/`.

Sources de complétion :

1. mots-clés du langage : accepter `let` insère un bloc `let val … = … in …
   end` (snippet, `insertTextFormat: 2`) ;
2. builtins (`print_int`, `array_*`, `map_*`, conversions) ;
3. prélude (`option`, `result`, `list`, `list_length`, …) ;
4. déclarations et variables locales du fichier ;
5. noms importés sélectivement ;
6. membres qualifiés des modules importés (`json.parse`, `string.trim`, …) ;
7. modules des dépendances pas encore importés : taper `Ct` propose `Ctx` avec
   un `additionalTextEdits` qui ajoute `import alpage/Ctx` ;
8. chemins de modules pour `import yack/…` ;
9. champs d'un payload record (`Simple ({ name = … })`), filtrés d'après le
   constructeur quand il est reconnaissable ;
10. constructeurs dans un `case`, filtrés d'après le type annoté du scrutinee
    et enrichis en patterns complets (`Admin ({ name = name, level = level })`) ;
11. appels de fonction avec snippet : les placeholders portent le nom des
    paramètres déclarés, `option.unwrap_or (${1:value}, ${2:default})`, avec le
    curseur dans le premier argument. Les builtins ont aussi des noms, déclarés
    une fois dans `CheckBuiltins.parameterNames` :
    `array_get (${1:array}, ${2:index})`, et affichent leur signature complète
    en `detail` dès qu'une analyse typée existe ;
12. forme inline dans une position de callback : `x => …` ou
    `(x, y) => …`, insérée comme `x => ` / `(x, y) => `.

Les items sont classés par pertinence via `sortText` : variables locales de la
portée courante (la plus proche d'abord), fonctions locales, déclarations du
fichier, noms importés, modules, builtins/prélude, puis mots-clés. À catégorie
égale, un nom exact passe avant un préfixe, lui-même avant une sous-chaîne. Une
liaison d'une autre fonction, une branche `case` voisine ou une variable pas
encore en portée n'est pas proposée. Les listes contextuelles (membres après
`.`, champs d'un record, constructeurs d'un `case`, chemins d'`import`) restent
exclusives : seuls les items pertinents pour ce contexte sont renvoyés.

Quand un qualificatif n'est pas encore importé, ses membres sont proposés avec
un `additionalTextEdits` qui ajoute l'import correspondant. Taper `os.` dans un
fichier sans `import yack/os` propose donc `os.home`, et accepter l'item ajoute
`import yack/os` après le dernier import existant (ou en tête de fichier). Si
le module est déjà importé, aucun edit supplémentaire n'est renvoyé.

L'index des modules est construit à partir du `yack.toml` du paquet, du dossier
`src/` et des dépendances déclarées, puis mis en cache. Le cache est indexé par
racine de paquet et par date de modification du `yack.toml` (ajouter une
dépendance reconstruit l'index à la requête suivante, sans redémarrer
l'éditeur), et il est invalidé sur `didSave`. Ouvrir un fichier hors paquet
n'empêche pas la complétion dans un autre paquet. Quand le code source contient
`lib/yack` au-dessus du fichier, le paquet `yack` est aussi découvert
implicitement, comme le fait le compilateur pour l'alias réservé `yack`. Les
fichiers `.yack` sont lus et parsés sans exécuter le type checker, ce qui garde
la complétion rapide même sur une grande stdlib.

## Hover

`textDocument/hover` renvoie, dans cet ordre :

1. la signature de la déclaration sous le curseur ;
2. son commentaire de documentation `(* ... *)` placé juste au-dessus ;
3. sinon, le type inféré le plus précis.

Exemple :

````text
```yack
fun label (r : role) : string
```

Build a short label for a role.
````

Le commentaire doit être **directement adjacent** à la déclaration (pas de
ligne vide) :

```yack
(* Construit un libellé court pour un rôle. *)
fun label (r : role) : string =
    ...
```

Les signatures et docs sont aussi disponibles pour les modules importés :
survoler ou compléter `string.trim` affiche
`fun trim (text : borrow string) : string` et le commentaire du module s'il y
en a un. Un nom qualifié est résolu dans le module correspondant
(`json.parse` lit `yack/json`), jamais dans le premier module qui exporte le
même nom court.

Les liaisons sans déclaration retombent sur le type inféré : survoler `x` dans
`let val x = ...` donne toujours le type de `x`. Un builtin affiche sa
signature typée (`fun print_int (value : int) : unit`), y compris pendant que
l'appel est incomplet grâce à la dernière analyse typée.

## Quick fixes

`textDocument/codeAction` produit des actions `quickfix` avec un `TextEdit`
unique. Elles fonctionnent même lorsque le fichier est en erreur : le serveur
retombe sur un parse local pour retrouver l'AST utile.

| Code | Action | Effet |
|---|---|---|
| `E3021` | Add missing case branches | ajoute les constructeurs manquants avec leurs champs de record |
| `E3021` | Add wildcard branch | ajoute `\| _ => …` après la dernière branche ; le littéral est déduit des branches (`0`, `false`, `""`, `()`) |
| `E3022` | Remove unreachable branch | supprime la branche signalée |
| `E3001`, `E3002` | Change to `…` | propose jusqu'à 3 noms proches (distance ≤ 2) |
| `E3001`, `E3002` | Import `name` from `module` | ajoute `import module.{name}` quand un module exporte le nom |
| `E3001`, `E3002` | Qualify as `module.name` | qualifie l'identifiant si le module est déjà importé |
| `E3001`, `E3002` | Create function `name (…) : T` | si le nom est un argument de type fonction, crée la fonction avec les paramètres et le retour attendus |
| `E3012` | Import `type name` | même chose pour un type inconnu |
| `E3027` | Import `Constructor` | même chose pour un constructeur |
| `W3001` | Remove unused function `name` | supprime la déclaration complète |
| `E0517` | Wrap the argument in `borrow (…)` | ajoute un borrow autour de l'argument |
| `E3003` | Add missing arguments | complète l'appel avec des placeholders du bon type |
| `E3003` | Remove extra arguments | supprime les arguments en trop |
| `E3004` | Convert with `int_to_string` / `float_to_string` | enveloppe un argument numérique attendu en string |
| `E3004` | Wrap in `borrow (...)` | enveloppe un argument quand le paramètre attend un borrow |
| `E3004` | Add missing record field `y` | complète un littéral record d'après le type attendu |
| `E3019` | Add missing tuple elements | ajoute `, _` aux patterns de tuple trop courts |
| `E3026` | Remove the constructor argument / Add the constructor argument | corrige l'arité d'un constructeur |
| `E3041`, `E3044` | Use `array_get_borrow` / `map_get_borrow` | remplace le getter qui copie par la variante borrow |
| `E3015` | Remove `Copy` annotation | retire ` : Copy` de la déclaration |
| `E3016` | Declare the type `Linear` | remplace `Affine` par `Linear` |
| `E3010` | Add `main` function | ajoute `fun main () = ()` en fin de fichier |
| `E0512` | Mark the callback parameter as `consume` | insère `consume ` devant le type du paramètre de callback de la fonction appelée, quand sa déclaration est dans le fichier courant |
| `E0513` | Mark the callback parameter as `consume` | même action : transforme `f : T -> R` en `f : consume T -> R` pour que l'appelant transfère l'argument |

Les actions restent conservatrices : un edit ne modifie qu'une déclaration
ou une branche, et n'essaie jamais de réécrire un programme complet.

### Refactors

Deux actions `refactor.rewrite` s'appliquent sans diagnostic :

- **Pattern match on this value** : sur une expression de type datatype, la
  remplace par un `case` complet, un bras par constructeur. Les payloads
  record sont dépliés (`Square ({ side = side })`) et les branches ont un
  corps `()` à remplir. L'action n'est proposée que si l'expression tient sur
  une ligne et commence en début de ligne, pour ne pas casser l'indentation.
- **Add type annotation `: T`** : sur le nom d'un `val` sans annotation dont
  le type est inféré, insère ` : T` après le nom. Les types encore partiellement
  inférés (variables libres) sont ignorés.
- **Destructure into `(x, y)` / `{ x = x, y = y }`** : sur `val name = expr`
  dont le type est un tuple ou un record, remplace le nom par un pattern
  complet. Les tuples reçoivent `x`, `y`, `z`, … ; les records utilisent leurs
  noms de champs.

`E0518` est publié quand une fonction locale capturante est utilisée comme
valeur, retournée ou stockée. Le diagnostic explique que Yack n'a pas de
closures capturantes et propose deux formes valides : appeler la fonction
directement, ou la déplacer au niveau module en passant les captures comme
paramètres explicites. Il n'y a pas encore de quick fix automatique pour ce
code.

`E0519` est publié quand un binding masque une variable capturée ou une
fonction locale. Le message indique le nom fautif et demande de le renommer.
Le shadowing ordinaire de `val`, hors de ce périmètre, reste accepté.

## Architecture

```text
src/json.sml           parseur / sérialiseur JSON minimal (UTF-8, \uXXXX)
src/lsp_protocol.sml   framing Content-Length, notifications et réponses
src/lsp_text.sml       URI, positions 1-based → LSP UTF-16, ranges
src/lsp_docs.sml       commentaires `(* ... *)` et signatures
src/lsp_ast.sml        requêtes AST (exports, imports, patterns, case)
src/lsp_project.sml    analyse via Driver.loadWith + index des modules
src/lsp_state.sml      documents ouverts, cache d'analyse, cache d'index
src/lsp_types.sml      types record, datatypes et constructeurs pour l'IDE
src/lsp_diag.sml       conversion et publication des diagnostics
src/lsp_completion.sml complétion (mots-clés, prélude, builtins, modules)
src/lsp_hints.sml      inlay hints et hover
src/lsp_actions.sml    quick fixes déduits des diagnostics
src/lsp_rename.sml     rename local et par type
src/lsp_tokens.sml     semantic tokens
src/lsp_calls.sml      call hierarchy
src/lsp_signature.sml  signature help
src/lsp_nav.sml        définition et références
src/lsp_symbol.sml     document symbols et folding
src/lsp_format.sml     formatage du document via Fmt
src/lsp.sml            capacités, dispatch JSON-RPC et boucle serveur
src/driver.sml         loadWith : reader d'overlay + requireMain + report
src/main.sml           `yack lsp` court-circuite la CLI classique
```

Le serveur maintient :

- la liste des documents ouverts (URI, chemin, texte, version) ;
- un cache d'analyse par chemin ;
- la dernière analyse typée par chemin, utilisée comme repli pour la signature
  help et les builtins quand le buffer ne parse plus ;
- les URI de diagnostics publiés par document ;
- l'index des modules et la racine du workspace.

Une requête qui a besoin de types (`inlayHint`, `hover`, `codeAction`) utilise
le cache ; si le document n'a jamais été analysé, il l'analyse à la demande.

## Configuration éditeur

### Helix

```toml
[language-server.yack-lsp]
command = "/chemin/vers/yack"
args = ["lsp"]

[[language]]
name = "yack"
scope = "source.yack"
file-types = ["yack"]
comment-token = "(*"
language-servers = ["yack-lsp"]
formatter = { command = "/chemin/vers/yack", args = ["fmt", "-"] }
auto-format = true
```

La configuration de référence est dans
[`editors/helix/languages.toml`](../editors/helix/languages.toml).

### Neovim (extrait)

```lua
vim.lsp.start({
  name = "yack",
  cmd = { "/chemin/vers/yack", "lsp" },
  root_dir = vim.fs.root(0, { "yack.toml" }),
})
```

## Limites actuelles

- une trame JSON-RPC malformée peut faire tomber le serveur : le framing ne
  valide pas encore assez les en-têtes et le corps ;
- `references` et `rename` couvrent le fichier et son graphe d'imports chargé ;
  les autres paquets ne sont pas parcourus, et le rename de type reste local au
  fichier ;
- le formatage suit la largeur par défaut du formateur, pas les options du
  client ;
- l'index de modules ignore les dossiers `test`, `bench`, `.git`, `.trail`,
  `.commandcode`, `.yack` et `node_modules` ;
- les positions non ASCII sont converties en UTF-16 depuis les colonnes en
  octets de Yack ; les fichiers ASCII n'ont aucun coût supplémentaire ;
- les quick fixes ne viennent pas du compilateur : le serveur les déduit du
  code et de l'AST.

## Tests

```sh
make test-lsp
```

[`test/lsp.mjs`](../test/lsp.mjs) lance un vrai processus `yack lsp`, écrit des
trames `Content-Length: N\r\n\r\n…` et vérifie :

- les capacités d'initialisation ;
- l'absence de diagnostics sur un fichier valide ;
- les inlay hints et le hover ;
- la signature help, l'outline, le pliage, la définition, les références, le
  formatage, les semantic tokens, les document links et la call hierarchy ;
- le rename cross-file et les imports sélectifs ;
- les code actions typo/import/remove/create-function et les refactors
  pattern-match/annotation ;
- la synchronisation incrémentale et les diagnostics `E3004`/`E3021` ;
- le quick fix « wildcard branch » ;
- la complétion `json.` et `import yack/`.

Le test est aussi exécuté par `make test`.
