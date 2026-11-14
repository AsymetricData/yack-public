# Yack

Yack est un langage de la famille ML, à la syntaxe proche de SML et OCaml :
agréable et simple, où l'on se sent chez soi, confortable à écrire, avec peu de
cérémonie sauf celle que les types imposent. Les types affines et linéaires, des
idées reprises de Rust sans sa lourdeur syntaxique, remplacent le
ramasse-miettes.

L'ownership, les emprunts et les durées de vie sont vérifiés **dès le
frontend**, sur l'AST typé, plutôt que reconstruits dans un backend. Les
libérations sont déterministes, la compilation passe par LLVM jusqu'à un
exécutable natif, et les erreurs sont des diagnostics structurés avec des codes
stables, des notes et des suggestions. Un formateur et un language server sont
intégrés au binaire. Le projet reste expérimental ; la version courante est
`0.2.0`.

La motivation et le modèle mémoire sont racontés plus longuement dans
[les idées derrière Yack](site/idees.html).

```yack
fun fact (n) =
    if n = 0 then 1 else n * fact (n - 1)

fun main () =
    print_int (fact (5))
```

```console
$ ./yack build examples/fact.yack fact && ./fact
120
```

## Installer

Yack produit de l'IR LLVM puis appelle `clang` pour l'exécutable final. Le
compilateur lui-même est un binaire Linux x86_64 statique ; les trois
méthodes ci-dessous donnent le même `yack`.

### Binaire précompilé

Télécharger `yack-linux-x86_64.tar.gz` depuis les
[releases](https://github.com/AsymetricData/yack-public/releases), puis :

```sh
tar -xzf yack-linux-x86_64.tar.gz
export PATH="$PWD/yack-linux-x86_64:$PATH"
yack version
```

L'archive garde `yack` et `lib/` côte à côte : le compilateur trouve donc les
bibliothèques tout seul. `YACK_LIB` (le répertoire qui contient
`yack/yack.toml`) n'est nécessaire que si `lib/` est déplacé ailleurs.

Sur la machine qui compile les programmes Yack, il faut ensuite :

- `clang` 15 ou plus récent. L'IR émis utilise les pointeurs opaques (`ptr`,
  et non plus `i8*`) : vérifié sur la sortie de `yack llvm`, et clang 15 est
  la première version qui les accepte ;
- `libsqlite3-dev` pour le paquet `sql` (l'édition de liens ajoute
  `-lsqlite3`) ;
- `libc6-dev` (ou l'équivalent) pour les en-têtes et les objets de démarrage.

### Docker

L'image par défaut contient `clang`, `libc6-dev` et `libsqlite3-dev` : rien
d'autre à installer pour compiler.

```sh
docker build -t yack .
docker run --rm -u "$(id -u):$(id -g)" -v "$PWD":/work yack build main.yack app
./app

docker run --rm -u "$(id -u):$(id -g)" -v "$PWD":/work yack new hello
cd hello
docker run --rm -u "$(id -u):$(id -g)" -v "$PWD":/work yack run
```

`yack build <fichier> <sortie>` écrit l'exécutable dans le répertoire de
travail monté. Le binaire produit est lié à la libc de l'image : exécutez-le
sur une distribution au moins aussi récente (Ubuntu 24.04) ou dans le
conteneur. Pour obtenir le binaire Yack statique, sans MLton ni image
runtime :

```sh
docker build --target release \
    --build-arg YACK_COMMIT=$(git rev-parse --short HEAD) -o dist .
# dist/yack-linux-x86_64/{yack,lib,README.md}
```

### Depuis les sources

Avec [MLton](http://mlton.org/) et `clang` installés :

```sh
make yack
./yack version
```

La cible par défaut construit `./yack` à partir de `yack.mlb`. Les
pré-requis détaillés sont dans [Démarrage rapide](#démarrage-rapide), et les
commandes de test dans [Tests](#tests).

## État du projet

Yack est un projet de recherche, pas encore un langage prêt pour la production.
Version courante : `0.2.0`.

| Domaine | État |
|---|---|
| Lexer, parser, reprise après erreur | ✅ |
| Inférence Hindley-Milner, génériques, `'a` / `''a` | ✅ |
| Contraintes numériques et monomorphisation | ✅ |
| Entiers multi-largeurs, flottants, records, tuples, datatypes génériques | ✅ |
| `case` / patterns imbriqués, gardes, exhaustivité | ✅ |
| Propriétés mémoire déclarées et effectives | ✅ |
| Moves, use-after-move, `token` / `resource` Linear | ✅ |
| Emprunts partagés/mutables, conflits, provenance (`from`) | ✅ |
| Modules, imports qualifiés/sélectifs/alias, packages et dépendances | ✅ |
| IR typé, LLVM, exécutables natifs | ✅ |
| Pile/tas, drops déterministes, vérification ASan | ✅ |
| Bibliothèque standard `yack/*` (23 modules documentés) | ✅ |
| Mode d'allocation sans heap (`--memory=stack`) | 🟡 expérimental |
| Formateur `yack fmt` | ✅ |
| LSP intégré (`yack lsp`) : diagnostics, hints, complétion, rename, quick fixes | ✅ |
| Fonctions par nom ou `x => ...` | ✅ |
| Pattern matching sur paramètres (`fun f (0) = ...`) | ✅ |
| Types de fonction multi-arguments et `consume` optionnel | ✅ |
| Listes `[1, 2, 3]` et `head :: tail` en pattern | ✅ |
| Patterns de chaîne (`"préfixe/" ^ reste`) et de bytes (`<<"GET ", reste>>`) | ✅ |
| Fermetures capturantes | ⛔ hors modèle |


## Démarrage rapide

Pré-requis :

- [MLton](http://mlton.org/) pour compiler le compilateur ;
- `clang` pour produire les exécutables natifs ;
- Node.js (ou Bun) uniquement pour lancer les tests.

```sh
make                 # produit ./yack
./yack new hello     # crée un projet « Hello from Yack ! »
cd hello
../yack run
```

Ou directement sur les exemples :

```sh
./yack check examples/hello.yack
./yack build examples/hello.yack hello && ./hello
```

## CLI

| Commande | Rôle |
|---|---|
| `yack new [name] [--template hello\|cli\|web\|tui]` | crée un projet ; sur un TTY, demande le nom et le template s'ils sont absents |
| `yack run [file]` | compile puis exécute ; sans fichier, prend `src/main.yack` ou `main.yack` du paquet |
| `yack check <file>` | lex, parse, résolution et vérification de types (silencieux si OK) |
| `yack fmt [options] <file\|->` | formate un fichier ou l'entrée standard |
| `yack resolve <file>` | affiche les liaisons, leurs identifiants et leurs usages |
| `yack types <file>` | affiche les propriétés déclarées → effectives |
| `yack parse <file>` | résume les déclarations parsées |
| `yack arena <file>` | explique ce que chaque bloc `arena` alloue et libère |
| `yack alloc-map <file>` | liste les sites d'allocation et leur portée prévue |
| `yack tokens <file>` | affiche le flux de tokens avec positions |
| `yack ir <file>` | abaisse vers l'IR typé |
| `yack llvm <file>` | imprime l'IR LLVM textuel |
| `yack build <file> <out>` | compile en exécutable via `clang` |
| `yack lsp` | lance le language server sur `stdin`/`stdout` |
| `yack version` | affiche la version |

`yack help` liste les commandes ; `yack help fmt` et `yack help new` détaillent
leurs options.

### Templates

`yack new` propose quatre points de départ. `--template` (ou `-t`) choisit
directement, et sur un TTY la même liste s'affiche sous forme de menu si
l'option est absente :

```sh
yack new hello                    # hello : programme minimal (défaut)
yack new tool --template cli      # cli : options, aide, code de sortie
yack new site -t web              # web : serveur Alpage sur le port 8080
yack new screen --template=tui    # tui : écran terminal avec yack/tui
```

Les templates `cli`, `web` et `tui` déclarent leurs dépendances vers
`lib/yack`, `lib/alpage`, etc. `yack new` les résout depuis l'arbre source
(ou depuis `YACK_LIB`), ce qui permet de créer le projet dans n'importe quel
répertoire.

Les erreurs renvoient le code de sortie `1` et portent un code stable
(`E2002` syntaxe, `E3004` type, `E0402` use-after-move, `E0502` conflit
d'emprunt, etc.).

En terminal, `yack new`, `yack build` et `yack run` affichent des étapes
colorées et animées. `NO_COLOR` désactive la couleur, `YACK_UNICODE=0` les
glyphes Unicode, et `YACK_PROGRESS=0` les animations.

### Formateur

`yack fmt` écrit le source formaté sur la sortie standard, ce qui permet
`yack fmt main.yack > main.formatted.yack` ou `yack fmt - < main.yack`.
`--write` / `-w` réécrit le fichier sur place, et `--check` / `-c` renvoie le
code `1` si le fichier n'est pas déjà formaté (utile en CI).

Les règles sont volontairement simples et déterministes : indentation de
4 espaces, largeur cible de 88 colonnes, une déclaration par bloc séparée par
une ligne vide, imports consécutifs regroupés sans ligne vide entre eux (mais
avec une ligne vide après le bloc), `let`/`case`/`if` structurés sur plusieurs
lignes quand ils ne tiennent pas, et commentaires `(* ... *)` conservés. Le
formateur est idempotent.

`--best-effort` laisse l'entrée inchangée quand elle est syntaxiquement
invalide, ce qui évite qu'un auto-format d'éditeur affiche une erreur pendant
la frappe ; `yack lsp` continue de publier le diagnostic.

### Allocation et modes mémoire

Le backend accepte une option de build :

```sh
./yack build --memory=stack examples/stack_mode.yack stack
```

`--memory=stack` remplace les allocations heap par une région à bump pointer.
La région est réservée avec `mmap` (`MAP_NORESERVE`, commit-on-touch) et
protégée par une page de garde ; `--arena-reserve=4G` change la réservation
(défaut : 1 GiB). Chaque activation possède sa région.

Pour réduire la taille de l'exécutable, `--trim` compile avec `-Os`, retire les
sections inutilisées (`-ffunction-sections`, `-fdata-sections`,
`--gc-sections`) et supprime la table de symboles. Les builds AddressSanitizer
(`--arena-debug=poison`) gardent leurs symboles pour les rapports.

Pour la mémoire temporaire, utilisez un bloc `arena` :

```yack
fun render (request : string) : string =
    arena
        val path = string_slice (borrow request, 0, 8)
        val body = "hello " ^ path
    in
        body
    end
```

À `end`, Yack libère d'un coup les chaînes temporaires du bloc. Les tableaux,
maps et données qui dépassent le bloc restent dans la région racine. Le
compilateur vérifie qu'une valeur temporaire ne s'échappe pas du bloc, et
`yack arena fichier.yack` explique ce qui est libéré et ce qui survit :

```console
$ yack arena report.yack
arena in `report`, lines 5–11, heap mode

Freed at `end` (3)
- 6:20 `^` concatenation
- 6:32 `int_to_string`
- 10:9 `^` concatenation

Stored to the root: the container outlives the arena (1)
- 8:35 `^` concatenation — value of `array_push`

Not arena memory: outlive the block (1)
- 7:20 `array_new` — heap, freed when its owner is dropped

Strings allocated by `label` while the block runs are freed at `end` too.

Value: `string` — copied out of the arena before it is released.
```

Le rapport part des sites d'allocation réels du compilateur (pas du nom des
fonctions) et suit le mode mémoire : en stack, chaque conteneur indique sa
région (fonction, appelant, racine). Le LSP affiche le résumé en inlay hint
après `arena` (`3 strings freed at end · 1 stored to root · …`), et le
détail au survol, avec des liens vers chaque site. En mode heap,
l'arena utilise de gros blocs et les recycle entre les blocs pour éviter les
`malloc`/`free` par chaîne ; en heap, `--arena=noop` désactive
l'optimisation sans changer le source, ce qui est utile pour comparer. En
stack, `--arena=noop` est rejeté : sans arène de requête, les temporaires qui
échappent n'ont pas de durée de vie bornée.

### Diagnostiquer les allocations

`yack alloc-map` liste les sites d'allocation prévus par le compilateur :

```sh
yack alloc-map app.yack
yack alloc-map --json app.yack > /tmp/alloc-map.json
```

Chaque site porte un identifiant stable, sa fonction, sa position et son type
(`string concat`, `constructor`, `array_new`, etc.). Les colonnes
`destination`, `allocates` et `reason` distinguent une simple lecture
(`read`) d'une allocation réelle et indiquent la région réelle : `heap`,
`activation`, `destination`, `arena` ou `root`. En mode stack, un site
`root` hors arène est signalé comme potentiellement répété ; c'est le cas des
conteneurs qui restent en racine (M3). `yack --memory=stack arena app.yack`
affiche la même destination pour chaque allocation. Ensuite :

```sh
yack build --trace-alloc app.yack app-trace
./app-trace 2> /tmp/alloc.log
tools/alloc-report.py --map /tmp/alloc-map.json /tmp/alloc.log
```

Le rapport donne, par site, les allocations, les libérations, les octets
alloués/libérés et les octets encore vivants. Le même identifiant apparaît
dans l'IR (`sys.trace_enter`) et dans le LLVM (`; yack.alloc site=...`).

Le mode par défaut peut venir du `yack.toml` :

```toml
[memory]
mode = "stack"     # heap (défaut) ou stack
arena = "on"       # on (défaut) ou noop
reserve = "1G"
```

`--memory=...` et `--arena=...` sur la ligne de commande ont priorité sur le
manifeste. `yack new` génère toutes les options commentées.

C'est expérimental : la vérification statique complète des échappements
inter-procéduraux reste à faire.

Pour diagnostiquer une fuite ou un double free, `--debug-alloc` instrumente
le runtime heap : chaque `malloc`, `realloc` et `free` est imprimé sur stderr
avec son adresse et sa taille, puis un résumé est affiché à la sortie de
`main`.

```sh
./yack build --debug-alloc examples/nested_field_move.yack trace
./trace
# [yack] alloc 0x... size 4
# [yack] free 0x...
# [yack] allocs=2 frees=2 net=0
```

Une allocation sans `free` correspondant indique une fuite ; un `free` en
double ou d'une adresse inconnue indique un double free. Le mode est sans
effet avec `--memory=stack`, où les libérations sont des no-ops.

## Langage et bibliothèque standard

Le noyau couvre les fonctions de niveau module, la récursion (y compris
mutuelle), `let` déstructurant, `if`, `case` avec gardes, le pipe `|>`, les
records, tuples, datatypes génériques, les entiers de largeur explicite
(`int1` … `int64`), `float`/`float32`/`float64`, les emprunts
(`borrow`/`&`, `mut`, `from`) et les propriétés `Copy`/`Affine`/`Linear`.

Les fonctions se passent **par nom** (`list.map (double, values)`) ou sous
forme inline avec `x => ...` (`list.map (x => x * 2, values)`). Une valeur de
fonction ne capture aucun environnement : utiliser une variable libre est
rejeté avec `E0511`. La syntaxe `fn` n'existe pas ; il n'y a ni fermeture, ni
application partielle.

Les fonctions locales sont supportées par lambda lifting :

```yack
fun main () =
    let val base = 40
        fun add (x) = x + base
    in print_int (add (2))
    end
```

Une fonction locale qui capture des variables ne peut être appelée que
directement. La retourner, la stocker ou la passer comme callback est refusé
avec `E0518` : Yack n'a pas de closures capturantes.

Une fonction qui s'appelle elle-même est automatiquement considérée comme
récursive. Le mot-clé `rec` permet de l'annoncer explicitement :

```yack
rec count (n : int, acc : int) : int =
    if n <= 0 then acc else count (n - 1, acc + 1)
```

Un appel récursif en position terminale à la fonction elle-même devient une
boucle dans le backend : la pile ne grandit pas, même à `-O0` ou sous ASan.
Les appels terminaux entre fonctions différentes sont annotés `tail` (non
garantis). Le compilateur n'utilise pas `musttail`, refusé par certains
backends pour les retours agrégés.

Les paramètres de fonction acceptent des patterns et plusieurs équations :

```yack
fun fact (0) : int = 1
fun fact (n) : int = n * fact (n - 1)

fun first (Cons (value, _)) : int = value
fun first (Nil) : int = 0
```

Les listes s'écrivent entre crochets, avec `[]` pour la liste vide. Dans un
motif, `::` décompose une liste en tête et reste :

```yack
fun sum (values : int list) : int =
    case values of
        [] => 0
      | value :: rest => value + sum (rest)

fun main () = print_int (sum ([1, 2, 3]))
```

`[1, 2]` se désucre en `Cons ((1, Cons ((2, Nil))))`.

Les types de fonction distinguent `(int, int) -> int` (deux paramètres) de
`(int * int) -> int` (un paramètre tuple). L'annotation optionnelle
`consume value -> int` indique qu'un callback consomme son argument. Les
boucles n'existent pas ; la récursion terminale est la forme d'itération
prévue, et un appel terminal à soi-même est compilé en saut, sans croissance
de pile.

Les modules suivent une syntaxe proche de Gleam :

```yack
import yack/os
import yack/option
import modules/Math as M
import modules/Shapes.{type point, Circle, area}
```

Un package se déclare avec un `yack.toml` :

```toml
[package]
name = "app"
version = "0.1.0"

[dependencies]
util = { path = "../lib" }

[memory]
mode = "heap"   # or "stack"
```

La bibliothèque standard est dans [`lib/yack/src`](lib/yack/src), avec 23
modules et 509 déclarations documentées :

| Domaine | Modules |
|---|---|
| Bases | `bool`, `int`, `char`, `float`, `string`, `bytes` |
| Collections | `list`, `array`, `map`, `sort` |
| Erreurs | `option`, `result` |
| Système | `io`, `os`, `path` |
| Texte et formats | `fmt`, `math`, `json` |
| Terminal et CLI | `ansi`, `args`, `cli`, `term`, `tui` |

Le package optionnel [`lib/net`](lib/net) fournit `net/Net`, `net/Http`,
`net/Server`, `net/Url` et `net/Buffer` (partiel, POSIX).

Le package optionnel [`lib/web`](lib/web) fournit un framework HTTP minimal :
`Request`, `Response`, `Router`, `Middleware`, `Cookie`, `Session`, `Html` et
`Server`. API, exemples et limites dans [`docs/web.md`](docs/web.md).

L'API et les limites de chaque module sont décrites dans
[`docs/stdlib.md`](docs/stdlib.md).

## LSP

`yack lsp` expose le frontend comme language server JSON-RPC sur `stdin` /
`stdout`. Le serveur est natif : il ne dépend ni de Node ni d'un autre
compilateur, et réutilise exactement le même chargement de modules, la même
inférence et le même borrow checker que `yack check`.

Fonctionnalités :

- diagnostics `yack check` publiés à l'ouverture et à chaque modification
  (lexer, parser, résolution, types, ownership) ;
- inlay hints des types inférés pour les paramètres, `val` et patterns non
  annotés ;
- complétion des mots-clés, builtins, prélude, déclarations locales, modules
  importés, membres qualifiés (`string.`, `json.`), chemins d'import
  (`import yack/`), champs de payload record et constructeurs de `case`, avec
  signature et documentation des déclarations ;
- hover avec signature, commentaire `(* ... *)` placé juste au-dessus, ou type
  inféré de l'expression/liaison sous le curseur ;
- quick fixes pour les erreurs courantes : branches de `case` manquantes
  (constructeurs et champs de record), branche `_ =>`, branche inatteignable,
  annotation `Copy`/`Affine` à corriger, `main` manquant, et ajout de
  `consume` sur un paramètre de callback (`E0512`/`E0513`).

Les sources non sauvegardées sont servies depuis les buffers du client, y
compris pour les modules importés. Les positions suivent UTF-16 comme le veut
le protocole. La synchronisation est « full text » pour rester simple et
robuste.

```json
{
  "language-server": {
    "yack": { "command": "/chemin/vers/yack", "args": ["lsp"] }
  }
}
```

Le détail des méthodes, des limites et des tests est dans
[`docs/lsp.md`](docs/lsp.md).

## Architecture

```text
source .yack
  → lexer      tokens + positions
  → parser     AST + reprise après erreur
  → resolve    noms → identifiants de liaison stables
  → check      inférence, propriétés mémoire, borrow checker
  → lower      IR typé / owned
  → llvm       LLVM IR → clang → exécutable natif
```

Modules principaux :

- `src/lexer.sml`, `src/parser.sml`, `src/ast.sml` : frontend ;
- `src/resolve.sml` : résolution des noms ;
- `src/types.sml`, `src/check.sml` : types et diagnostics ;
- `src/ownership.sml` : propriétés, moves et emprunts ;
- `src/ir.sml`, `src/lower.sml`, `src/llvm.sml` : codegen ;
- `src/fmt.sml` : formateur de source ;
- `src/json.sml`, `src/lsp_*.sml` : JSON minimal et language server modulaire
  (`protocol`, `text`, `ast`, `project`, `state`, `diag`, `completion`,
  `hints`, `actions`) ;
- `src/driver.sml`, `src/main.sml` : CLI.

Le backend monomorphise les fonctions génériques et numériques, place en pile
les valeurs qui ne s'échappent pas, et émet les `free` après le dernier
propriétaire. Les performances du frontend et les points encore à optimiser
sont documentés dans [`docs/performance.md`](docs/performance.md).

## Tests

```sh
make test             # frontend, diagnostics, codegen, exemples, LSP et docs
make test-lsp         # smoke test JSON-RPC du language server
make test-stdlib-docs # couverture documentation de lib/yack/src
make test-asan        # compilation + exécution sous AddressSanitizer
make test-stack       # mode heap-free : comparaison heap/stack et IR sans malloc
```

La suite vérifie les 116 exemples `.yack` de `examples/` (compilation et
exécution natives), les 98 programmes de `test/reject/` avec leur code
d'erreur, les projets multi-modules et multi-packages, le formateur (dont
l'idempotence), le LSP, et la documentation des 509 déclarations de
`lib/yack/src`.

## Documentation

- [`tutorial/`](tutorial/) : tutoriel rapide pour apprendre Yack ;
- [`docs/performance.md`](docs/performance.md) : complexité et pistes
  d'optimisation du frontend ;
- [`docs/stdlib.md`](docs/stdlib.md) : bibliothèque standard `yack/*` ;
- [`docs/args-tutorial.md`](docs/args-tutorial.md) : mini-tuto CLI avec
  `yack/args` et les spec lists ;
- [`docs/lsp.md`](docs/lsp.md) : language server intégré ;
- [`examples/`](examples/) : programmes valides et représentatifs.

## Éditeurs et outillage

- [`editors/tree-sitter-yack/`](editors/tree-sitter-yack/) : grammaire
  Tree-sitter, queries et scanner de commentaires imbriqués ;
- [`editors/helix/`](editors/helix/) : configuration Helix avec `yack lsp` et
  `yack fmt -` ;
- [`docs/lsp.md`](docs/lsp.md) : méthodes LSP, architecture et limites ;
- [`tools/yack-watch.ts`](tools/yack-watch.ts) : watcher de lint terminal pour
  la CI ou les éditeurs sans client LSP ;
- [`examples/cli`](examples/cli/) contient quatre CLI : `ls` (`-al`, tailles,
  tri), `cat`, `wc` et une démo `yack/ansi` + `yack/tui`, avec `os.exit` et
  les codes de sortie ; guide pas à pas dans [`site/cli.html`](site/cli.html).

```sh
make watch        # panneau de lint sur examples/stdlib/src/main.yack
make watch-once   # un seul check, code de sortie 1 en cas d'erreur
```

## Limites connues

- fonctions par nom ou `x => ...` sans capture (pas de fermetures) ;
- fonctions locales par lambda lifting, mais pas de closure capturante (`E0518`) ;
- pas de boucles, de threads ni de concurrence ;
- pas de varargs ni d'interfaces/type classes ;
- la bibliothèque standard est utilisable mais partielle, notamment pour le
  réseau, les dates et l'encodage ;
- le LSP est en synchronisation full text et ne fournit pas encore `rename`,
  `references`, `documentSymbol` ni `formatting` ;
- le mode `--memory=stack` est expérimental : l'analyse inter-procédurale
  des valeurs qui sortent d'un bloc `arena` n'est pas encore complète ;
- le backend cible Linux/POSIX.

## Licence

MIT. Voir [`LICENSE`](LICENSE).
