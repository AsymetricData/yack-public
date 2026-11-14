# Yack : bibliothèque standard (`yack/*`)

> **Statut : draft de conception + première tranche compilable.**
> Le paquet vit dans `lib/yack/`, pas dans `Driver.preludeText`.
> Les modules déjà écrits compilent avec le compilateur actuel ; les modules
> marqués ⏳ dépendent de primitives qui n'existent pas encore.

## 1. Principes

La stdlib de Go est un bon catalogue de référence, pas un modèle directement
portable. Go s'appuie sur des interfaces, la réflexion, les goroutines, les
closures et un GC. Yack n'a aujourd'hui aucune de ces briques. La stdlib Yack
doit donc être :

- **explicite** : pas de prélude magique, pas d'import global implicite ;
- **possédée** : les signatures disent qui possède, qui emprunte, qui consomme ;
- **totale** : les erreurs sont des `result`, pas des exceptions ni des panics ;
- **sans état caché** : pas de singleton, pas de buffer global, pas de GC ;
- **spécialisée par type** quand les type classes manquent : `sort_int`,
  `contains_string`, etc. ;
- **pensée pour le LSP** : JSON, décodage dynamique et génération de code sont
  des priorités, pas des annexes. Chaque déclaration publique de
  `lib/yack/src` est documentée par un commentaire `(* ... *)` adjacent,
  avec un résumé, les paramètres nommés et le résultat. C'est cette
  documentation qui alimente le hover et la complétion de `yack lsp`, et
  `make test-stdlib-docs` vérifie la couverture.

## 2. « Pas dans la prelude » : ce que cela veut dire

Aujourd'hui `Driver.preludeText` injecte :

- les datatypes `option`, `result`, `list` ;
- les fonctions `list_length`, `list_tail`, `list_head`, `option_is_some`,
  `result_is_ok`.

Le draft déplace **toute l'API de bibliothèque** dans `lib/yack/`. La prelude
doit rester le noyau du langage :

- types primitifs (`int`, `float`, `bool`, `string`, `unit`, …) ;
- builtins (`print_int`, `print_int64`, `array_*`, `map_*`, `io_*`, `sys.*`,
  conversions, `sprintf`) ;
- éventuellement les trois datatypes universels (`option`, `result`, `list`),
  parce que les builtins les retournent (`string_to_int`, `io.read_file`, …).

Deux options pour la suite :

| Option | Avantage | Coût |
|---|---|---|
| Garder `option`/`result`/`list` comme datatypes du noyau | les builtins restent utilisables partout | la prelude contient encore des types |
| Les déplacer dans `yack/core`, auto-importé | prelude vraiment minimale | il faut un import implicite ou `import yack/core` dans chaque fichier |

Recommandation : garder les **types** dans le noyau, sortir toutes les
**fonctions** vers `yack/*`. Le mot « stdlib » désigne l'API, pas les types
fondamentaux.

## 3. Espace de noms et imports

Le paquet s'appelle `yack`. Les modules sont en minuscules :

```yack
import yack/int
import yack/string
import yack/float
import yack/list
import yack/json

fun main () =
    let val text = "42"
        val value = int.parse (borrow text)
    in case value of
           None => print_int (0)
         | Some (n) => print_int (n)
    end
```

Règles :

- `import yack/string` lie le qualificatif `string` ; on écrit
  `string.trim (borrow text)`.
- Un import sélectif reste possible :
  `import yack/string.{trim, join}`.
- Pas de `open`, pas de glob.
- Les helpers internes sont préfixés `internal_` tant que `pub`/privé n'existe
  pas.
- À terme, `yack` doit être un alias réservé du compilateur, résolu vers le
  paquet embarqué `lib/yack`, sans `[dependencies]` dans `yack.toml`.

Le compilateur et le LSP découvrent déjà `lib/yack` en remontant les parents du
fichier source : un projet placé sous l'arbre de Yack peut donc écrire
`import yack/...` sans dépendance. Hors de cet arbre, un projet déclare :

```toml
[dependencies]
yack = { path = "../../lib/yack" }
```

## 4. Contraintes du langage qui dictent l'API

Ces contraintes ne sont pas des détails : elles expliquent la forme de chaque
module.

### 4.1 Fonctions passées par nom, sans capture

Une fonction de niveau module peut être passée comme valeur
(`list.map (predicate, values)`), et une fonction inline sans capture peut être
écrite `x => ...`. En revanche une fonction ne capture aucun environnement :
utiliser une variable libre est rejeté avec `E0511`. Donc :

- `list.map`, `list.filter`, `list.filter_map`, `list.foldl`, `list.foldr`
  et `list.zip` existent ;
- `option.map`, `option.and_then`, `result.map`, `result.map_error` et
  `result.and_then` existent ;
- `sort_by` avec comparateur est possible en principe, mais pas encore exposé ;
- `Decoder(t)` comme fonction `Dynamic -> Result(t, …)` reste hors modèle ;
- `http.handler` avec callback capturant reste hors modèle.

Conséquences :

- `yack/list` fournit les opérations structurelles polymorphes (`reverse`,
  `append`, `concat`, `nth`, `take`, `drop`), les combinateurs
  (`map`, `filter`, `filter_map`, `foldl`, `foldr`, `zip`) et des opérations spécialisées
  (`sum_int`, `contains_string`, …).
- `yack/sort` expose `sort_int`, `sort_int_list`, `sort_string_list`, etc.
- Le décodage JSON utilise un `decoder` **datatype** (defunctionalisation), pas
  une fonction. Voir §8.

### 4.2 Pas d'interfaces ni de type classes

Pas de `io.Reader`, `io.Writer`, `fmt.Stringer`, `Comparator`, `Hashable`,
`ToJson`. Les APIs sont concrètes :

- `yack/io` read/write des fichiers et du stdin ;
- `yack/json` des fonctions par type ;
- `yack/map` une map à clés `string` seulement.

Une map générique exigerait `hash` et `eq` pour la clé : ce sera possible avec
des type classes ou des dictionnaires explicites, pas avant.

### 4.3 Collections Affine : le threading est explicite

Un `array`, un `map`, un `string` et la plupart des datatypes sont **Affine**.
Une fonction utilisateur qui reçoit un `array` le **déplace**. Les builtins,
eux, ne déplacent pas (`array_length (values)` puis `array_get (values, 0)`
fonctionne). Mais on ne peut pas passer un `borrow array` à `array_get` :

```yack
(* Rejeté : array_get attend 'a array, pas borrow 'a array *)
fun length_of (values : borrow int array) : int =
    array_length (values)
```

Donc les helpers de collection suivent deux styles :

1. **Mutation threadée** : la fonction rend la collection.

```yack
val values = array.set_int (values, 0, 42)
val values = sort.sort_int (values)
```

2. **Builtin direct** pour les lectures non consommatrices :

```yack
val n = array_length (values)
val x = array_get (values, 0)
```

Les opérations qui ne rendent pas la collection (`sum_int`, `to_string`, …)
sont documentées comme **consommatrices**.

### 4.4 Emprunt nommé puis déplacement

Le vérificateur suit maintenant la valeur qui porte un emprunt : un emprunt
nommé meurt à son dernier usage (alias et agrégats compris), et non à la fin
de sa portée. Le programme suivant est donc accepté :

```yack
fun first_and_owner (line : string) : string =
    let val borrowed = borrow line
        val size = string_length (borrowed)
    in
        if size >= 0 then line else line
    end
```

Un emprunt qui n'est jamais lu est mort dès sa création : deux `borrow`
successifs sur la même place ne se chevauchent que si le premier est encore
utilisé. La mutation d'un conteneur est autorisée après le dernier usage d'un
`array_get_borrow`/`map_get_borrow`.

Restent conservateurs : une projection de champ ou de constructeur dont la
sélection n'est pas suivie garde le loan jusqu'au dernier usage du porteur
entier ; les builtins hors de l'allowlist `releasesLoanAtCall` ne rendent pas
un emprunt d'argument avant la fin de l'appel.

L'exemple `strip_cr` ci-dessous passe depuis toujours : `string_length`,
`string_char_at_unchecked` et `string_slice` font partie des builtins dont les
emprunts d'arguments sont relâchés à la fin de l'appel.

```yack
fun strip_cr (line : string) : string =
    let val size = string_length (borrow line)
    in if size > 0 then
           if string_char_at_unchecked (borrow line, size - 1) = 13 then
               string_slice (borrow line, 0, size - 1)
           else line
       else line
    end
```

Conséquences concrètes :

- `string.strip_cr` copie toujours la chaîne et ne rend jamais l'original.
- `sort_string_list` trie une **liste** de chaînes : comparer deux `string`
  exige `borrow`, et NLL termine l'emprunt avant de déplacer la cellule. Le tri
  d'un **tableau** de chaînes reste absent, car il demande `array_swap`.
- Les modules qui inspectent puis rendent une valeur possédée peuvent éviter
  la copie quand le dernier usage de l'emprunt précède le retour.

### 4.5 Pas de boucles, mais une récursion terminale garantie

Pas de `while`/`for`. Les boucles sont des récursions terminales. Un appel
terminal à la fonction elle-même devient un saut vers l'en-tête de la
fonction dans le backend, avec un `phi` par paramètre : la pile ne grandit
pas, même à `-O0` ou sous ASan. Les appels terminaux entre fonctions
différentes restent un indice `tail`, non garanti. La stdlib est écrite dans
ce style.

### 4.6 Détails syntaxiques à retenir

- Pas de `and` pour la récursion mutuelle : les fonctions d'un module sont
  toutes mutuellement visibles, il suffit de les déclarer séparément.
- Pas de `val (a, b) = …` : utiliser `val pair = …`, puis `pair.0`, `pair.1`.
- Les constructeurs à argument tuple demandent des doubles parenthèses :
  `Cons ((value, rest))`, pas `Cons (value, rest)`.
- `borrow` ne s'applique qu'à une place locale :
  `val key = "name"` puis `borrow key` ; pas `borrow "name"`.
- `print_int` ajoute un saut de ligne ; `print_string` non.
- `map_get` rend la valeur zéro si la clé manque ; tester `map_has` d'abord.

## 5. Arborescence

```text
lib/yack/
  yack.toml
  README.md
  src/
    bool.yack       ✅
    int.yack        ✅
    char.yack       ✅
    float.yack      🟡
    option.yack     ✅
    result.yack     ✅
    list.yack       ✅
    string.yack     ✅
    bytes.yack      🟡
    math.yack       ✅
    fmt.yack        ✅
    path.yack       ✅
    array.yack      🟡
    map.yack        🟡
    sort.yack       🟡
    io.yack         🟡
    ansi.yack       ✅
    args.yack       ✅
    cli.yack        ✅
    os.yack         🟡
    term.yack       🟡
    tui.yack        🟡
    json.yack       🟡
    dynamic.yack    ⏳
    encoding/hex.yack      ⏳
    encoding/base64.yack   ⏳
    csv.yack               ⏳
    toml.yack              ⏳
    url.yack               🟡
    set.yack               ⏳
    net.yack               🟡
    http.yack              🟡
    server.yack            🟡
    buffer.yack            🟡
    lsp.yack               ⏳
    lsp/rpc.yack           ⏳
    time.yack              ⏳
    random.yack            ⏳
    log.yack               ⏳
    test.yack              ⏳
```

`✅` compilable et utilisable aujourd'hui, `🟡` partiel ou avec limites
documentées, `⏳` en attente de primitives compilateur.

## 6. Catalogue complet, aligné sur Go

| Go | Yack | État | Notes |
|---|---|---|---|
| `strings` | `yack/string` | ✅ | octets, pas Unicode complet |
| `strconv` | `yack/int`, `yack/float`, `yack/fmt` | 🟡 | float parse/format manquants |
| `bytes` | `yack/bytes` | 🟡 | API consommatrice |
| `slices` | `yack/list`, `yack/array` | ✅ | fonctions sans capture |
| `maps` | `yack/map` | 🟡 | clés `string` seulement |
| `sort` | `yack/sort` | 🟡 | int et listes ; tableau de chaînes attendant `array_swap` (voir §4.4) |
| `math` | `yack/math` | ✅ | sqrt Newton ; floor/ceil manquants |
| `path/filepath` | `yack/path` | ✅ | POSIX |
| `fmt` | `yack/fmt` | ✅ | pas de varargs |
| `io` | `yack/io` | 🟡 | fichiers/stdin ; handles manquants |
| `os` | `yack/os` | 🟡 | args/env via /proc, `exit`, `cwd` ; stat/chdir manquants |
| `time` | `yack/time` | ⏳ | clock/sleep/format |
| `encoding/json` | `yack/json`, `yack/dynamic`, `yack/json/derive` | 🟡 | voir §8 |
| `encoding/hex` | `yack/encoding/hex` | ⏳ | pur, facile |
| `encoding/base64` | `yack/encoding/base64` | ⏳ | pur, facile |
| `encoding/csv` | `yack/csv` | ⏳ | pur |
| `net` | `yack/net` | 🟡 | existe dans `lib/net` |
| `net/http` | `yack/http`, `yack/server` | 🟡 | existe dans `lib/net` |
| `net/http` (framework) | `web/*` | 🟡 | `lib/web` : Router, Response, Middleware, Cookie, Session, Html |
| `net/url` | `yack/url` | 🟡 | existe dans `lib/net` |
| `database/sql` | `sql/Sql`, `sql/Sqlite` | 🟡 | `lib/sql` : adaptateur SQLite, voir `docs/sql.md` |
| `flag` | `yack/args`, `yack/cli` | ✅ | flags, options, positionnels, aide |
| `ansi` | `yack/ansi` | ✅ | couleurs, curseur, écran |
| terminal | `yack/term`, `yack/tui` | 🟡 | rendu ; raw mode manquant |
| `log` | `yack/log` | ⏳ | stderr + time |
| `testing` | `yack/test` | ⏳ | assertions ; exit/argv |
| `regexp` | `yack/regexp` | ⏳ | moteur à écrire |
| `crypto/*` | `yack/crypto` | ⏳ | bitwise + RNG + constant-time |
| `compress/*` | `yack/compress` | ⏳ | bitwise |
| `archive/*` | `yack/archive` | ⏳ | bitwise + IO |
| `image/*` | `yack/image` | ⏳ | bas niveau |
| `unicode/utf8` | `yack/utf8` | ⏳ | décodage UTF-8 |
| `reflect` | aucun | ❌ | pas de réflexion |
| `unsafe` | aucun | ❌ | hors modèle mémoire |
| `context`, `sync`, goroutines | `sys.thread_*` | 🟡 | threads POSIX bas niveau ; voir `docs/threads.md` |
| `go/*` | `yack/ast`, `yack/format` | ⏳ | outillage futur |
| `database/sql` | `yack/db` | ⏳ | drivers + IO |
| `html/template` | `yack/template` | ⏳ | closures/interfaces |

## 7. API du noyau implémenté

### `yack/bool`

```yack
not : bool -> bool
and : bool * bool -> bool
or : bool * bool -> bool
xor : bool * bool -> bool
to_string : bool -> string
from_string : borrow string -> bool option
compare : bool * bool -> int
```

### `yack/int`

```yack
abs, min, max, clamp, sign : int ...
wrapping_add, wrapping_sub, wrapping_mul : int * int -> int
wrapping_add64, wrapping_sub64, wrapping_mul64 : int64 * int64 -> int64
is_even, is_odd : int -> bool
pow : int * int -> int
gcd, lcm : int * int -> int
parse : borrow string -> int option
to_string : int -> string
compare : int * int -> int
```

`+`, `-` et `*` échouent au débordement. `wrapping_*` rendent le résultat
modulo 2³², pour les hachages et les checksums ; le calcul passe par `int64`,
donc il ne peut pas échouer. `wrapping_*64` font de même en `int64`, modulo
2⁶⁴ (FNV-1a 64 bits, par exemple).

Les variantes `int8`/`int64` restent à dupliquer depuis les builtins
(`int8_to_string`, `string_to_int64`, …).

### `yack/char`

```yack
code : borrow string -> int
from_code : int -> string
is_digit_code, is_alpha_code, is_alnum_code,
is_space_code, is_upper_code, is_lower_code,
is_printable_code : int -> bool
to_upper_code, to_lower_code : int -> int
is_digit, is_alpha, is_alnum, is_space, is_upper, is_lower :
    borrow string -> bool
to_upper, to_lower : borrow string -> string
compare : borrow string * borrow string -> int
```

`char` est un octet, pas un scalaire Unicode.

### `yack/string`

```yack
copy, length, is_empty : ...
char_at : borrow string * int -> int option
char_at_unchecked : borrow string * int -> int
slice : borrow string * int * int -> string
equals, compare : borrow string * borrow string -> ...
starts_with, ends_with, contains : ...
index_of, last_index_of : ...
trim, trim_left, trim_right : borrow string -> string
repeat : borrow string * int -> string
join : borrow string * string list -> string
split : int * borrow string -> string list
split_on : borrow string * borrow string -> string list
lines : borrow string -> string list
to_upper, to_lower, reverse : borrow string -> string
replace : borrow string * borrow string * borrow string -> string
count : borrow string * borrow string -> int
pad_left, pad_right : borrow string * int * int -> string
```

Limites : indexation par octet, pas de `string_builder`, concaténation `^`
O(n) par appel. `lines` accepte `\n` et retire un `\r` final.

### `yack/list`

Structurel et polymorphe :

```yack
length, is_empty, head, tail, reverse, append, concat,
nth, take, drop, range, repeat_int
```

Combinateurs :

```yack
map, filter, filter_map, foldl, foldr, zip,
borrow_map, borrow_map_int
```

Les listes s'écrivent entre crochets : `[1, 2, 3]`. La liste vide est `[]`.

`borrow_map` et `borrow_map_int` parcourent une liste empruntée (`borrow values`)
et la laissent disponible. `borrow_map` passe chaque élément comme `borrow 'a`
(utile pour les éléments Affine comme `string`) ; `borrow_map_int` passe les
entiers Copy par valeur, car un `borrow int` ne peut pas être relu comme `int`.

Spécialisé :

```yack
sum_int, max_int, min_int
contains_int, index_of_int
contains_string, index_of_string
any_true, all_true
zip_int_string
```

`list.head`/`tail` consomment la liste, comme les helpers actuels de la
prelude. `reverse`, `append`, `concat`, `nth`, `take`, `drop` déplacent les
cellules sans dupliquer les éléments.

### `yack/option`, `yack/result`

```yack
option.is_some, is_none, unwrap_or, to_result, from_result, compare,
       map, and_then
result.is_ok, is_error, unwrap_or, error_or, to_option, from_option,
       map, map_error, and_then
```

`unwrap_or` suppose que la valeur par défaut peut être abandonnée (Copy ou
Affine). Les valeurs `Linear` demandent une API dédiée.

### `yack/math`, `yack/float`

```yack
math.abs_int, min_int, max_int, clamp_int, sign_int
math.is_even, is_odd, gcd, lcm, pow_int
math.abs_float, min_float, max_float, clamp_float, sqrt
math.pi, math.e

float.abs, min, max, clamp, sign, is_zero, pow, sqrt, pi, e
```

`float` n'a ni parse, ni format, ni `floor`/`ceil`/`round`/`is_nan`. `sqrt` est
une itération de Newton.

### `yack/fmt`

```yack
format_int, format_int64, format_float, format_bool, format_char
string_to_string : borrow string -> string
newline, tab
print_line, print_bool_line, print_string_line
join, concat, repeat, pad_left, pad_right, quote
```

`sprintf` est un builtin du noyau, pas une fonction variadique de `yack/fmt` :

```yack
sprintf ("%s = %d", (name, count))
```

Le format doit être un littéral. Pour les cas dynamiques, composer avec `^`,
`int_to_string`, `float_to_string` et `string.join`.

### `yack/path`

```yack
is_absolute, base, dir, extension, strip_extension,
join, join_all
```

POSIX uniquement (`/`).

### `yack/array`

La couche publique est écrite en Yack au-dessus d'un petit noyau de
primitives (`array_kernel_*`, réservé à la stdlib). Les tests de bornes et la
politique de croissance sont donc dans `lib/yack/src/array.yack`, vus par le
vérificateur et le graphe ; le noyau LLVM ne garde que l'accès mémoire générique
d'un élément, sa destruction typée, le rooting arène/pile et l'abandon du
runtime.

```yack
new : unit -> 'a array
make : ''a * int -> ''a array
make_uninit : int -> ''a array
make_float : float * int -> float array
length, capacity : borrow 'a array -> int
get : borrow ''a array * int -> ''a
get_int : borrow int array * int -> int
get_float : borrow float array * int -> float
get_borrow : borrow 'a array * int -> borrow 'a from 1
set : 'a array * int * 'a -> 'a array
set_int, set_int_unchecked : int array * int * int -> int array
put : 'a array * int * 'a -> 'a array
take : 'a array * int -> 'a array * 'a
push, push_unchecked : 'a array * 'a -> 'a array
```

```yack
reverse_int : int array -> int array
copy_int : borrow int array -> int array
sum_int, max_int, min_int : borrow int array -> ...
index_of_int, contains_int : borrow int array * int -> ...
```

`push` contrôle la capacité et double le buffer (minimum 4) avant d'écrire ;
`push_unchecked` s'arrête si le tableau est plein. `set` et `take` vérifient
l'indice puis laissent le compilateur détruire l'ancienne valeur. Tous les
accès par index arrêtent le programme hors bornes avec
`index N out of bounds (length L)` sur stderr, en heap comme en stack.

Le noyau reste accessible aux builtins historiques (`array_get`, `array_set`,
`array_push`, …), qui gardent leur implémentation LLVM le temps de comparer
les deux.
Un programme utilisateur qui appelle directement
`array_kernel_*` est refusé (`E3042`) : ces primitives lisent et écrivent une
case sans tester l'indice.

`array_take` déplace l'élément hors du tableau et laisse un trou dans le
slot ; `array_put` écrit une nouvelle valeur sans tenter de détruire ce trou.
Lire un trou qui tient un pointeur (datatype, array, map, closure, ou un
record ou tuple qui en contient un) arrête le programme
(`array element N was already taken`) au lieu de rendre un pointeur nul.

Tout accès par index (`array_get`, `array_get_borrow`, `array_set`,
`array_take`, `array_put` et leurs `_unchecked`) vérifie les bornes : hors
du tableau, le programme s'arrête avec `index N out of bounds (length L)`
sur stderr. Il n'y a plus de valeur zéro de repli.
Ils permettent de sortir et remettre un contexte Affine sans le cloner.
`array_put` normalise immédiatement la valeur vers la racine : elle peut donc
survivre à une arène qui l'a produite.

Pour les tableaux internes dont le tableau lui-même est normalisé à la sortie
d'une arène, `array_put_arena` diffère cette normalisation. C'est un builtin
interne réservé aux tableaux qui restent dans l'arène courante. Le compilateur normalise les
tableaux écrits avant `scratch_leave`, donc un tableau extérieur ne garde pas
de pointeur scratch même s'il n'est pas la valeur retournée par l'arène.

### `yack/map`

```yack
new : unit -> 'v map
set : 'v map * string * 'v -> 'v map
remove : 'v map * string -> 'v map
keys : 'v map -> string array * 'v map
```

Clés `string` uniquement. `map_get` rend zéro si absent ; `map_has` pour
distinguer. `map_get` refuse les valeurs Affine ; `map_get_borrow` est le
builtin à utiliser.

### `yack/bytes`

```yack
from_string : borrow string -> int8 array
to_string : int8 array -> string
to_string_range : int8 array * offset * count -> string
set, set_unchecked : int8 array * int * int8 -> int8 array
copy, equal, compare, slice : ...
```

`to_string_range (values, offset, count)` copie `count` octets à partir de
l'indice `offset` ; la borne de fin est `offset + count`, pas une borne
absolue. Le builtin `bytes_to_string_range` a la même signature
`(values, offset, count)`.

### `yack/io`

```yack
read_file : string -> string result
write_file : string * string -> unit result
read_stdin : unit -> string result
print_raw, print, print_line : borrow string -> unit
print_lines : string list -> unit
eprint, eprint_line : borrow string -> unit
```

`print_string` ajoute un saut de ligne ; `print_raw` utilise `print_span` et
écrit brut. C'est indispensable pour le TUI et les séquences ANSI.
`print_lines` écrit une ligne par élément, pour une liste de résultats déjà
calculée.

`read_stdin` lit **tout** stdin ; pour le LSP il faudra `read_line` et
`read_exact`.

### `yack/sort`

```yack
sort_int : int array -> int array
sort_int_list : int list -> int list
insert_string : string * string list -> string list
sort_string_list : string list -> string list
binary_search_int : int array * int -> int option * int array
```

`sort_string_list` trie une liste par ordre des octets (`string.compare`). Le
tri d'un **tableau** de chaînes exige encore `array_swap` pour déplacer un
élément Affine ; voir §4.4.

## 8. CLI, ANSI et mini-TUI

Le paquet fournit une chaîne complète pour écrire un CLI sans dépendance :

| Module | Rôle |
|---|---|
| `yack/args` | parsing pur de `string list` : flags, options, positionnels, `--`, `--name=value`, aide |
| `yack/cli` | aide globale, erreurs, versions, codes de sortie conventionnels |
| `yack/ansi` | couleurs, styles, curseur, écran alterné, effacement |
| `yack/term` | largeur/hauteur via `COLUMNS`/`LINES`, helpers d'écran |
| `yack/tui` | rendu de boîtes, menus, barres de progression, spinner |
| `yack/os` | `args`, `env`, `getenv`, `cwd`, `exit` |
| `yack/io` | `read_file`, `write_file`, `print`, `eprint`, `eprint_line` |

### 8.1 Arguments

`yack/args` ne dépend pas du processus : `parse` prend une `string list`.

Mini-tuto pas à pas : [`docs/args-tutorial.md`](args-tutorial.md).

```yack
import yack/args
import yack/os

fun specs () : args.spec list =
    [
        args.Flag (("verbose", Some ("v"), "verbose output")),
        args.Opt (("output", Some ("o"), "output file", None)),
        args.Positional (("file", "input file", true))
    ]

fun main () =
    let val argv = os.command_args ()
        val parsed = args.parse (specs (), argv)
    in
        case parsed of
            Error (message) => cli.print_error (message)
          | Ok (value) =>
                let val pair = args.take_flag (value, "verbose")
                    val verbose = pair.0
                    val rest = pair.1
                in ...
                end
    end
```

Les accesseurs rendent `(valeur, parsed restant)` :

- `take_flag : parsed * string -> bool * parsed`
- `take_option : parsed * string -> string option * parsed`
- `take_option_or : parsed * string * string -> string * parsed`
- `positionals : parsed -> string list` (consomme le reste)

C'est volontaire : les listes sont Affine, et un accesseur qui rendrait juste
`bool` consommerait le `parsed`. Le style `pair.0` / `pair.1` est explicite et
compatible avec le vérificateur actuel.

Syntaxe reconnue : `--long`, `--long=value`, `--long value`, `-s`,
`-s value`, `-s=value`, `-abc`, `--`, puis les positionnels. Un cluster
`-abc` est accepté quand chaque lettre est l'alias court d'un `Flag` : c'est
ce qui rend `ls -al` littéral. Un alias d'`Opt` dans un cluster reste une
erreur (donner la valeur séparément). `--help` et `--version` désactivent la
validation des positionnels requis, pour que l'aide fonctionne sans argument.

### 8.2 Sortie standard, erreurs et code de sortie

- `print_string` écrit sur stdout **avec** un saut de ligne (le runtime utilise
  `%.*s\n`).
- `yack/io.print_raw` (alias `print`) écrit sans saut de ligne via
  `print_span` ; `yack/io.print_line` ajoute exactement un `\n`. Utiliser
  `print_raw` pour un TUI, du JSON compact ou des séquences ANSI.
- `yack/io.eprint` / `eprint_line` écrivent sur `/dev/stderr` (Unix). Ce n'est
  pas un builtin : le module ouvre `/dev/stderr`, écrit, referme.
- `os.exit : int -> unit` est un builtin (`exit(3)`), donc `os.exit_ok`,
  `os.exit_usage` et `os.exit_failure` sont disponibles.
- `yack/cli.print_error` ajoute le préfixe `error:` et écrit sur stderr.
- `yack/cli.print_help`, `print_global_help`, `print_version` écrivent sur
  stdout.

Exemple de code de sortie :

```yack
case args.parse (specs (), os.command_args ()) of
    Error (message) =>
        let val _ = cli.print_error (message)
        in os.exit_usage ()
        end
  | Ok (value) => ...
```

### 8.3 ANSI

`yack/ansi` rend des chaînes, jamais des effets de bord :

```yack
print_string (ansi.fg_green () ^ "ok" ^ ansi.reset () ^ "\n");
print_string (ansi.bold () ^ "title" ^ ansi.reset () ^ "\n");
print_string (ansi.cursor_to (3, 1) ^ "ligne 3");
```

Disponible : `bold`, `dim`, `italic`, `underline`, `reverse`, couleurs 8/16,
`color256`, `rgb`, `clear_screen`, `clear_line`, `home`, `cursor_to`,
`cursor_up/down/left/right`, `hide/show_cursor`, `alternate_screen`,
`save/restore_cursor`.

### 8.4 Mini-TUI

`yack/tui` construit un écran texte :

```yack
print_string (tui.box ("cli-demo", body, 60) ^ "\n");
print_string (tui.progress (2, 5, 24) ^ "\n");
print_string (tui.menu (items, selected, 24));
```

- `box (title, body, width)` : cadre UTF-8, lignes paddées ;
- `menu (items, selected, width)` : marqueur `>`, inversion ANSI sur la ligne
  sélectionnée ;
- `progress (current, total, width)` : barre `█/░` et pourcentage ;
- `spinner (frame)` : frames Braille ;
- `status_bar (text, width)` ;
- `render (screen)` : efface l'écran, imprime le rendu ;
- `menu_step (key, selected, count)` : `j`/`down`, `k`/`up`, borné.

Ce n'est pas encore une boucle interactive : il manque `io.read_line` et le
mode raw (`termios`). On peut déjà rendre un écran, un menu, une barre de
progression et un spinner, et lire une ligne avec `io.read_stdin` quand
l'entrée est un pipe/fichier.

Important : `string_of_char` est **octet** (0–255). Pour les caractères UTF-8
de cadre (`─`, `│`, `┌`, …), utiliser des littéraux de chaîne, pas
`string_of_char 9472`. `tui` le fait déjà.

### 8.5 os.args sans primitive dédiée

`os.args ()` lit `/proc/self/cmdline` et découpe sur NUL. `io.read_file` a été
modifié pour lire en chunks : les fichiers non seekables (`/proc`, pipes)
retournent leur contenu même si `ftell` rend 0.

```yack
fun main () =
    print_int (os.argc ());
    print_string (string.join (" ", os.command_args ()))
```

`os.env` / `os.getenv` lisent `/proc/self/environ`. C'est Linux-only pour
l'instant ; macOS/BSD demanderont des primitives ou un autre chemin.

### 8.6 Démo

[`examples/cli`](../examples/cli/) contient quatre programmes complets,
décrits pas à pas dans [`site/cli.html`](../site/cli.html) :

```sh
cd examples/cli
../../yack build src/ls.yack ls
../../yack build src/wc.yack wc
../../yack build src/cat.yack cat
../../yack build src/main.yack cli

./ls -al README.md src/ls.yack
./wc src/*.yack
./cat README.md
./cli -v -o out.txt a.yack b.yack
./cli --nope        # code 2
```

`ls` combine `-a` et `-l` en cluster, trie les noms, lit les tailles par
`io.read_file` et sort en 1 sur un chemin illisible. Sans `readdir`/`stat`
(§11), il liste les chemins nommés et ne voit ni mode, ni propriétaire, ni
date. `make test-cli` construit et vérifie les quatre programmes.

## 9. JSON, dynamic et génération de décodeurs

### 8.1 Ce que fait Gleam

Gleam sépare deux paquets :

- `gleam/json` : type opaque `Json`, builders (`object`, `array`, `string`,
  `int`, `float`, `bool`, `null`), `to_string`, et
  `parse(from:, using: decoder)` ;
- `gleam/dynamic` + `gleam/dynamic/decode` : type `Dynamic`, type opaque
  `Decoder(t)`, et les combinateurs `field`, `optional_field`, `list`,
  `optional`, `at`, `one_of`, `recursive`, `map`, `then`, `success`, `run`.

Un décodeur Gleam est une **fonction** `Dynamic -> #(t, List(Error))`. Le
langage server génère automatiquement un décodeur pour un type personnalisé
(code action « generate dynamic decoder »). C'est l'équivalent Gleam des tags
de struct de Go.

Exemple Gleam :

```gleam
let cat_decoder = {
  use name <- decode.field("name", decode.string)
  use lives <- decode.field("lives", decode.int)
  use nicknames <- decode.field("nicknames", decode.list(decode.string))
  decode.success(Cat(name:, lives:, nicknames:))
}
json.parse(from: json_string, using: cat_decoder)
```

### 8.2 Adaptation Yack

Yack n'a ni `use`, ni closure capturante, ni `Decoder(t)` fonction. Le draft reprend la
**séparation en deux couches** et remplace les fonctions par des valeurs.

#### `yack/json` : arbre et syntaxe

```yack
datatype value =
    Null
  | Bool of bool
  | Number of string
  | String of string
  | Array of value list
  | Object of (string * value) list

parse : borrow string -> value result
to_string : value -> string
object : (string * value) list -> value
array : value list -> value
string : string -> value
int : int -> value
bool : bool -> value
null : unit -> value
```

`Number` conserve le **lexème** (`"42"`, `"-1.5e3"`) au lieu d'un `float` :
le noyau n'a pas `float_to_string`. `as_int` utilise `string_to_int`. Pour les
floats, il faudra `string_to_float` et `float_to_string`.

Aides typées :

```yack
as_null, as_bool, as_int, as_string, as_array, as_object
field, required, find_field
field_int, field_string, field_bool, field_array, field_object
```

Combinateurs d'ordre supérieur :

```yack
array_map, array_filter_map, array_foldl, array_decode
object_map, object_map_values, object_decode_values
field_decode
```

Ces fonctions prennent une fonction par nom ou `x => ...`, sans capture. Le
callback **consomme** la valeur JSON qu'il reçoit : utiliser `as_int`,
`as_string`, etc., ou une fonction qui les appelle. C'est le contrat qui évite
de dupliquer un arbre Affine.

Attention : `Object` est une liste, et une liste est Affine. `field_*` consomme
la liste. Pour décoder plusieurs champs, il faut :

- soit générer un décodeur qui parcourt l'objet **une seule fois** ;
- soit utiliser un curseur `reader` qui rend les champs restants :

```yack
type reader = { fields : (string * value) list }
reader : (string * value) list -> reader
take : borrow string * reader -> (value * reader) result
take_int, take_string, take_bool, take_array, take_object : ...
```

Le curseur est ownership-correct : chaque `take` rend un nouveau reader. Il
demande de rebinder à chaque champ, ce qui est verbeux mais explicite.

#### `yack/dynamic` : décodeurs comme valeurs

Sans fermeture capturante, `Decoder(t)` devient un **datatype fermé** :

```yack
datatype dynamic =
    DNull
  | DBool of bool
  | DInt of int
  | DString of string
  | DList of dynamic list
  | DObject of (string * dynamic) list

datatype decoder =
    DAny
  | DNull
  | DBool
  | DInt
  | DString
  | DList of decoder
  | DOptional of decoder
  | DField of string * decoder
  | DOptionalField of string * decoder
  | DOneOf of decoder list

run : decoder * value -> dynamic result
```

`run` est le `decode.run` de Gleam. Il décode vers `dynamic`, pas vers un type
utilisateur : le passage `dynamic -> T` reste une fonction écrite à la main ou
générée.

Ce qui manque par rapport à Gleam, et pourquoi :

| Gleam | Yack v0 | Raison |
|---|---|---|
| `Decoder(t)` fonction | `decoder` datatype | closures capturantes hors modèle |
| `map` | `array_map` / `object_map_values` | callback direct, sans capture |
| `then` | `and_then` | enchaînement direct, sans capture |
| `recursive` | absent | demande `() -> Decoder(t)` ; possible avec un registre de décodeurs nommés |
| `success` | absent | demande une valeur finale typée |
| `field` avec continuation | `DField` + `run` | la continuation est une fonction |
| code action LSP | `yack json derive` | génération de code |

#### Génération `yack json derive`

C'est la pièce qui donne « depuis et vers des types » sans réflexion.

Entrée : une déclaration `type` ou `datatype`.
Sortie : deux fonctions générées, `t_to_json` et `t_from_json` (ou
`t_decoder`), dans un module `.yack` compilé normalement.

Exemple cible :

```yack
type user = { name : string, age : int }

(* généré *)
fun user_to_json (value : user) : json.value =
    case value of
        { name = name, age = age } =>
            json.object
                (Cons ((json.entry ("name", json.string (name)),
                        Cons ((json.entry ("age", json.int (age)), Nil)))))

fun user_from_json (value : json.value) : user result =
    case json.as_object (value) of
        Error (message) => Error (message)
      | Ok (fields) =>
            let val nameKey = "name"
                val ageKey = "age"
                val name = json.field_string (borrow nameKey, fields)
                (* pour plusieurs champs : reader ou génération d'un seul passage *)
            in ...
            end
```

Le générateur doit produire un **parcours unique** de l'objet, pas des
`field_*` successifs qui consomment la liste. Il connaît tous les champs, donc
il peut faire un seul `case` sur la liste et accumuler.

Tags / noms custom :

- par défaut, le nom Yack du champ est le nom JSON ;
- attribut proposé : `@json("nom")`, `@json(skip)`, `@json(default = …)` ;
- à défaut d'attributs dans la syntaxe, un fichier sidecar
  `derive.toml` peut décrire les renommages ;
- les champs `option` deviennent `optional_field` ;
- les datatypes deviennent un `one_of` sur un champ discriminant (`"type"`,
  `"kind"`, …).

Custom par type :

- si un champ a un type `T` pour lequel l'utilisateur a écrit
  `t_to_json`/`t_from_json`, le générateur appelle ces fonctions ;
- si le type est opaque ou vient d'un paquet, une annotation
  `@json(with = module.fn)` indique la fonction à appeler ;
- pas besoin de fonction de première classe : le nom de la fonction est résolu
  statiquement par le générateur.

C'est exactement le rôle de la code action Gleam « generate dynamic decoder »,
avec un modèle d'extension par noms de fonctions au lieu de valeurs de fonction.

#### Custom sans générateur

Pour un type ponctuel, l'utilisateur écrit à la main :

```yack
fun point_to_json (value : point) : json.value =
    json.object
        (Cons ((json.entry ("x", json.int (value.x)),
                Cons ((json.entry ("y", json.int (value.y)), Nil)))))

fun point_from_json (value : json.value) : point result =
    case json.as_object (value) of
        Error (message) => Error (message)
      | Ok (fields) =>
            let val xKey = "x"
                val yKey = "y"
            in ...
            end
```

C'est le `MarshalJSON`/`UnmarshalJSON` de Go, en plus explicite.

#### Évolution quand les closures arriveront

Quand `fn`/closures seront supportés :

```yack
type codec 'a = { encode : 'a -> json.value,
                  decode : json.value -> 'a result }

fun json.field (name : string, codec : codec 'a) : codec 'a = ...
```

`map`, `then`, `one_of` et `recursive` deviennent alors des fonctions
génériques comme à Gleam, et `Decoder(t)` peut remplacer le datatype fermé.
Le générateur reste utile pour éviter d'écrire les codecs à la main.

### 8.3 JSON pour le LSP

Le LSP est le premier consommateur réel. Il impose :

- objets et tableaux imbriqués ;
- chaînes avec `\"`, `\\`, `\/`, `\n`, `\r`, `\t`, `\b`, `\f` ;
- échappements `\uXXXX` (le draft gère le BMP, pas les paires de surrogates) ;
- entiers (`id`, `line`, `character`, `Content-Length`) ;
- `null` ;
- union `id : int | string` (JSON-RPC) ;
- messages volumineux, donc un parseur linéaire et sans allocation inutile ;
- erreurs localisées (position) pour diagnostiquer un message malformé.

Le draft `yack/json` couvre ces points sauf les paires de surrogates et les
floats. `Number` en texte permet de garder `Content-Length` et les entiers LSP
sans dépendre de `float_to_string`.

## 10. Paquet LSP

> **Note (0.2).** Le compilateur embarque désormais un LSP natif en SML
> (`yack lsp`, voir `docs/lsp.md`). Cette section décrit un futur LSP
> auto-hébergé écrit en Yack, avec `yack/lsp/rpc` et `yack/jsonderive`. Les
> deux peuvent coexister : le serveur natif reste la référence pour les
> diagnostics et les types.

```text
yack/lsp/rpc     lecture/écriture des trames JSON-RPC
yack/lsp/types   Position, Range, Diagnostic, TextEdit, messages
yack/lsp/server  boucle principale et dispatch
yack/lsp/capabilities
```

### 9.1 Framing JSON-RPC

```text
Content-Length: 123\r\n
\r\n
{...}
```

Le module `yack/lsp/rpc` doit :

- lire les en-têtes ligne par ligne ;
- parser `Content-Length` (insensible à la casse) ;
- lire exactement N octets ;
- écrire `Content-Length` + corps sur stdout ;
- ignorer les en-têtes inconnus ;
- gérer `\r\n` et `\n`.

Primitives manquantes : `io.read_line`, `io.read_exact`, `io.write_stdout`,
`io.flush`. `io.read_stdin` actuel lit tout stdin, ce qui ne suffit pas pour un
serveur interactif.

### 9.2 Types et décodeurs

Les messages LSP sont des records avec beaucoup de champs. C'est le cas
d'usage idéal de `yack json derive` :

```yack
type position = { line : int, character : int }
type range = { start : position, end : position }

(* généré *)
fun position_from_json (value : json.value) : position result
fun range_from_json (value : json.value) : range result
```

Pour les unions (`id : int | string`, `params` polymorphe), le générateur émet
un `one_of` ou un `dynamic` laissé à l'utilisateur.

### 9.3 Boucle serveur

Le serveur LSP reste mono-thread : une boucle récursive terminale lit une
trame, dispatche, écrit la réponse, recommence. `sys.thread_spawn` existe pour
les services qui veulent plusieurs threads explicites. Les notifications (`textDocument/didOpen`, …) ne
produisent pas de réponse. Les requêtes produisent une réponse avec le même
`id`.

Les handlers sont des fonctions de module, pas des valeurs : le dispatch est un
`case` sur `method` qui appelle la fonction correspondante.

## 11. Primitives compilateur manquantes

Classées par impact stdlib.

### Bloquantes pour le LSP

1. `io.read_line : unit -> string result`
2. `io.read_exact : int -> string result`
3. `io.write_stderr : borrow string -> unit`
4. `io.flush : unit -> unit`
5. `string_to_float` / `float_to_string` (JSON nombres)
6. `io.is_tty : unit -> bool`, taille du terminal (`ioctl TIOCGWINSZ`)
7. mode raw / `termios`, `io.read_key`, `io.read_line` non bloquant
8. décodage complet des paires de surrogates `\uD800\uDC00`
9. `string_of_codepoint`/UTF-8 (car `string_of_char` est un octet)

### Bloquantes pour les collections

7. `array_swap : 'a array * int * int -> unit`
8. variantes `borrow` de `array_get`/`array_length`/`array_set`/`map_get`/…
9. `map_values`, `map_entries` (itération)
10. map générique (hash/eq de clé)

### Bloquantes pour les types et l'ergonomie

12. ✅ durées de vie non lexicales (emprunt nommé qui meurt au dernier usage)
13. `clone`/`copy` explicite pour les types Affine
14. `pub`/privé pour cacher `internal_*`
15. alias de types (`type byte = int8`)
16. `string_builder` pour la concaténation amortie
17. fermetures capturantes (`E0511`)
18. interfaces / type classes
19. `val` déstructurant les tuples
20. attributs (`@json`, `@derive`) ou macro pour `yack json derive`

### Bloquantes pour crypto/compress

21. opérateurs bitwise `&`, `|`, `^`, `<<`, `>>`, `~`
22. RNG de qualité cryptographique
23. constant-time compare

### Bloquantes pour os/time

24. `chdir`, `stat`, `mkdir`, `remove`, `rename`, `readdir`
25. `argv`/`env` natifs (le draft passe par `/proc` sur Linux)
26. `open`/`close`/`read`/`write`/`seek` sur handles
27. `clock`, `sleep`, `format_time`

## 12. Conventions

- Module : minuscules, nom singulier (`yack/string`, `yack/list`).
- Fichier : `src/<module>.yack`, sous-dossiers pour les familles
  (`encoding/hex`).
- Fonction : `snake_case`.
- Prédicat : `is_*`.
- Conversion : `to_*`, `from_*`, `as_*`.
- Constructeur : `make_*` ou nom court (`json.object`).
- Variante sans repli : suffixe `_unchecked`. Les bornes sont toujours
  vérifiées : un index hors bornes arrête le programme
  (`index N out of bounds (length L)`), avec ou sans suffixe. Pour les
  arrays, seul `array_push_unchecked` diffère encore : il ne fait pas
  grandir le tableau et s'arrête s'il est plein.
- Variante avec offset : suffixe `_from`.
- Variante avec défaut : suffixe `_or`.
- Helper interne : préfixe `internal_`.
- Type : minuscules ; constructeur : UpperCamel.
- Erreur : `string result` aujourd'hui ; `error` structuré plus tard.
- Lecture : `borrow` ; retour possédé ; collection Affine threadée.
- Aucun état global, aucun buffer caché, aucune allocation non documentée.
- Toute fonction qui consomme une valeur le dit dans son nom ou sa doc.

## 13. Migration depuis la prelude

Phase 0 (draft actuel) :

- `lib/yack` existe et compile ;
- la prelude reste inchangée ;
- `examples/stdlib` montre l'usage ;
- les helpers de la prelude restent pour compatibilité.

Phase 1 :

- déprécier `list_length`, `list_tail`, `list_head`, `option_is_some`,
  `result_is_ok` au profit de `yack/list`, `yack/option`, `yack/result` ;
- mettre à jour les exemples et tests ;
- ajouter un avertissement de dépréciation dans `yack check`.

Phase 2 :

- `yack` devient un alias réservé résolu vers `lib/yack` sans dépendance
  déclarée ;
- la prelude ne garde que les types du noyau et les builtins ;
- `yack/core` peut être auto-importé si l'on veut sortir aussi les datatypes.

## 14. Tests

- `test/stdlib/` : un paquet par module, `main` qui imprime un résultat
  stable ;
- `yack check` sur chaque module et sur `examples/stdlib` ;
- tests de round-trip JSON : `parse (to_string value) = value` (quand `value`
  est comparable) ;
- tests d'échappements : `"`, `\`, `\n`, `\r`, `\t`, `\b`, `\f`, `\u00E9` ;
- tests d'erreurs : objet non terminé, virgule finale, champ manquant, type
  incorrect, `Content-Length` invalide ;
- ASan (`make test-asan`) et stack mode (`make test-stack`) sur les modules
  collection ;
- `examples/cli` : `--help`, option courte/longue, positionnels, `--`, code de
  sortie 2 sur erreur ;
- un test LSP de framing avec un flux `Content-Length` synthétique.

Pas de framework de test pour l'instant : `yack/test` fournira des assertions
et un `main` qui renvoie un code de sortie, quand `os.exit` existera.

## 15. Roadmap

| Phase | Contenu | Dépendances |
|---|---|---|
| 0 | `bool`, `int`, `char`, `string`, `list`, `option`, `result`, `math`, `float`, `fmt`, `path` | aucune |
| 1 | `array`, `map`, `bytes`, `sort`, `io`, `json` | threading, primitives IO |
| 2 | `dynamic`, `json derive`, `lsp/rpc` | génération de code, `io.read_line` |
| 3 | `os`, `time`, `random`, `log`, `test`, `flags` | syscalls |
| 3bis | `ansi`, `args`, `cli`, `term`, `tui` | déjà en place ; TUI interactif = `io.read_line`/termios |
| 4 | `encoding/hex`, `base64`, `csv`, `toml`, `utf8` | pur |
| 5 | `net`, `http`, `server`, `url` | syscalls réseau (déjà dans `lib/net`) |
| 6 | `crypto`, `compress`, `archive`, `image` | bitwise, RNG, IO |
| 7 | `regexp`, `template`, `db` | closures/interfaces |

## 16. Décisions ouvertes

1. **Datatypes dans la prelude ou dans `yack/core` ?** Recommandation : noyau.
2. **`yack` implicite ou dépendance déclarée ?** Recommandation : alias réservé.
3. **`decoder` datatype fermé ou génération systématique ?** Recommandation :
   les deux ; datatype pour les primitives, génération pour les records.
4. **Tags JSON dans la syntaxe ou sidecar ?** Recommandation : attributs
   `@json(...)` quand le parser les supportera ; sidecar en attendant.
5. **Erreurs JSON : `string` ou type structuré ?** Recommandation : type
   `json.error` avec position, kind et chemin, dès que les datatypes suffisent.
6. **NLL ou `clone` explicite ?** Tranché : NLL local est livré (emprunt
   nommé jusqu'au dernier usage). `clone`/`copy` explicite reste utile pour les
   cas conservateurs et les agrégats copiés volontairement.

## 17. Références

- Gleam `gleam/json` : builders, `parse(from:, using:)`, `to_string`.
- Gleam `gleam/dynamic/decode` : `Dynamic`, `Decoder(t)`, `field`,
  `optional_field`, `list`, `optional`, `at`, `one_of`, `recursive`, `run`,
  génération de décodeurs par le language server.
- Go `encoding/json` : `Marshal`/`Unmarshal`, tags de struct, `MarshalJSON`
  custom.
