<div align="center">

# Le tour de Yack

**Apprendre le langage en 20 minutes**

*Un langage fonctionnel inspiré de SML, compilé nativement, avec un ownership
vérifié à la compilation.*

</div>

---

Yack est un petit langage fonctionnel inspiré de SML, compilé nativement avec
LLVM. Il n'a pas de ramasse-miettes : l'ownership, les déplacements et les
emprunts sont vérifiés à la compilation, et les libérations sont déterministes.

Chaque bloc de code de ce tutoriel est un programme complet. Copiez-le dans un
fichier `.yack` et lancez-le.

> **Prérequis :** savoir écrire une fonction et un `if`. Aucune connaissance de
> SML, de Rust ou de LLVM n'est nécessaire.

## Sommaire

1. [Installer et lancer](#1-installer-et-lancer)
2. [Hello, Yack](#2-hello-yack)
3. [Expressions et `let`](#3-expressions-et-let)
4. [Fonctions](#4-fonctions)
5. [Types de base](#5-types-de-base)
6. [`if` et `case`](#6-if-et-case)
7. [Datatypes et listes](#7-datatypes-et-listes)
8. [Records](#8-records)
9. [Modules](#9-modules)
10. [Mémoire : Copy, Affine, Linear](#10-mémoire--copy-affine-linear)
11. [Emprunts](#11-emprunts)
12. [Fonctions comme valeurs](#12-fonctions-comme-valeurs)
13. [Bibliothèque standard](#13-bibliothèque-standard)
14. [Et maintenant ?](#14-et-maintenant-)

---

## 1. Installer et lancer

Dans le dépôt Yack :

```sh
make
./yack new hello
cd hello
../yack run
```

Sortie :

```text
Hello, Yack !
```

Les commandes utiles pendant l'apprentissage :

| Commande | Rôle |
|---|---|
| `yack run [fichier.yack]` | compile et exécute ; sans fichier, prend l'entrée du paquet |
| `yack check fichier.yack` | vérifie sans exécuter |
| `yack build fichier.yack sortie` | produit un exécutable natif |
| `yack fmt -w fichier.yack` | formate le source |
| `yack types fichier.yack` | montre les propriétés mémoire |
| `yack arena fichier.yack` | explique ce que chaque bloc `arena` libère |
| `yack alloc-map fichier.yack` | liste les sites d'allocation |

Un programme doit contenir exactement une fonction `main`, sans paramètre et
de type `unit`.

---

## 2. Hello, Yack

```yack
fun main () =
    print_string ("Hello, Yack !")
```

`fun` déclare une fonction de niveau module. `print_string` est un builtin qui
prend une `string` possédée et renvoie `unit`.

Les commentaires utilisent `(* ... *)` et s'imbriquent :

```yack
(* Un commentaire
   (* imbriqué *)
   sur plusieurs lignes. *)
fun main () = print_string ("ok")
```

---

## 3. Expressions et `let`

Tout est expression. Un bloc local s'écrit `let val ... in ... end` :

```yack
fun main () =
    let val name = "Ada"
        val age = 36
        val next = age + 1
    in
        print_string (name ^ " aura " ^ int_to_string (next) ^ " ans")
    end
```

Sortie :

```text
Ada aura 37 ans
```

Quelques règles :

- pas de point-virgule : les `val` se suivent ligne par ligne ;
- une liaison `val` est immuable ;
- `^` concatène deux `string` ;
- `int_to_string` convertit un entier en chaîne ;
- `val _ = expression` évalue et jette un résultat, utile pour `unit`.

Une annotation de type est une contrainte vérifiée, pas un cast :

```yack
fun main () =
    let val count : int = 40 + 2
    in print_int (count)
    end
```

Un `val` peut aussi déstructurer un tuple ou un record :

```yack
fun main () =
    let val (a, b) = (20, 22)
        val { x = px, y = py } = { x = a, y = b }
    in print_int (px + py)
    end
```

Le motif doit être exhaustif. Pour un constructeur comme `Some x`, utilisez
`case` à la place.

### Garder une condition ou un motif : `guard`

`guard` coupe la suite du bloc si la condition est fausse, ou si le motif ne
correspond pas. Le fallback n'est évalué qu'en cas d'échec.

```yack
fun parse (value : int result) : int =
    let guard Ok inner <- value else 0
    in inner end

fun positive (value : int) : int =
    let guard value > 0 else 0
    in value end
```

Deux formes :

```yack
guard condition else fallback
guard pattern <- value else fallback
```

`guard` est du sucre : le compilateur le transforme en `if` ou en `case` avant
le typage. Il ne crée ni closure ni retour anticipé général.

---

## 4. Fonctions

```yack
fun add (x, y) =
    x + y

fun main () =
    print_int (add (20, 22))
```

Une fonction peut avoir plusieurs équations, avec des motifs en paramètres :

```yack
fun fact (0) : int = 1
fun fact (n) : int = n * fact (n - 1)

fun first (Nil) : int = 0
fun first (value :: _) : int = value

fun main () =
    print_int (fact (5) + first ([7]))
```

Deux points de syntaxe importants :

- l'appel exige des parenthèses : `f (x)`, pas `f x` ;
- la virgule sépare les arguments : `f (a, b)` est un appel à deux arguments,
  tandis que `f ((a, b))` passe un seul argument tuple.

Les fonctions de niveau module sont toutes mutuellement visibles : la
récursion directe et mutuelle ne demande aucune déclaration spéciale.

### Le pipe

`|>` insère la valeur de gauche en **premier argument** de l'appel de droite :

```yack
fun add (a, b) = a + b
fun double (x) = x * 2

fun main () =
    let val result = 5 |> add (3) |> double
    in print_int (result)
    end
```

Sortie : `16`. La chaîne se lit `double (add (5, 3))`.

---

## 5. Types de base

| Type | Signification |
|---|---|
| `int` | entier signé 32 bits, alias de `int32` |
| `int1` … `int64` | entiers de largeur explicite |
| `float`, `float32`, `float64` | flottants |
| `bool` | `true` / `false` |
| `unit` | valeur `()` |
| `string` | chaîne possédée |
| `token` | valeur Linear |
| `resource` | valeur Affine |

```yack
fun volume (width : int, height : int, depth : int) : int =
    width * height * depth

fun main () =
    let val big : int64 = 40
        val small : int = 2
    in print_int64 (big + int_to_int64 (small))
    end
```

`int` est un `int32`. Pour de grands entiers, utilisez `int64` et les
conversions explicites (`int_to_int64`, `int64_to_int`). `int64_to_int`
arrête le programme si la valeur ne tient pas sur 32 bits ;
`int64_to_int_wrapping` tronque.

Les types génériques suivent la convention SML :

```yack
fun identity (value : 'a) : 'a =
    value

fun same (left : ''a, right : ''a) : bool =
    left = right
```

`'a` est une variable de type ordinaire ; `''a` est une variable de type
d'égalité, utilisable avec `=` et `<>`.

---

## 6. `if` et `case`

`if` est une expression, et les deux branches sont obligatoires :

```yack
fun absolute (n : int) : int =
    if n < 0 then 0 - n else n
```

`case` déstructure et filtre. Les gardes permettent de tester les variables
liées par le motif :

```yack
fun classify (n : int) : string =
    case n of
        0 => "zero"
      | x if x < 0 => "negative"
      | _ => "positive"

fun main () =
    let val n = 0 - 3
    in print_string (classify (n))
    end
```

Sortie : `negative`.

L'exhaustivité est vérifiée. Si vous oubliez un constructeur ou le cas `false`
d'un `bool`, la compilation échoue avec `E3021`. Une garde ne compte pas pour
l'exhaustivité : gardez une branche `_` si nécessaire.

Les motifs peuvent être imbriqués :

```yack
fun describe (value : int option) : string =
    case value of
        None => "rien"
      | Some 0 => "zero"
      | Some _ => "quelque chose"
```

---

## 7. Datatypes et listes

Un datatype déclare un ensemble de constructeurs. Les constructeurs commencent
par une majuscule, les variables par une minuscule :

```yack
datatype shape = Circle of int | Rectangle of int * int

fun area (shape : shape) : int =
    case shape of
        Circle radius => radius * radius * 3
      | Rectangle (width, height) => width * height

fun main () =
    print_int (area (Rectangle ((4, 5))))
```

Sortie : `20`.

Les datatypes peuvent être génériques. L'application de type est postfixe,
comme en SML :

```yack
datatype 'a option2 = None2 | Some2 of 'a

fun unwrap_or (fallback : 'a, value : 'a option2) : 'a =
    case value of
        None2 => fallback
      | Some2 x => x

fun main () =
    print_int (unwrap_or (0, Some2 (42)))
```

### Listes

Le type `'a list` est un datatype standard avec `Nil` et `Cons`. Une liste
s'écrit entre crochets, avec des virgules, et `[]` désigne la liste vide :

```yack
fun sum (values : int list) : int =
    case values of
        [] => 0
      | value :: rest => value + sum (rest)

fun main () =
    print_int (sum ([1, 2, 3]))
```

`[1, 2]` se désucre en `Cons ((1, Cons ((2, Nil))))`. Dans un motif, `::`
reste le sucre pour `Cons` et sert à décomposer une liste. On peut enchaîner
plusieurs têtes :

```yack
fun first_three (values : int list) : int =
    case values of
        first :: second :: third :: _ => first + second + third
      | _ => 0
```

### Chaînes et bytes

Un littéral de chaîne se compare exactement, ou sert de préfixe avec `^`. La
partie droite du `^` lie le suffixe :

```yack
fun route (path : string) : string =
    case path of
        "/health" => "health"
      | "/api/" ^ rest => "api " ^ rest
      | "" => "empty"
      | _ => "other"
```

Un pattern de bytes s'écrit entre `<<` et `>>` et s'applique à `int8 array`.
Les segments peuvent être des chaînes ou des octets numériques, et le dernier
motif lie le reste :

```yack
fun method (request : int8 array) : string =
    case request of
        <<"GET ", rest>> =>
            "GET " ^ int_to_string (array_length (rest))
      | <<"POST ", rest>> => "POST"
      | <<>> => "empty"
      | _ => "other"

fun main () =
    let val text = "GET /hello"
    in print_string (method (bytes_from_string (borrow text)))
    end
```

Sortie : `GET 6`. Le reste lié est un nouvel `int8 array` indépendant, car le
scrutinee est consommé par le match.

Yack n'a pas de boucles : la récursion, idéalement terminale, est la forme
d'itération prévue.

---

## 8. Records

Un record se déclare avec `type`, se construit avec `{ champ = valeur }` et
s'accède avec `.champ` :

```yack
type point : Copy = { x : int, y : int }

fun shift_x (p : point, dx : int) : point =
    { x = p.x + dx, y = p.y }

fun main () =
    let val p = { x = 1, y = 2 }
        val q = shift_x (p, 10)
    in print_int (p.x + q.x)
    end
```

Sortie : `12`. `p` reste utilisable après l'appel car `point` est déclaré
`Copy`.

Sans annotation, un type utilisateur est **Affine** : la valeur est possédée et
déplacée, pas copiée implicitement.

Un record peut être déstructuré dans un `case` :

```yack
fun get_x (p : point) : int =
    case p of
        { x = x } => x
```

Un record déclaré avec `type` ne peut pas être récursif. Pour une structure
récursive, utilisez un `datatype` :

```yack
datatype user = User of { name : string, age : int, friends : user list }
```

---

## 9. Modules

Un fichier peut importer un autre module. Le qualificatif est le dernier
segment du chemin, ou l'alias donné avec `as` :

```yack
(* src/Math.yack *)
fun double (x : int) : int = x * 2

(* src/main.yack *)
import Math

fun main () = print_int (Math.double (21))
```

Un import sélectif lie aussi les noms directement :

```yack
import Math.{double as twice}

fun main () = print_int (twice (21))
```

Un package se décrit avec un `yack.toml` :

```toml
[package]
name = "app"
version = "0.1.0"

[dependencies]
util = { path = "../lib" }

[memory]
mode = "heap"   # or "stack"
```

Arborescence typique :

```text
app/
  yack.toml
  src/
    Math.yack
    main.yack
```

Les imports sont transitifs, les cycles sont rejetés, et toutes les
déclarations de niveau module sont exportées. Il n'y a pas encore de
`pub`/privé.

La bibliothèque standard s'importe avec `yack/...` :

```yack
import yack/list
import yack/string
```

---

## 10. Mémoire : Copy, Affine, Linear

C'est la partie qui distingue Yack. Chaque type a une propriété mémoire :

| Propriété | Comportement |
|---|---|
| `Copy` | copie implicite et libération libre |
| `Affine` | déplacé à l'usage, libéré une fois |
| `Linear` | doit être consommé exactement une fois sur chaque chemin |

Valeurs typiques :

- `int`, `bool`, `unit` : `Copy` ;
- `string`, `array`, `map`, types utilisateur par défaut : `Affine` ;
- `token` : `Linear` ;
- `resource` : `Affine`.

Passer une valeur Affine à une fonction la **déplace**. L'ancienne liaison
devient inutilisable :

```yack
type box : Affine = { value : int }

fun unbox (box : box) : int = box.value

fun main () =
    let val b : box = { value = 42 }
        val n = unbox (b)
    in print_int (n)
    end
```

Utiliser `b` après `unbox (b)` produit `E0402` (utilisation après transfert).

Une valeur `Linear` doit être consommée exactement une fois, y compris sur
chaque branche :

```yack
fun use (flag : bool) : unit =
    let val token = new_token ()
    in
        if flag then consume_token (token) else consume_token (token)
    end
```

Si une branche oublie `consume_token`, le compilateur renvoie `E0403`.

Yack n'a pas de GC. Le backend place en pile ce qui ne s'échappe pas, alloue sur
le tas ce qui s'échappe, puis insère les libérations après le dernier
propriétaire. `yack types` montre la propriété déclarée et la propriété
effective.

### Mémoire temporaire : `arena`

Quand beaucoup de valeurs temporaires sont créées puis jetées ensemble, un
bloc `arena` évite de les libérer une par une :

```yack
fun normalize (text : string) : int =
    arena
        val trimmed = string_slice (borrow text, 0, 5)
        val upper = string_copy (borrow trimmed)
        val message = "value: " ^ upper
    in
        string_length (borrow message)
    end
```

À `end`, les chaînes allouées dans le bloc sont libérées d'un coup. En mode
stack, c'est une arène à bump pointer ; en mode heap, Yack regroupe les
allocations dans de gros blocs et les recycle.

Les tableaux, maps et données qui dépassent le bloc ne sont pas libérés par
`arena` : ils vivent jusqu'à la fin du programme ou au retour de la fonction.
Le compilateur refuse qu'une valeur temporaire non prouvée s'échappe.

Pour voir ce que fait un bloc :

```sh
../yack arena main.yack
```

La commande liste les chaînes libérées à `end`, celles copiées vers la racine
parce qu'elles sont rangées dans un conteneur, et ce qui n'est pas de la
mémoire d'arène (tableaux, maps, données). Le LSP affiche le résumé en inlay
hint après `arena` et le détail au survol.

---

## 11. Emprunts

Au lieu de déplacer une valeur, on peut l'emprunter :

```yack
fun length_of (text : borrow string) : int =
    string_length (text)

fun main () =
    let val text = "Yack"
        val n = length_of (borrow text)
    in print_int (n)
    end
```

`borrow` crée une vue temporaire sur une **place** : une variable locale, ou
une projection de champ/index enracinée dans une variable locale. `borrow
(f x)` est rejeté (`E0500`).

Les variantes :

```yack
borrow user           (* partagé, Copy *)
borrow mut user       (* exclusif, Affine *)
borrow user.name      (* champ de record *)
borrow pair.0         (* index de tuple *)
```

`&` est l'alias court de `borrow`, dans les expressions comme dans les types :

```yack
fun length_of (text : &string) : int =
    string_length (text)
```

Règles principales :

- plusieurs emprunts partagés sont autorisés ;
- un emprunt mutable est exclusif ;
- une place empruntée ne peut pas être déplacée (`E0501`) ;
- deux emprunts qui se recouvrent entrent en conflit (`E0502`) ;
- un emprunt ne peut pas survivre à son propriétaire local (`E0507`).

Une fonction peut retourner un emprunt rattaché à un paramètre avec `from` :

```yack
fun first (value : borrow string) : borrow string from value =
    value

fun main () =
    let val text = "Yack"
        val alias = first (borrow text)
        val n = string_length (alias)
    in print_int (n)
    end
```

`from` accepte le nom du paramètre (recommandé) ou son index à partir de 1.
Avec plusieurs paramètres d'emprunt, la provenance doit être explicite.

À ce stade, `borrow mut` sert surtout au vérificateur d'alias : aucune
opération du langage ne mute encore à travers un emprunt mutable.

---

## 12. Fonctions comme valeurs

Une fonction de niveau module peut être passée par son nom :

```yack
import yack/list

fun double (x : int) : int = x * 2

fun main () =
    let val values = [1, 2, 3]
        val doubled = list.map (double, values)
        val total = list.sum_int (doubled)
    in print_int (total)
    end
```

Une fonction inline s'écrit `x => expression` :

```yack
import yack/list

fun main () =
    let val values = [1, 2, 3]
        val doubled = list.map (x => x * 2, values)
    in print_int (list.sum_int (doubled))
    end
```

Une valeur de fonction ne capture **aucun** environnement. Ceci est rejeté
avec `E0511` :

```yack
fun add_n (n : int) : int -> int =
    x => x + n    (* E0511 : capture de n *)
```

Il n'y a ni `fn`, ni fermeture, ni application partielle. Un `fun` local existe
dans un `let`, mais il ne peut pas capturer de variable quand il est utilisé
comme valeur. En revanche, les callbacks qui possèdent leur argument peuvent
être annotés `consume T -> R`, par exemple pour les décodeurs JSON.

---

## 13. Bibliothèque standard

La stdlib est dans `lib/yack/src` et s'importe par domaine :

| Domaine | Modules |
|---|---|
| Bases | `bool`, `int`, `char`, `float`, `string`, `bytes` |
| Collections | `list`, `array`, `map`, `sort` |
| Erreurs | `option`, `result` |
| Système | `io`, `os`, `path`, `fs`, `file`, `process`, `net`, `time`, `random` |
| Texte et formats | `fmt`, `math`, `json` |
| Terminal et CLI | `ansi`, `args`, `cli`, `term`, `tui` |

Exemple avec `string` et `list` :

```yack
import yack/io
import yack/list
import yack/string

fun main () =
    let val text = "  yack, gleam, ml  "
        val trimmed = string.trim (borrow text)
        val separator = ", "
        val words = string.split_on (borrow separator, borrow trimmed)
        val count = list.length (words)
        val message = "champs: " ^ int_to_string (count)
    in io.print_line (borrow message)
    end
```

Sortie : `champs: 3`.

Quelques fonctions à retenir :

- `list.map`, `list.filter`, `list.foldl`, `list.foldr`, `list.zip` ;
- `string.trim`, `string.split_on`, `string.join`, `string.to_upper` ;
- `option.unwrap_or`, `option.map`, `option.and_then` ;
- `result.unwrap_or`, `result.map`, `result.and_then` ;
- `io.read_file`, `io.write_file`, `io.print_line`.

`io.read_file` renvoie un `string result`, à traiter avec `case` :

```yack
import yack/io

fun main () =
    case io.read_file ("README.md") of
        Error message => io.print_line (borrow message)
      | Ok contents =>
          let val size = string_length (borrow contents)
              val message = int_to_string (size) ^ " octets"
          in io.print_line (borrow message)
          end
```

Le builtin `sprintf` formate à partir d'un littéral connu à la compilation :

```yack
fun main () =
    let val name = "Ada"
        val count = 3
        val message = sprintf ("%s a %d idees", (name, count))
    in print_string (message)
    end
```

---

## 14. Et maintenant ?

Vous connaissez le noyau. Pour aller plus loin :

1. écrivez un petit outil qui lit un fichier et compte ses lignes avec
   `io.read_file` et `string.lines` ;
2. réimplémentez `list.map` et `list.filter` avec `case` et la récursion ;
3. explorez `lib/yack/src/json.yack` pour voir les callbacks `consume` ;
4. lancez `yack lsp` dans votre éditeur pour les diagnostics et la complétion.

Documentation utile :

- [`../docs/stdlib.md`](../docs/stdlib.md) : API de la bibliothèque standard ;
- [`../examples/`](../examples/) : programmes complets et testés.

Limites actuelles à garder en tête :

- pas de fermetures ni de boucles. Les threads se limitent à
  `sys.thread_spawn` et `sys.thread_join`, sans concurrence de plus haut
  niveau ;
- pas de varargs, d'interfaces ni de type classes ;
- la stdlib reste partielle : réseau TCP sans TLS, dates en UTC seulement,
  pas d'encodage base64 ni hexadécimal ;
- le compilateur et le backend ciblent Linux x86-64 ;
- `--memory=stack` est encore expérimental.

---

## Aide-mémoire

| Besoin | Syntaxe |
|---|---|
| Déclarer une fonction | `fun f (x, y) = ...` |
| Appeler une fonction | `f (x, y)` |
| Bloc local | `let val x = ... in ... end` |
| Condition | `if cond then a else b` |
| Filtrage | `case x of A => ... \| B y => ...` |
| Type record | `type point : Copy = { x : int, y : int }` |
| Construire un record | `{ x = 1, y = 2 }` |
| Accéder à un champ | `point.x` |
| Datatype générique | `datatype 'a t = A \| B of 'a` |
| Liste | `[1, 2, 3]` |
| Décomposer une liste | `first :: second :: tail` |
| Préfixe de chaîne | `"/api/" ^ rest` |
| Préfixe de bytes | `<<"GET ", rest>>` |
| Pipe | `x \|> f (y)` |
| Module | `import yack/list` puis `list.map` |
| Emprunt | `borrow value`, `&value` |
| Fonction inline | `x => x + 1` |
| Commentaire | `(* ... *)` |

### Erreurs fréquentes

| Erreur | Cause | Correction |
|---|---|---|
| `E1003` | appel partiel `f x` | écrire `f (x)` |
| `E0511` | capture dans `x => ...` | passer la valeur en argument |
| `E3021` | `case` non exhaustif | ajouter les constructeurs manquants ou `_` |
| `E0402` | utilisation après transfert | déplacer avant, ou emprunter |
| `E0501` | déplacement d'une valeur empruntée | terminer l'emprunt avant le move |
| `E0502` | emprunts incompatibles | séparer les emprunts ou utiliser `borrow` partagé |

Bon yacking !
