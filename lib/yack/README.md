# yack : bibliothèque standard

Paquet de la stdlib Yack, **hors prelude**. Les modules s'importent sous le
namespace `yack` :

```yack
import yack/string
import yack/int
import yack/json
import yack/args
import yack/tui

fun main () =
    let val text = "  yack  "
        val trimmed = string.trim (borrow text)
    in print_string (trimmed)
    end
```

Le paquet est compilable avec le compilateur actuel. Les modules marqués ⏳
dans [`docs/stdlib.md`](../../docs/stdlib.md) attendent des primitives
compilateur (UTF-8, raw mode, threads, bitwise, etc.).

## Modules

| Domaine | Modules |
|---|---|
| valeurs | `bool`, `int`, `char`, `float`, `option`, `result`, `list`, `string` |
| calcul | `math`, `fmt`, `path`, `sort` |
| collections | `array`, `map`, `bytes` |
| entrées/sorties | `io`, `os` |
| données | `json` |
| CLI | `ansi`, `args`, `cli`, `term`, `tui` |

## Style d'API

- Les lectures de chaînes prennent `borrow string` ; les constructions rendent
  une chaîne possédée.
- Les collections Affine se **threadent** : une mutation rend la collection.
- Les accesseurs de `args` rendent `(valeur, parsed restant)` parce qu'une
  liste est Affine.
- `print_string` ajoute un saut de ligne ; utiliser `io.print_raw` pour un
  rendu TUI/ANSI ou du JSON compact.
- Les erreurs sont des `string result` ; pas d'exception.
- Fonctions passées par nom ou `x => ...`, sans capture. Les combinateurs
  `list.map`, `list.filter`, `list.foldl`, `list.foldr`, `list.filter_map`,
  `option.map`, `result.map`, `json.array_map`, `json.array_filter_map`,
  `json.array_decode` et `json.object_map_values` en profitent ; pas de type
  classes.

## Exemples

- [`examples/stdlib`](../../examples/stdlib/) : import et usage des modules de
  base + JSON ;
- [`examples/cli`](../../examples/cli/) : CLI complète, avec args, aide, couleurs,
  code de sortie, mini-TUI.

## Dépendance

En attendant l'alias `yack` réservé par le compilateur :

```toml
[dependencies]
yack = { path = "../../lib/yack" }
```

## Tests

```sh
./yack check ../examples/stdlib/src/main.yack
./yack check ../examples/cli/src/main.yack
```
