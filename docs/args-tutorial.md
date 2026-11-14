# Mini-tuto : écrire un CLI avec `yack/args`

`yack/args` transforme une `string list` en options typées. Il ne lit jamais le
processus lui-même : on lui passe `os.command_args ()`.

Le point important : une **spec list** est une liste Yack ordinaire, donc elle
s'écrit avec des crochets :

```yack
[
    args.Flag (("verbose", Some ("v"), "mode bavard")),
    args.Opt (("output", Some ("o"), "fichier de sortie", Some ("out.txt"))),
    args.Positional (("file", "fichier d'entrée", true))
]
```

Pas de `Cons`/`Nil` à écrire à la main. `Nil` devient `[]`, et
`Cons (x, rest)` devient `[x, ...]`.

## 1. Programme complet

```yack
import yack/args
import yack/cli
import yack/os
import yack/string

fun specs () : args.spec list =
    [
        args.Flag (("help", Some ("h"), "affiche cette aide")),
        args.Flag (("verbose", Some ("v"), "mode bavard")),
        args.Opt (("output", Some ("o"), "fichier de sortie", Some ("out.txt"))),
        args.Positional (("file", "fichier d'entrée", true))
    ]

fun run (value0 : args.parsed) : unit =
    let val helpPair = args.take_flag (value0, "help")
        val help = helpPair.0
        val value1 = helpPair.1
    in
        if help then
            let val _ = cli.print_help ("demo", "Démo yack/args.", specs ())
            in os.exit_ok ()
            end
        else
            let val verbosePair = args.take_flag (value1, "verbose")
                val verbose = verbosePair.0
                val value2 = verbosePair.1
                val outputPair =
                    args.take_option_or (value2, "output", "out.txt")
                val output = outputPair.0
                val value3 = outputPair.1
                val files = args.positionals (value3)
                val mode = if verbose then "verbose" else "normal"
                val body =
                    "mode: " ^ mode ^ "\n" ^
                    "sortie: " ^ output ^ "\n" ^
                    "fichiers: " ^ string.join (", ", files)
            in print_string (body)
            end
    end

fun main () =
    case args.parse (specs (), os.command_args ()) of
        Error (message) =>
            let val _ = cli.print_error (message)
            in os.exit_usage ()
            end
      | Ok (parsed) => run (parsed)
```

Compiler et lancer :

```sh
yack build src/main.yack demo
./demo -v -o out.txt a.yack b.yack
```

Sortie :

```text
mode: verbose
sortie: out.txt
fichiers: a.yack, b.yack
```

`yack run` ne transmet pas d'arguments au programme ; pour un CLI, construire
d'abord l'exécutable avec `yack build`, puis le lancer directement.

## 2. Les trois constructeurs de `spec`

```yack
Flag (long, short, help)
Opt (long, short, help, default)
Positional (name, help, required)
```

| Constructeur | Exemple | Ce que `parse` produit |
|---|---|---|
| `Flag` | `args.Flag (("verbose", Some ("v"), "mode bavard"))` | `--verbose` ou `-v` met le booléen à `true` |
| `Opt` | `args.Opt (("output", Some ("o"), "fichier de sortie", Some ("out.txt")))` | `--output x`, `--output=x`, `-o x`, `-o=x` |
| `Positional` | `args.Positional (("file", "entrée", true))` | un argument nu, obligatoire si `true` |

Le quatrième champ de `Opt` sert à l'**affichage de l'aide**. Il ne fournit pas
automatiquement la valeur au programme : pour un défaut à l'exécution, utiliser
`args.take_option_or`.

## 3. Lire le résultat

`args.parse (specs, argv)` rend `parsed result`. Le cas `Error` contient un
message lisible ; le cas `Ok` contient un `parsed`.

Les accesseurs consomment le `parsed` et rendent une paire :

```yack
val pair = args.take_flag (parsed, "verbose")
val verbose = pair.0
val rest = pair.1
```

| Accesseur | Résultat |
|---|---|
| `take_flag (parsed, "verbose")` | `bool * parsed` |
| `take_option (parsed, "output")` | `string option * parsed` |
| `take_option_or (parsed, "output", "out.txt")` | `string * parsed` |
| `positionals (parsed)` | `string list` |

Il existe aussi des versions qui jettent le reste du résultat :
`has_flag`, `get_option`, `get_option_or`. Elles ne servent que si l'on a fini
d'extraire les autres champs, puisque le `parsed` est consommé.

`parsed` est Affine : on ne peut pas le réutiliser deux fois. C'est pour cela
que chaque `take_*` rend le `parsed` restant, à passer à l'appel suivant.

## 4. `--help`, `--version` et code de sortie

Le parseur ne sait pas quoi afficher : il ne fait que constater qu'un flag
nommé `help` ou `version` est présent. Il a deux conséquences utiles :

- les positionnels obligatoires ne sont pas réclamés ;
- c'est à l'application de déclarer le flag et de traiter le cas.

```yack
let val pair = args.take_flag (parsed, "help")
    val help = pair.0
    val rest = pair.1
in
    if help then
        let val _ = cli.print_help ("demo", "Démo yack/args.", specs ())
        in os.exit_ok ()
        end
    else ...
end
```

`cli.print_help` rend une chaîne puis l'écrit ; `cli.print_error` écrit sur
stderr ; `os.exit_ok ()`, `os.exit_usage ()` et `os.exit_failure ()` terminent
avec les codes conventionnels 0, 2 et 1.

## 5. Syntaxe reconnue

| Forme | Exemple |
|---|---|
| Flag long | `--verbose` |
| Flag court | `-v` |
| Option longue séparée | `--output out.txt` |
| Option longue collée | `--output=out.txt` |
| Option courte séparée | `-o out.txt` |
| Option courte collée | `-o=out.txt` |
| Fin des options | `--` |
| Cluster de flags | `-al` équivaut à `-a -l` |
| Positionnel | `a.yack` |
| Tirets seuls | `-` est un positionnel |

Un cluster `-abc` n'est découpé que si chaque lettre est l'alias court d'un
`Flag` ; un alias d'`Opt` dans un cluster reste une erreur, pour ne pas changer
le sens de `-o value`. Une option inconnue, une valeur manquante ou un flag
suivi de `=valeur` produisent une `Error`.

## 6. Pièges à retenir

1. **`specs` est consommé par `parse`** : si l'aide a besoin de la même liste
   plus tard, rappeler `specs ()` (comme dans l'exemple) ou copier la liste
   avant le parsing.
2. **`parsed` est consommé par chaque accesseur** : toujours récupérer
   `pair.1` et le passer au suivant.
3. **Le défaut de `Opt` n'est pas appliqué** : `Some ("out.txt")` dans la spec
   ne sert qu'à l'aide. Utiliser `take_option_or` pour la valeur réelle.
4. **Seuls les positionnels `required = true` sont comptés** pour l'erreur
   `missing required argument`.
5. **Les sous-commandes ne sont pas intégrées** : déclarer un positionnel
   `command`, puis faire un `case` dessus dans `main`.

Pour un exemple plus complet avec ANSI et mini-TUI, voir
[`examples/cli`](../examples/cli/).
