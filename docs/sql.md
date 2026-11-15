# SQLite

`lib/sql` est un package optionnel : un adaptateur SQLite au-dessus des builtins
`sqlite_*`. Le build natif ajoute `-lsqlite3` quand un programme utilise ce
package.

## Types

```yack
datatype value =
    Null
  | Int of int
  | Text of string

type row = (string, value) list
type result = row list
```

Les flottants et les blobs passent par `Text`, pour ne pas dépendre d'un format
binaire.

## API SQLite

```yack
open         : string -> db result
close        : db -> unit
exec         : borrow db * string -> unit result
changes      : borrow db -> int
prepare      : borrow db * string -> statement result
finalize     : statement -> unit
query        : borrow db * string -> result result
```

`db` et `statement` sont **Linear**. Une connexion se termine par `close`,
une requête préparée par `finalize`, une fois et une seule sur chaque chemin :

- fermer deux fois, ou utiliser `db` après `close`, est rejeté (E0402) :
  libsqlite lirait de la mémoire libérée ;
- oublier `close` ou `finalize` sur un chemin est rejeté (E0403).

Les opérations qui ne ferment pas empruntent la connexion : `exec (borrow db,
...)`. Un statement préparé par `prepare` garde lui aussi un emprunt de la
base : `close (db)` est refusé (E0501) tant qu'il n'est pas finalisé. Le
statement rendu par `query_handle` n'a pas de base à emprunter (`None` dans
son champ `owner`) ; ce chemin brut reste non vérifié, comme ci-dessous.

### Handles bruts (non vérifiés)

```yack
handle       : borrow db -> int64
query_handle : int64 * string -> result result
```

`sys.thread_spawn` ne transporte qu'un `int64`. Pour prêter une connexion à
un thread, on passe son handle brut, et le worker fait ses requêtes avec
`query_handle`. Ce chemin n'est **pas vérifié** : rien ne lie le handle au
`db` d'origine. Le propriétaire garde le `db` ouvert jusqu'au `thread_join`
du worker, et un seul thread utilise la connexion à la fois. Le worker ne
peut pas fermer la connexion : aucune fonction ne ferme un handle brut, et
aucune ne refait un `db` à partir d'un `int64`.

## Pool 1 writer / N readers

`sql/Pool` ajoute un pool SQLite :

```yack
open           : string * int -> pool result
close          : pool -> unit
reader_count   : borrow pool -> int
acquire_reader : pool -> reader option * pool
release_reader : pool * reader -> pool
reader_handle  : borrow reader -> int64
query_reader   : pool * string -> Sql.result result * pool
query_writer   : pool * string -> Sql.result result * pool
exec_writer    : pool * string -> unit result * pool
```

`pool` et `reader` sont Linear. `acquire_reader` sort une connexion du pool
(tour à tour) dans un `reader`, ou rend `None` si tous les readers sont
dehors ; `release_reader` la remet au bout de la file. Tant qu'un `reader`
est dehors, personne d'autre ne l'obtient : chaque thread a sa connexion.
`close` ferme le writer et les readers présents, donc les readers sortis
doivent être rendus avant. `query_reader` prend un reader et le rend, ou
interroge le writer si aucun reader n'est libre. `reader_handle` donne le
handle brut d'un reader pour `sys.thread_spawn` (non vérifié, voir plus
haut) : on ne rend le reader qu'après le `thread_join`.

`open (path, 10)` ouvre un writer et 10 readers sur le même fichier. Le writer
active WAL, `busy_timeout` et `synchronous=normal` ; les readers sont ouverts
en `query_only` avec un `busy_timeout`.

Le pool garde **un seul writer**, parce que SQLite ne sait pas écrire en
parallèle sur une base classique. Les readers peuvent lire en parallèle avec
WAL.

`bench/sqlite_pool/main.yack` montre 10 threads lecteurs utilisant chacun une
connexion du pool, avec le writer dans le thread principal. Chaque reader est
tenu par un appel récursif pendant que son thread tourne, puis rendu après
`thread_join`.

## Exemple

```yack
import sql/Sql
import sql/Sqlite

fun main () =
    case Sqlite.open (":memory:") of
        Error message => print_string (message)
      | Ok db =>
            let val _ = Sqlite.exec (borrow db, "create table t(id integer, name text);")
                val _ = Sqlite.exec (borrow db, "insert into t values (1, 'alice');")
                val _ = Sqlite.exec (borrow db, "insert into t values (2, 'bob');")
                val result = Sqlite.query (borrow db, "select id, name from t order by id;")
                val _ = Sqlite.close (db)
            in
                case result of
                    Error message => print_string (message)
                  | Ok rows => ...
            end
    end
```

## Builtins bas niveau

```text
sqlite_open, sqlite_close, sqlite_exec, sqlite_prepare, sqlite_finalize,
sqlite_step, sqlite_column_count, sqlite_column_name, sqlite_column_type,
sqlite_column_int, sqlite_column_text, sqlite_changes, sqlite_errmsg
```

Ils sont destinés à `lib/sql`, pas à l'usage applicatif direct.

## Limites

- pas encore de binding typé paramétré (`?`) ;
- pas de transactions explicites ;
- pas de BLOB binaire ;
- `close`/`finalize` ne sont pas automatiques (pas de destructeur), mais leur
  oubli est rejeté à la compilation ;
- SQLite est chargé dynamiquement (`-lsqlite3`), donc la machine de build doit
  avoir le développement SQLite installé.

## Test

```sh
make test-sql
```
