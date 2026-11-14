# Threads

Yack expose deux builtins POSIX minimaux :

```yack
sys.thread_spawn : ((int64) -> unit) * int64 -> int64
sys.thread_join  : int64 -> unit
```

Le worker est une fonction statique `int64 -> unit`. Le langage n'a pas de
lambda capturante : un nom de fonction est un pointeur de code, sans
environnement. Pour transférer un handle, on encode son `int64` (par
exemple un handle SQLite).

```yack
fun worker (id : int64) : unit =
    print_string ("worker " ^ int_to_string (int64_to_int (id)))

fun main () =
    let val thread = sys.thread_spawn (worker, int_to_int64 (1))
        val _ = sys.thread_join (thread)
    in ()
    end
```

Heap et stack sont supportés. En stack, chaque thread reçoit sa propre
`%region` et son `@yack_root_region` thread-local.

Un échec de `pthread_create` libère le contexte de 16 octets et arrête le
programme avec `[yack] cannot create thread`; un échec de `pthread_join`
arrête avec `[yack] cannot join thread`. `join` ne lit donc jamais un `tid`
non initialisé. `test/threads.sh` force l'échec de création avec
`ulimit -u 1` et vérifie le message.

## Allocation

Les globales d'arène, de scratch, de réserve root et `yack_store_root` sont
`thread_local`. Deux threads ne partagent donc ni bump pointer, ni free-list,
ni profondeur d'arène.

En heap, `yack_alloc`/`yack_free` utilisent `malloc`/`free`, déjà thread-safe.
En stack, chaque thread a sa réserve root thread-local. La réserve par défaut
est 1 GiB **par thread** : pour beaucoup de workers, compiler avec
`--arena-reserve=64M` ou une valeur adaptée.

## Ce qui traverse réellement un thread

Le seul chemin est `sys.thread_spawn`, dont la signature est fixée :

| chemin | ce qui passe | ce qui le garantit |
|--------|--------------|--------------------|
| worker | un pointeur de code statique (nom de fonction) | le type `(int64) -> unit` rejette toute autre fonction (E3004) ; sans lambda capturante, il n'y a pas d'environnement à envoyer |
| payload | un `int64` | copie par valeur ; le type rejette string, borrow, record, tuple, tableau, map et datatype |
| `sys.thread_join` | l'identifiant `int64` du thread | copie par valeur, aucune valeur de retour |
| handles SQLite | `int64` brut (`Pool.reader_handle`, `Sqlite.handle`), lu par `Sqlite.query_handle` | copie par valeur, **non vérifié** : le `reader` Linear reste hors du pool jusqu'au `thread_join`, donc aucun autre thread ne reçoit la connexion ; le worker ne peut pas la fermer |
| état de l'allocateur | rien | arène, scratch, réserve root et `yack_store_root` sont `thread_local` |

Aucun pointeur vers le tas Yack ne peut franchir la frontière. Il faudrait
l'encoder dans un `int64`, et les seules fonctions qui décodent un `int64`
en ressource (`Sqlite.query_handle`) attendent un handle SQLite, pas une
adresse. Aucune ne refait un `Sqlite.db` à partir d'un `int64` : la
connexion reste possédée, et fermée, par le thread qui l'a ouverte. Envoyer un pointeur d'arène demanderait un `int64_to_string`-like
cast, qui n'existe pas.

**Décision : pas de type `Send`/`Shared` pour l'instant.** La frontière est
déjà un `int64` : une propriété `Send` ne rejetterait rien de plus. À
introduire le jour où une API accepte un payload polymorphe, par exemple un
`thread_spawn : ('a -> unit) * 'a`. Le cas se discutera alors avec les mêmes
règles que `Copy` (étape 6 du plan de sûreté).

## Ce qui est vérifié

- `test/reject/thread_spawn_{string,record,borrow,worker_type}.yack` :
  chaque valeur non scalaire est refusée par le typage (E3004).
- `test/thread_tsan.sh`, lancé par `make test-threads` :
  `bench/sqlite_pool/main.yack` sous ThreadSanitizer, puis sous
  AddressSanitizer avec `--arena-quarantine=strict`. Aucune course, aucune
  erreur mémoire.

## Exemple SQLite

`bench/sqlite_pool/main.yack` ouvre un pool `sql/Pool` avec 1 writer et 10
readers. Le thread principal remplit la table via le writer, puis sort
chaque reader du pool (`acquire_reader`) et transfère son handle brut à un
thread qui exécute son `SELECT`. Le reader, Linear, est tenu par un appel
récursif et ne revient dans le pool (`release_reader`) qu'après le
`thread_join` : une connexion n'est utilisée que par un thread à la fois, et
`Pool.close` ne peut pas la fermer pendant qu'un thread s'en sert.

```sh
./yack build --memory=heap bench/sqlite_pool/main.yack /tmp/sqlite-pool
/tmp/sqlite-pool
```

Les tests associés :

```sh
make test-threads
make test-sqlite-pool
```
