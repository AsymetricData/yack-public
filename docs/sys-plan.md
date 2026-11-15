# Plan : fonctions système dans la stdlib

## Principe

Le compilateur ne fournit qu'un mécanisme, la déclaration `extern`, réservée à
la stdlib (même garde que `array_kernel_*`, E3042). Un `extern` est un appel
libc brut qui rend un `int` (-1 et errno en cas d'échec) ou remplit un buffer.

Toute l'API publique est écrite en Yack, dans `lib/yack/src/*.yack`. Ce qui peut
échouer rend `'a result`, avec un message `op: strerror` comme `io.read_file`.
Ce qui peut être absent rend `'a option`, et ce qui ne peut pas échouer sur
Linux (les horloges) rend la valeur directement. Les ressources (fichier
ouvert, processus, connexion) sont `Linear` : la fermeture est obligatoire et
vérifiée par les types.

## Portabilité : Linux x86-64 uniquement

Les `extern record` (`struct stat`, `timespec`, `addrinfo`) codent les offsets
de Linux x86-64, `getdents64` est appelé par le numéro 217 (61 sur aarch64) et
le runtime LLVM suppose la même ABI. `yack build`, `yack run` et `yack llvm`
refusent de tourner si `Posix.ProcEnv.uname` ne rend pas `Linux` avec
`x86_64`/`amd64` ; `check`, `fmt` et le LSP restent disponibles ailleurs. Un
port aarch64 demande de revoir chaque `extern record`, le numéro de
`getdents64` et les déclarations libc du runtime.

## Décision (prise le 2026-11-14) : `extern` réservé à la stdlib

Option A : un builtin par fonction, comme avant. C'est simple, mais chaque
ajout touche 6 fichiers du compilateur (resolve, check_builtins, check_types,
lower_builtins, llvm_builtin_sys, alloc_runtime).

Retenu : une déclaration `extern` réservée à la stdlib, limitée aux types
`int`, `int64`, `borrow string` (pointeur + longueur, ou copie terminée par
NUL) et `borrow bytes` / `int8 array` (pointeur). Un seul chantier compilateur,
ensuite chaque fonction système s'écrit dans la stdlib sans y toucher. Il n'y a
pas d'FFI publique : les programmes utilisateur n'y ont pas accès.

Un `extern` est toujours privé au module qui le déclare. Le compilateur refuse
tout accès depuis un autre module (import, appel qualifié, réexport), même
depuis un autre module de la stdlib. Chaque module écrit donc son wrapper
public typé (`result` / `option`), ce qui est voulu. Un test de rejet couvre
l'appel depuis un autre module et l'appel depuis un programme utilisateur.

## P0 : utile tout de suite pour un service

| Module | API | Échec |
| --- | --- | --- |
| `yack/time` | `monotonic_ms ()`, `now_ms ()` (epoch), `sleep_ms`, `format_utc` (ISO 8601, pur Yack) | aucun |
| `yack/random` | `bytes (n) : int8 array result` (getrandom), `token_hex (n) : string result`, `int_below (n) : int result` | result |
| `yack/fs` | `exists`, `stat` (taille, mtime, kind) `: info result`, `mkdir`, `mkdir_all`, `remove`, `rename`, `list_dir : string list result`, `append_file` | result |
| `yack/os` | vrai `getenv` (libc, pas `/proc`), `pid ()`, `exit` déjà là | option / aucun |

## P1 : clients et outils

| Module | API | Échec |
| --- | --- | --- |
| `net/Net` | `resolve (host) : addr list result` (getaddrinfo), `connect (host, port) : stream result`, `connect_timeout` | result |
| `yack/process` | `run (prog, args) : output result`, `run_with` (stdin texte, fd ou hérité ; stdout/stderr capturés ou fd) par popen/pclose | result |
| `yack/file` | handle `Linear` : `open`, `read_chunk`, `write_all`, `write`, `seek`, `sync`, `close` pour lire ou écrire sans tout charger en mémoire | result |
| `yack/io` | `read_line` sur stdin en streaming, `is_tty` | option / bool |

## P2 : plus tard

`setenv`, `chdir`, `chmod`, `symlink`, `temp_file` (mkstemp), signaux
utilisateur au-delà de stop.

## Déjà vérifié

- `send` utilise `MSG_NOSIGNAL | MSG_DONTWAIT` : un client qui ferme ne tue pas
  le serveur par SIGPIPE.
- `sys.monotonic_ms : unit -> int64` existe (correctif timeout de
  `http_server`), et `yack/time.monotonic_ms` l'expose maintenant.

## Ordre

1. [x] `extern` stdlib-only dans le compilateur (garde E3042, privé au module
   E3043, types `int`/`int64`/`borrow string`/`borrow int8 array`). Fait le
   2026-11-14 : `extern fun nom (...) : type = "symbole"` est désucré en
   fonction privée dont le corps est l'appel libc ; le test
   `extern_stdlib` de `test/run.sh` l'exécute de bout en bout.
2. [x] `yack/time` (le kernel monotonic existe déjà), puis `yack/random`.
   Fait le 2026-11-14 : `time.monotonic_ms`, `time.now_ms`, `time.sleep_ms`,
   `time.format_utc` (ISO 8601 avec millisecondes, calcul pur Yack) ;
   `random.bytes`, `random.token_hex`, `random.int_below` (getrandom, rejet
   uniforme).
3. [x] `yack/fs` et `yack/file`. Fait le 2026-11-14 : `fs` couvre `exists`,
   `stat`, `mkdir`, `mkdir_all`, `remove`, `rename`, `list_dir`,
   `append_file` ; `file` fournit un handle Linear avec `open`,
   `read_chunk`, `write`, `sync`, `close`. `stat` lit `struct stat` par un
   `extern record` à offsets explicites et `extern_view` ; `list_dir` lit
   `getdents64` et ferme son descripteur.
4. [x] `net` resolve et connect. Fait le 2026-11-14 avec `c_ptr` et
   `extern_view_ptr` : `yack/net` lit `getaddrinfo`, libère chaque liste par
   `freeaddrinfo`, et `net/Net.resolve` / `Net.connect` exposent l'API
   publique. `yack/process` est fait le 2026-11-15 : `popen`/`pclose`
   capturent `stdout`, `stderr` passe par un fichier temporaire supprimé
   après lecture, les arguments sont échappés entre quotes simples et
   `status` décode le statut de `wait (2)`. `run_with` ajoute stdin par
   texte ou descripteur, et `stdout`/`stderr` vers un descripteur existant ;
   les descripteurs sont dupliqués au-dessus de 2 pour ne pas viser le pipe
   de `popen`. `stdout` est drainé en temps linéaire, les fichiers
   temporaires respectent `TMPDIR` (0600, `O_EXCL`, `O_CLOEXEC`) et sont
   supprimés sur tous les chemins ; une lecture en échec rend `Error`.

Chaque étape : un worker, tests ciblés seulement (le programme d'exemple du
module + `make test-stdlib-docs`), pas les suites complètes.
