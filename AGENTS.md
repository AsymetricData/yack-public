# Règles pour les agents qui codent dans Yack

Ces règles viennent de bugs réels trouvés en revue de la stdlib (`process`,
`net`, `time`, `fs`, `file`, `random`). Chaque règle dit quoi faire et pourquoi.

## 1. Complexité et mémoire

**Ne jamais accumuler une chaîne dans un `borrow` copié à chaque tour.**

```yack
(* MAUVAIS : O(n²) en mémoire. Chaque appel garde son `next` vivant jusqu'au
   retour, donc tous les préfixes coexistent. 1 Mo lu = 124 Mo RSS,
   8 Mo = out of memory. *)
fun drain (fd : int, acc : borrow string) : string =
    ...
    val next = string_copy (acc) ^ chunk
    in drain (fd, borrow next)
```

À faire, au choix :

- accumulateur **possédé** et consommé : `drain (fd, acc ^ chunk)` avec
  `acc : string` (mémoire linéaire, mesuré) ;
- empiler les morceaux dans une `string list`, puis une seule `string.join`
  à la fin.

Règles générales :

- Toute boucle qui lit une entrée de taille non bornée (fichier, pipe,
  socket, stdout d'un processus) doit être linéaire en temps **et** en mémoire.
- Une récursion qui construit un résultat doit être terminale (accumulateur),
  sauf si la profondeur est bornée par une petite constante.
- `^` dans une boucle sur une entrée non bornée : vérifier que la partie
  gauche est possédée et consommée, jamais `string_copy (borrow ...)`.
- Écrire un test avec une entrée d'au moins quelques Mo pour tout code qui
  draine un flux, lancé sous `ulimit -v 4000000`.

## 2. Appels `extern` (FFI libc)

### Codes d'erreur

- Lire la page de manuel : toutes les fonctions ne mettent pas `errno`.
  `getaddrinfo` / `getnameinfo` rendent un code `EAI_*` négatif ; `errno`
  n'a de sens que pour `EAI_SYSTEM`. Mettre le code rendu dans le message.
- Lire `sys.errno ()` **juste après** l'appel qui a échoué, avant tout autre
  appel (un `close` ou un `unlink` de nettoyage écrase `errno`).
- Quand on essaie plusieurs alternatives (`unlink` puis `rmdir`, plusieurs
  adresses dans `connect`), garder l'errno de l'échec pertinent. Ne jamais
  remplacer la vraie cause par un message générique (`no stream address`
  quand la connexion est refusée).

### EINTR et opérations partielles

- `read`, `write`, `getrandom` peuvent rendre `-1` avec `EINTR` :
  réessayer, ne pas renvoyer une erreur ni traiter comme EOF. Pour
  `connect`, EINTR ne coupe pas la connexion : un nouvel appel peut rendre
  `EALREADY` puis `EISCONN` ; `EISCONN` est un succès, `EINTR` et
  `EALREADY` attendent `POLLOUT` puis lisent `SO_ERROR` (pas de plafond
  artificiel de 1 s).
- `write` peut écrire moins que demandé : boucler jusqu'à tout écrire
  (`write_all`), ne pas renvoyer « short write ».
- `getrandom` au-delà de 256 octets peut rendre moins : boucler.
- `read` qui rend `-1` n'est **jamais** une fin de fichier. Seul `0` l'est.

### Descripteurs

- Ouvrir avec `O_CLOEXEC` (`524288`) et créer les sockets avec
  `SOCK_CLOEXEC` (`524288`, à ajouter au type). Sinon chaque processus fils
  hérite de tous les fichiers et sockets ouverts.
- Pour passer un descripteur à un fils, le faire explicitement (`dup2` côté
  fils, ou `dup` côté parent puis redirection), pas en comptant sur
  l'absence de `O_CLOEXEC`.
- Un descripteur rendu à l'utilisateur doit être un type `Linear` avec un
  `close` obligatoire (modèle : `yack/file`). Ne pas rendre un `int` brut.

### Paramètres

- Ne pas passer `NULL` comme `hints` à `getaddrinfo` : préciser
  `ai_socktype = SOCK_STREAM` (sinon chaque adresse revient 3 fois).
- Valider les entrées avant l'appel (port dans `0..65535`, taille positive,
  descripteur positif) et rendre `Error` avec le nom de la fonction.

### Disposition mémoire et architecture

- Les offsets d'`extern record` et les numéros de syscall (`getdents64` =
  `217`) sont **Linux x86-64**. Les commenter comme tels. Ne pas en ajouter
  sans noter l'architecture ; sur aarch64 ils sont différents.
- Vérifier chaque offset contre les en-têtes (`offsetof`), pas de mémoire.

## 3. Erreurs

- Ne jamais avaler une erreur en rendant `""`, `0` ou `[]` dans un chemin
  `Ok`. Si une lecture échoue, la fonction rend `Error`.
- Messages : `module.fonction: action failed (errno N)` ou `(code N)`.
  Le préfixe doit être le nom de la fonction publique appelée
  (`process.run_with`, pas `process.run` depuis `run_with`).

## 4. Fichiers temporaires

- Jamais de nom prévisible. Si la source d'aléa échoue, rendre `Error`,
  pas un nom de repli (`"fallback"`).
- Créer avec `O_CREAT | O_EXCL | O_CLOEXEC` (`64 + 128 + 524288`) et le mode
  `0600` (`384`), pour ne pas suivre un lien symbolique et ne pas exposer
  le contenu aux autres utilisateurs.
- Respecter `TMPDIR`, `/tmp` seulement en repli.
- Supprimer le fichier sur **tous** les chemins, erreurs comprises.

## 5. Processus et shell

- Dans un fils lancé par `popen`, le fd `1` est déjà le pipe : `>&1` ne
  renvoie pas vers le stdout du parent. Tester tout routage de descripteur
  avec les fd `0`, `1`, `2` et un fd `>= 10`.
- `/bin/sh` est bash sur la machine de dev mais dash sous Ubuntu (image
  Docker). N'utiliser que du shell POSIX et tester sous dash.
- Tout argument passe par `shell_quote`. Un argument contenant un octet NUL
  doit donner `Error`, pas être tronqué.

## 6. Horloges et précision

- Une fonction `*_ms` doit avoir une vraie précision milliseconde :
  `clock_gettime`, pas `time () * 1000`.
- Les tests de temps utilisent des valeurs non rondes (ms ≠ 000), sinon une
  perte de précision passe inaperçue.

## 7. Entiers

- `int` fait 32 bits. Un littéral hors plage (`4000000000`) est refusé
  (`E2002`) ; utiliser `int64` et `int_to_int64` pour les grandes valeurs.
- Avant de calculer une borne (`usable = (bound / size) * size`), vérifier
  qu'elle ne peut pas valoir `0` pour une entrée acceptée.

## 8. Tests exigés pour un module stdlib

Pour chaque nouvelle fonction qui touche le système :

- le chemin nominal **et** chaque chemin d'erreur (fichier absent, permission
  refusée, hôte inconnu, connexion refusée) avec le message attendu ;
- une grosse entrée (plusieurs Mo) si la fonction lit un flux ;
- une valeur limite (vide, `0`, négatif, maximum) ;
- un test de fuite dans `test/alloc/` (`expect_no_leak`) et un delta de fd
  à `0` ;
- `test/run.sh` et `test/asan.sh` verts avant le commit.

Lancer les tests avec des limites : `ulimit -v 4000000` et au plus 8 CPU.
Pas de `ulimit -v` sous ASan.

## 9. Style Yack

- `&` est l'alias de `borrow` : préférer `&string` / `&value` à `borrow
  string` / `borrow value` dans le code neuf.
- `!` est l'alias de `consume` dans les annotations de type : préférer
  `!T` à `consume T`.
- Les listes littérales `[a, b]`, `[]` et `x :: rest` remplacent
  `Cons (a, Cons (b, Nil))` dans le code neuf.
- Pas de réécriture massive de l'existant : suivre le style du fichier
  touché, mais ne pas introduire de nouveau `Cons (...)` verbeux.
