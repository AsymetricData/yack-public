# yack/web : framework HTTP minimal

> Statut : V1 fonctionnelle. C'est un package optionnel, `lib/web`, construit
> sur le serveur epoll de `lib/net`. Il parle HTTP/1.1 avec keep-alive et sert
> plusieurs connexions à la fois.

## Principes

- Pas de `ResponseWriter` : `Response.t` est une valeur Affine qui se déplace.
- `Request.t` est possédée par le handler. Les paramètres de handler utilisent
  `consume`, pour que l'appel par pointeur de fonction transfère réellement la
  requête. Les accesseurs prennent `borrow Request.t` : lire ne consomme plus,
  donc un handler peut enchaîner plusieurs lectures sur le même emprunt et la
  requête est libérée une seule fois à la fin. Il n'y a pas de variantes
  `*_keep`.
- Le package n'utilise pas de closure capturante, et n'en attend pas du langage.
- Un middleware est un datatype `Copy`, compilé au démarrage en `entry` (record
  plat `Copy`) et stocké dans un tableau partagé par le routeur ;
  `Middleware.run` le parcourt par emprunt, sans copie par requête.
- Le routeur est une série de tableaux alloués au démarrage, jamais recopiés
  par requête.
- Le serveur est la boucle epoll de `net/Server`, avec des buffers réutilisés
  et du keep-alive. Les slots entrée/sortie sont réservés non initialisés
  (`array_make_uninit`) et hors huge pages : le RSS suit le nombre de
  connexions actives, pas la capacité maximale de 1024 slots. Une connexion
  lente, inactive ou qui ne lit pas sa réponse est fermée, et le serveur
  s'arrête proprement sur SIGINT/SIGTERM et recule quand `accept` rend
  `EMFILE`.
- `Request.t` garde le texte brut et des offsets ; method, path, query,
  en-têtes, cookies et body sont découpés à la demande. Un handler qui ne lit
  que `?name=` ne paie ni la liste des en-têtes ni celle des cookies.
- Les erreurs sont des valeurs (`result`), pas des exceptions.

## Arborescence

```text
lib/web/
  yack.toml
  src/
    Cookie.yack
    Handler.yack
    Html.yack
    Middleware.yack
    Request.yack
    Response.yack
    Router.yack
    Server.yack
    Session.yack
```

## Nomenclature

| Go | yack/web |
|---|---|
| `http.Request` | `Request.t` |
| `http.ResponseWriter` | `Response.t` |
| `http.ServeMux` | `Router.t` |
| `ListenAndServe` | `Server.run` |
| `http.Cookie` | `Cookie.t` |
| `html/template` | `Html.node` |

## Exemple

```yack
import web/Response
import web/Router
import web/Server

fun home
    (request : consume Request.t
    , params : consume (string * string) list
    ) : Response.t =
    Response.text ("hello")

fun main () =
    let val app =
            Router.new ()
            |> Router.get ("/", home)
        val result = Server.run (app, Server.config (8080))
    in
        case result of
            Ok (_) => ()
          | Error (message) => print_string (message)
    end
```

Le package de l'application déclare :

```toml
[dependencies]
net = { path = "../../lib/net" }
web = { path = "../../lib/web" }
```

Par défaut, le port est exclusif (`SO_REUSEPORT` désactivé) : une seconde
instance échoue avec `EADDRINUSE` au lieu de partager le trafic. Pour l'autoriser
explicitement (benchmark multi-processus), utiliser
`Server.reuse_port (Server.config (8080), true)`.

## Router

```yack
Router.new   : unit -> t
Router.get   : t * string * handler -> t
Router.post  : t * string * handler -> t
Router.put   : t * string * handler -> t
Router.delete: t * string * handler -> t
Router.mount : t * string * t -> t
Router.with  : t * Middleware.t -> t
Router.param : (string * string) list * string -> string option
```

Patterns :

- segment littéral : `/users` ;
- paramètre : `/users/:id` ;
- wildcard : `/files/*path` ;
- 404 si aucun chemin, 405 si le chemin matche mais pas la méthode.

## Middleware

```yack
datatype t =
    Log
  | Cors of provider
  | Security_headers
  | Basic_auth of provider
  | Static of provider
  | Before of (consume Request.t) -> Request.t * Response.t option
  | After of (consume Response.t) -> Response.t
```

`provider = Provider of unit -> string`. La config est une fonction nommée,
pas une capture. `Middleware.custom_before` et `custom_after` enveloppent un
hook nommé.

`Router.with` compile chaque `t` en `Middleware.entry`, un record plat `Copy`
(hooks, provider, flags). Les entrées vivent dans un tableau alloué au
démarrage ; `Middleware.run` le lit via `array.get_borrow` puis recopie les
champs Copy du layer, donc la chaîne n'est jamais recopiée par requête.

Ordre : les `Before` s'exécutent dans l'ordre d'ajout, les `After` dans l'ordre
inverse. Un `Before` qui rend `Some response` court-circuite.

## Cookies

```yack
Cookie.make   : string * string -> t
Cookie.parse_header : borrow string -> (string * string) list
Cookie.find   : borrow string * (string * string) list -> string option
Cookie.render : t -> string
```

## Sessions

Store en mémoire, possédé par `Server` :

```yack
Request.session : borrow Request.t * string -> string option
Response.session_set : Response.t * string * string -> Response.t
Response.session_clear : Response.t -> Response.t
```

Le cookie ne contient qu'un identifiant opaque, généré depuis
`/proc/sys/kernel/random/uuid`. Les sessions sont perdues au redémarrage.

## HTML

```yack
datatype node =
    Text of string
  | Element of { tag : string, attrs : (string * string) list, children : node list }
  | Raw of string
```

`Text` et les attributs sont échappés. `Html.raw` est la seule sortie non
échappée.

## Limites V1

- pas de TLS, HTTP/2, compression, multipart ;
- pas de pipelining, pas de `Transfer-Encoding: chunked` ;
- sessions : module `Session` fourni, mais le serveur epoll ne les persiste
  pas encore entre les requêtes ;
- les layers d'un sous-routeur monté sont ignorés.

## Test manuel

```console
$ ./yack build examples/webapp/src/main.yack webapp
$ ./webapp
$ curl -i http://127.0.0.1:18087/
$ curl -i http://127.0.0.1:18087/hello/alice
$ curl -i -c cookies.txt 'http://127.0.0.1:18087/login?user=alice'
$ curl -i -b cookies.txt http://127.0.0.1:18087/me
```

Le fuzzer de clients hostiles vit dans `test/http_fuzz.sh`
(`make test-http-fuzz`). Il envoie des trames tronquées, des en-têtes absurdes
et des connexions lentes, puis vérifie que le serveur répond encore, ne fuit
pas de descripteurs et ne tourne pas à vide quand plus aucun client n'est
connecté.
