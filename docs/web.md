# yack/web : framework HTTP minimal

> Statut : V1 fonctionnelle. Package optionnel `lib/web`, construit sur le
> serveur epoll de `lib/net`. HTTP/1.1 keep-alive, plusieurs connexions.

## Principes

- Pas de `ResponseWriter` : `Response.t` est une valeur Affine qui se déplace.
- `Request.t` est possédée par le handler. Les paramètres de handler utilisent
  `consume`, pour que l'appel par pointeur de fonction transfère réellement la
  requête. Les accesseurs prennent `borrow Request.t` : lire ne consomme plus,
  donc un handler peut enchaîner plusieurs lectures sur le même emprunt et la
  requête est libérée une seule fois à la fin. Pas de variantes `*_keep`.
- Pas de closures capturantes, ni maintenant ni plus tard.
- Middleware = datatype `Copy`, compilé au démarrage en `entry` (record plat
  `Copy`) et stocké dans un tableau partagé par le routeur ; `Middleware.run`
  le parcourt par emprunt, sans copie par requête.
- Routeur = tableaux alloués au démarrage, jamais recopiés par requête.
- Serveur = boucle epoll de `net/Server`, buffers réutilisés, keep-alive.
  Les slots recevront/sortie sont réservés non initialisés
  (`array_make_uninit`) et hors huge pages : le RSS suit le nombre de
  connexions actives, pas la capacité maximale de 1024 slots.
- `Request.t` garde le texte brut et des offsets ; method, path, query,
  en-têtes, cookies et body sont découpés à la demande. Un handler qui ne lit
  que `?name=` ne paie ni la liste des en-têtes ni celle des cookies.
- Erreurs = valeurs (`result`), pas d'exceptions.

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
