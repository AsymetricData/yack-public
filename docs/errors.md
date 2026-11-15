# Codes de diagnostic

Yack rend chaque diagnostic sous la forme `error[E3004]: message`, suivi de la
position (`--> fichier:ligne:colonne`) et du texte source. Les lignes `= note:`
donnent le contexte, les lignes `= hint:` proposent une correction. Les
avertissements utilisent `warning[W....]` et ne bloquent ni `yack check` ni
`yack build`.

Les codes `E09xx` sont des erreurs internes : ils signalent un bug du
compilateur, pas du programme. Signalez-les, avec la commande et le fichier
concernés, sur <https://github.com/AsymetricData/yack-public/issues>.

`yack check` rend le statut 0 quand le programme est accepté, 1 quand un
diagnostic d'erreur est émis, et 70 sur une erreur interne `E0902`.

## Lexique et syntaxe (E20xx)

### E2001 — erreur lexicale
Le lexeur ne reconnaît pas un caractère, ou un commentaire `(*` n'est pas
terminé. Vérifiez le jeton autour du curseur ; un commentaire se ferme par
`*)`.

### E2002 — erreur de syntaxe
Le parseur attendait une autre forme : parenthèse, mot-clé, déclaration ou
expression. Lisez le jeton signalé et corrigez la forme ; `yack fmt` montre la
disposition attendue quand le fichier est encore lisible.

```yack
fun main () =
    print_int (1
```

## Noms et types (E30xx)

### E3001 — variable inconnue
Un nom utilisé comme valeur n'est ni un paramètre, ni un `val`, ni un nom
importé. Vérifiez l'orthographe ou définissez le nom avec `val`/`fun` avant son
utilisation.

```yack
fun main () =
    print_int (missing)
```

### E3002 — fonction inconnue
Un nom appelé comme une fonction n'existe pas au niveau module. Vérifiez
l'orthographe, ou définissez `fun nom (...) = ...` au niveau module.

```yack
fun main () =
    print_int (missing (1))
```

### E3003 — mauvais nombre d'arguments
L'appel ne fournit pas le nombre d'arguments de la fonction. Passez tous les
arguments en une fois : `f (a, b)`.

```yack
fun double (x : int) : int = x + x

fun main () =
    print_int (double (1, 2))
```

### E3004 — incompatibilité de type
La valeur ne correspond pas au type attendu à cet endroit : argument, retour,
condition ou opérateur. Rendez le type voulu explicite par une annotation, ou
convertissez la valeur.

```yack
fun main () =
    print_int ("text")
```

### E3008 — paramètre dupliqué
Deux paramètres d'une même fonction portent le même nom. Renommez l'un des
paramètres.

### E3009 — fonction dupliquée
Deux fonctions de niveau module portent le même nom. Renommez l'une des
définitions, ou appelez celle qui existe déjà.

### E3010 — `main` absente ou invalide
Le programme n'a pas de `fun main () : unit` utilisable. Définissez
`fun main () : unit = ...` au niveau module.

### E3011 — builtin redéfini
Un nom réservé du prélude (`print_int`, `print_bool`, `new_token`,
`consume_token`, `new_resource`, `drop_resource`) est redéfini. Renommez la
fonction : les builtins sont réservés.

### E3012 — type inconnu
Un type nommé n'est pas déclaré, ou un type de record se référence lui-même
directement. Vérifiez l'orthographe, ou déclarez le type avec `type` /
`datatype`.

### E3013 — type ou champ dupliqué
Deux déclarations de type portent le même nom, ou deux champs d'un record ou
d'un `extern record` portent le même nom. Renommez l'une des déclarations.

### E3014 — champ ou accès de champ invalide
Le champ demandé n'existe pas, ou l'accès `valeur.champ` s'applique à une
valeur qui n'est pas un record. Utilisez un des champs listés par le
diagnostic, ou déclarez le champ manquant.

### E3015 — type `Copy` avec des composants non-Copy
Un type est déclaré `Copy` alors qu'un de ses champs possède une propriété
`Affine` ou `Linear`. Retirez l'annotation `Copy`, ou rendez tous les champs
`Copy`.

### E3016 — type `Affine` avec un composant `Linear`
Un type est déclaré `Affine` alors qu'un de ses champs est `Linear`. Retirez le
champ `Linear`, ou déclarez le type contenant `Linear`.

### E3017 — type de record ou de tuple non inférable
L'accès à un champ ou à un index de tuple se fait sur une valeur dont la forme
n'est pas encore connue. Ajoutez une annotation de type pour que la forme de la
valeur soit fixée.

### E3018 — champ de record dupliqué dans un motif
Un motif de record nomme deux fois le même champ. Retirez le champ en double.

### E3019 — erreur de tuple
La taille du motif ne correspond pas à celle du tuple, un index de tuple est
hors bornes, ou un motif de tuple s'applique à une valeur qui n'est pas un
tuple. Utilisez un index ou un motif compatible avec l'arité du tuple.

### E3020 — variable de motif dupliquée
Un même nom est lié deux fois dans un motif. Renommez l'une des variables ; un
nom ne peut être lié qu'une fois par motif.

### E3021 — `case` non exhaustif
Un `case` ne couvre pas tous les constructeurs du type examiné, ou pas tous
leurs arguments. Ajoutez une branche `_ => ...` ou couvrez les constructeurs
manquants.

```yack
datatype color = Red | Green

fun label (c : color) : int =
    case c of
        Red => 1

fun main () =
    print_int (label (Green))
```

### E3022 — branche de `case` inatteignable
Une branche ne peut jamais être choisie, parce qu'une branche précédente
couvre déjà les mêmes valeurs. Retirez la branche inutile, ou réordonnez les
branches.

### E3023 — constructeur sans majuscule
Un constructeur de `datatype` doit commencer par une majuscule. Renommez-le
avec une initiale majuscule.

### E3024 — constructeur dupliqué
Deux constructeurs portent le même nom. Renommez l'un des constructeurs ; les
constructeurs partagent un espace de noms unique.

### E3025 — nom déjà utilisé par un constructeur
Une fonction ou une valeur porte le nom d'un constructeur. Renommez la
fonction pour éviter la collision.

### E3026 — arité de constructeur incorrecte
Un constructeur est appliqué avec un argument alors qu'il n'en attend pas, ou
l'inverse. Écrivez `Nom` sans argument, ou `Nom (valeur)` avec un argument.

### E3027 — constructeur inconnu
Le nom utilisé comme constructeur n'est pas déclaré dans un `datatype`.
Vérifiez l'orthographe, ou déclarez le constructeur.

### E3028 — variables de type interdites dans une déclaration de type
Une déclaration de type contient des variables de type à un endroit où elles
ne sont pas encore supportées. Retirez les variables ; les déclarations de ce
genre sont monomorphes pour l'instant.

### E3029 — mauvais nombre d'arguments de type
Un type paramétré est utilisé sans argument, ou avec un nombre d'arguments
différent de sa déclaration. Fournissez le nombre d'arguments de type attendu.

### E3030 — conflit de qualificatif ou de nom importé
Deux imports lient le même qualificatif ou le même nom non qualifié, ou un nom
importé masque un nom du prélude ou une définition locale. Renommez l'import
avec `as`, ou utilisez un nom qualifié.

### E3031 — nom ou type importé absent du module
Le module importé n'exporte pas le nom ou le type demandé. Vérifiez le chemin,
le nom, ou exportez la déclaration depuis ce module.

### E3040 — `array_make` exige des éléments `Copy`
`array_make` ne peut pas dupliquer un élément `Affine` ou `Linear`. Utilisez un
type d'élément `Copy`, ou construisez le tableau avec `array_new` et
`array_push`.

### E3041 — lecture qui copierait un élément `Affine`
`array_get`, `array_get_unchecked`, `array_kernel_load` ou
`array_get_copy_borrow` ne peuvent pas copier un élément `Affine`. Utilisez
`array_get_borrow` pour lire l'élément sans en prendre possession.

### E3042 — primitive réservée à la stdlib
`extern`, `extern record`, les primitives `array_kernel_*` et les primitives
internes de bytes, d'arène ou de vue C ne sont autorisés que dans le dossier
`src` de la stdlib résolue. Importez le wrapper public du module au lieu de la
primitive privée.

### E3043 — `extern` privé à son module
Un `extern` est déclaré dans un module mais importé ou appelé depuis un autre
fichier. Un `extern` n'est jamais exporté, même dans la stdlib : importez le
wrapper public du module.

### E3044 — `map_get` copierait une valeur `Affine`
`map_get` ou `map_get_copy_borrow` ne peuvent pas copier une valeur `Affine`.
Utilisez `map_get_borrow` (ou `map_get_borrow_view`) pour lire la valeur sans
en prendre possession.

### E3045 — type de paramètre `extern` non supporté
Un paramètre d'`extern` n'est pas `int`, `int64`, `borrow string` ou
`borrow int8 array`. Utilisez un de ces types et laissez le wrapper Yack
interpréter les données.

### E3046 — type de résultat `extern` non supporté
Un `extern` n'a pas de type de retour explicite, ou retourne autre chose que
`int`, `int64` ou `unit`. Écrivez `: int`, `: int64` ou `: unit` ; pour remplir
un buffer, retournez le nombre d'octets et interprétez le tableau dans le
wrapper.

### E3047 — disposition d'`extern record` invalide
Un champ d'`extern record` a un type non supporté, dépasse la taille déclarée,
ou le record contient un champ non-`Copy`. Vérifiez l'offset et la `size` ;
seuls les champs de largeur fixe et les autres `extern record` sont admis.

## Propriété et emprunts (E04xx, E05xx)

### E0402 — utilisation après transfert
Une valeur est lue ou déplacée après avoir déjà été transférée à un autre
propriétaire, ou après qu'un de ses champs a été déplacé. Déplacez
l'utilisation avant le transfert, ou copiez la valeur si son type est `Copy`.

```yack
fun main () =
    let val token = new_token ()
        val first = consume_token (token)
    in consume_token (token)
    end
```

### E0403 — valeur `Linear` non consommée
Une valeur `Linear` n'est consommée sur aucun chemin, ou seulement sur un des
branches d'un `if`/`case`, ou elle est abandonnée par un motif `_`. Consommez-la
exactement une fois sur chaque chemin.

```yack
fun main () =
    let val token = new_token ()
    in if true then consume_token (token) else ()
    end
```

### E0500 — opérande de `borrow` invalide
`borrow` s'applique à une variable locale ou à un champ de variable locale, pas
à une expression temporaire ni à un littéral de chaîne mutable. Empruntez une
variable : `borrow user` ou `borrow user.name`.

```yack
fun main () =
    print_int (string_length (borrow ("a" ^ "b")))
```

### E0501 — déplacement d'une valeur empruntée
Le propriétaire est déplacé alors qu'un emprunt de cette valeur est encore
utilisé après. Terminez l'emprunt avant le déplacement, ou déplacez la valeur
avant de l'emprunter.

```yack
fun eat (s : string) : unit = ()

fun main () =
    let val s = "hello" ^ " world"
        val b = borrow s
        val _ = eat (s)
    in print_int (string_length (b))
    end
```

### E0502 — conflit d'emprunts
Deux emprunts incompatibles se chevauchent : un emprunt mutable avec un autre
emprunt, une lecture pendant un emprunt mutable, ou une mutation pendant un
emprunt. Terminez l'emprunt conflictuel d'abord ; les lectures partagées
peuvent se chevaucher, un emprunt mutable est exclusif.

```yack
type cell : Affine = { value : int }

fun touch (c : borrow mut cell) : unit = ()

fun main () =
    let val c : cell = { value = 1 }
        val m = borrow mut c
        val x = c.value
        val _ = touch (m)
    in print_int (x)
    end
```

### E0503 — provenance d'emprunt retourné absente ou ambiguë
Une fonction retourne un `borrow` sans indiquer de quel paramètre il provient.
Ajoutez `from <paramètre>` au type de retour, ou retournez une valeur possédée.

### E0504 — index `from` hors bornes
L'index `from` d'un type de retour emprunté ne désigne aucun paramètre. Utilisez
un index entre 1 et le nombre de paramètres.

### E0505 — source d'emprunt invalide
Le nom donné à `from` n'est pas un paramètre, le paramètre n'est pas un
`borrow`, ou la mutabilité ne correspond pas. Nommez un paramètre `borrow` dans
`from`, ou déclarez le paramètre comme `borrow`.

### E0506 — retour qui ne provient pas du paramètre annoncé
Le `borrow` retourné vient d'un autre paramètre que celui annoncé par `from`,
ou il s'échappe d'une valeur locale. Retournez le paramètre nommé dans `from`,
ou mettez `from` à jour.

### E0507 — emprunt qui s'échappe de son propriétaire
Un `borrow` est utilisé après la fin de la portée qui possède la valeur
empruntée. Déclarez le propriétaire dans un `let` englobant, ou gardez
l'emprunt local.

### E0508 — lecture d'un champ non-`Copy` à travers un emprunt
Un champ `Affine` ou `Linear` est lu à travers un `borrow` ; cette lecture
devrait en copier la valeur. Copiez un champ `Copy`, ou accédez au champ via
une valeur possédée.

### E0511 — capture dans une fonction anonyme
Une fonction `x => ...` utilise une variable de son environnement. Yack n'a pas
de fermetures capturantes : passez la valeur en argument explicite, ou déclarez
une fonction de niveau module avec `fun`.

```yack
fun main () =
    let val n = 1
        val f = (x : int) => x + n
    in print_int (f (1))
    end
```

### E0512 — paramètre emprunté déplacé dans un callback
Un paramètre de callback est seulement prêté, mais le corps le déplace vers une
fonction qui en prend possession. Si le callback doit consommer son argument,
écrivez `consume T -> ...` dans le type de fonction ; sinon lisez la valeur, ou
passez une fonction nommée.

### E0513 — fonction nommée possédante passée comme callback empruntant
La fonction nommée possède son paramètre, mais le type de callback ne le prête
qu'au callback : les deux côtés libéreraient la même valeur. Marquez le
paramètre du callback `consume T -> ...`, ou passez une lambda qui ne fait que
lire.

### E0514 — lambda empruntante passée à un callback qui consomme
Une lambda liée par `val` ne lit son paramètre, mais le callback attendu
transfère la valeur au callback. Inlinez la lambda au site d'appel, où le type
de callback décide de la propriété, ou passez une fonction nommée dont le
paramètre est possédé.

### E0515 — emprunt stocké dans un conteneur
Un tableau ou une map ne peut pas contenir un `borrow` : le conteneur peut
vivre plus longtemps que l'emprunt. Copiez les données empruntées avant de les
stocker, ou gardez l'emprunt local.

### E0516 — emprunt mutable d'un conteneur non supporté
`borrow mut` sur un tableau, une map ou une liste n'est pas encore supporté.
Utilisez `borrow` pour une traversée en lecture seule.

### E0517 — lambda empruntante appelée avec une valeur possédée
La lambda liée par `val` a un paramètre emprunté, mais l'appel lui passe une
valeur possédée : le transfert ne libérerait pas la valeur. Inlinez la lambda
pour que le type de callback décide, ou passez la valeur à une fonction nommée.

### E0518 — fonction locale capturante utilisée comme valeur
Une fonction locale qui capture des variables ne peut pas devenir une valeur de
fonction ; Yack n'a pas de fermetures capturantes. Appelez-la directement, ou
extrayez-la au niveau module.

### E0519 — un binding masque une variable capturée
Un `val` ou un paramètre masque une variable capturée par une fonction locale,
ce qui rend le site d'appel ambigu. Renommez le binding, ou déplacez la fonction
locale au-dessus du binding qui masque.

## IR et monomorphisation (E06xx)

### E0600 — type non supporté par le backend
La première tranche IR ne sait pas représenter ce type. Utilisez un type que le
backend supporte.

### E0601 — monomorphisation impossible
Une fonction ou un appel conserve des variables de type non résolues au moment
de l'abaissement. Ajoutez des annotations pour que chaque variable de type soit
résolue avant la génération de code. Un des diagnostics de ce code
(« destructuring binding survived desugaring ») est interne et signale un bug
du compilateur.

### E0603 — type d'expression manquant dans l'IR
Le type checker n'a pas enregistré le type de cette expression. C'est
normalement un bug du compilateur : simplifiez l'expression et signalez-la.

### E0604 — information d'appel manquante dans l'IR
L'abaissement ne retrouve pas la cible d'un appel ou d'une valeur de fonction.
Appelez une fonction de niveau module directement ; les valeurs de fonction ne
sont pas encore abaissées. Si l'appel est direct, c'est un bug du compilateur.

### E0605 — datatype inconnu à l'abaissement
L'IR ne retrouve pas la déclaration du datatype utilisé. Vérifiez le nom, ou
redéclarez le type dans ce module. Sinon, signalez le bug.

### E0606 — littéral incompatible avec son type IR
Un littéral entier n'a pas de type numérique après résolution. Ajoutez une
annotation de type numérique au littéral.

### E0607 — constructeur inconnu dans un motif
Un motif de `case` utilise un constructeur que l'IR ne connaît pas. Vérifiez le
nom, ou déclarez-le dans un `datatype`. Un constructeur valide qui produit ce
code révèle un bug du compilateur.

### E0608 — constructeur utilisé comme valeur alors qu'il attend un argument
Le constructeur est écrit sans son argument. Écrivez `Nom (valeur)` ; ce
constructeur a besoin d'un argument.

### E0609 — champ de record inconnu à l'abaissement
L'IR ne retrouve pas ce champ dans le type du record. Utilisez un champ déclaré
par le type de record. Un champ valide qui produit ce code révèle un bug du
compilateur.

### E0610 — directive `sprintf` inconnue
La chaîne de format contient une directive non supportée, ou le nombre de
valeurs ne correspond pas aux directives. Utilisez une chaîne littérale et
passez une valeur par directive (`%d`, `%i`, `%s`, `%b`, `%f`, `%c`, `%ld`,
`%lld`, `%%`).

## Régions, arène et échappement (E07xx)

### E0701 — valeur de la région d'activation échappante
Le vérificateur de régions voit une valeur construite dans la région
d'activation qui peut survivre à l'appel : retour, argument d'un appel qui la
conserve, ou stockage dans un conteneur. Construisez la valeur dans la région
de destination, ou enracinez-la avant qu'elle ne s'échappe. Ce diagnostic
appartient à la vérification interne de la stdlib en mode stack ; le rencontrer
dans du code utilisateur est un bug du compilateur.

### E0702 — stockage dont la durée de vie n'est pas prouvée
Un tableau ou une map recevrait une valeur qui peut ne pas vivre aussi
longtemps que le conteneur : valeur `Copy` allouée sur le tas, ou valeur de la
région courante en mode stack. Stockez une valeur fraîchement construite,
scalaire, ou allouée dans une région racine.

### E0703 — l'arène se ferme alors qu'une valeur est encore empruntée
Un `borrow` d'une valeur allouée dans l'arène est encore utilisé après la fin
du bloc `arena`, qui libère toute la région. Utilisez ou copiez les données
empruntées à l'intérieur de l'arène, ou gardez la valeur possédée à l'extérieur
et empruntez celle-là.

### E0704 — valeur allouée sur la pile native échappante
Le vérificateur de régions voit une valeur allouée sur la pile qui peut
survivre à l'appel. Construisez la valeur dans la région de destination ; une
`alloca` ne peut pas s'échapper. Comme `E0701`, ce diagnostic appartient à la
vérification interne de la stdlib en mode stack.

### E0708 — mémoire temporaire utilisée sans bloc `arena`
Le programme appelle une primitive de mémoire temporaire interne au compilateur
hors d'un bloc `arena`. Remplacez l'appel par `arena val ... in ... end` ; tout
ce qui est alloué dans le bloc est libéré d'un coup à `end`.

## Erreurs internes (E09xx)

### E0900 — validation d'ownership du graphe SSA
La validation d'ownership du graphe SSA refuse un programme que le frontend a
accepté. C'est un bug de Yack : le frontend a laissé passer un programme que
son propre abaissement ne peut pas libérer correctement. Signalez le fichier et
la commande.

### E0901 — validation des emprunts du graphe SSA
La validation des emprunts du graphe SSA refuse un programme que le frontend a
accepté. C'est un bug de Yack : le frontend a accepté un emprunt que le graphe
ne peut pas prouver sûr. Signalez le fichier et la commande, avec
`yack ssa --loans` si possible.

### E0902 — erreur interne du compilateur
Une exception SML a échappé à une phase du compilateur : parsing, typage,
vérification d'ownership, abaissement ou génération de code. Le message nomme
la phase en cours (`while ...`). Ce n'est jamais un problème du programme :
signalez la commande et le fichier sur
<https://github.com/AsymetricData/yack-public/issues>. Le statut de sortie est
70.

## Valeurs de fonction (E10xx)

### E1001 — syntaxe `fn` non supportée
Les fonctions anonymes s'écrivent `x => corps`, pas `fn`. Écrivez
`list.map (x => x * 2, values)`, ou passez une fonction par son nom.

### E1003 — appel complet requis
Une fonction est utilisée sans tous ses arguments, par exemple pour obtenir une
fonction partielle. Écrivez `nom (x)` ou `nom (x, y)` ; l'application partielle
n'est pas supportée.

### E1004 — liste de paramètres requise
Une déclaration de fonction n'a pas de liste de paramètres. Écrivez
`fun nom () = ...` ; le currying et les paramètres implicites ne sont pas
supportés.

## Avertissements (W3xxx)

### W3001 — fonction jamais utilisée
Une fonction de niveau module n'est appelée nulle part. Supprimez sa
définition, ou appelez-la depuis le code qui en a besoin. L'avertissement ne
bloque pas la compilation.

### W3101 — copie d'un conteneur possédé à chaque appel
Une fonction chaude (récursive, ou atteignable depuis une fonction récursive)
reçoit un conteneur possédé et en reconstruit un de même forme à chaque appel,
ce qui copie un nœud par élément. Prenez le paramètre en `borrow`, passez le
conteneur à travers les appels, ou utilisez un tableau/`ilist` d'éléments
`Copy`.

### W3102 — conteneur possédé seulement lu
Un paramètre de type conteneur est possédé, mais le corps ne le consomme
jamais : l'appelant transfère la propriété sans nécessité. Prenez
`borrow nom` pour que l'appelant garde la propriété, puis enveloppez l'argument
avec `borrow (...)` aux sites d'appel.

### W3103 — allocation dans la région racine hors arène
En mode stack, une allocation a lieu dans la région racine en dehors d'un bloc
`arena` ; elle se répète à chaque exécution de la fonction. Déplacez
l'allocation dans un bloc `arena`, prenez un `borrow` du conteneur au lieu de le
reconstruire, ou figez la valeur une fois au démarrage.
