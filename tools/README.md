# `yack-watch`

Panneau de lint Yack en terminal, pour la CI ou les éditeurs sans client LSP.

```sh
./tools/yack-watch.ts examples/stdlib/src/main.yack
```

ou, sans dépendre du bit exécutable :

```sh
node tools/yack-watch.ts examples/stdlib/src/main.yack
bun  tools/yack-watch.ts examples/stdlib/src/main.yack
```

Le watcher :

1. scrute les `.yack` et `yack.toml` du paquet (par défaut la racine trouvée
   via `yack.toml`) ;
2. relance `yack check <entry>` après chaque modification ;
3. affiche le statut, la durée et les diagnostics dans un panneau rafraîchi ;
4. peut aussi lancer `yack fmt --check` sur les fichiers modifiés avec `--fmt`.

Options utiles :

```sh
--root <dir>       dossier supplémentaire à surveiller (répétable)
--interval <ms>    période de polling (défaut 400)
--debounce <ms>    délai après modification (défaut 150)
--yack <path>      binaire Yack (défaut ./yack)
--fmt              vérifie aussi le formateur
--once             un seul check, code de sortie 1 si erreur (CI)
--no-clear         garde l'historique dans le terminal
--no-color         désactive les couleurs
```

Exemples :

```sh
# Pendant le développement de la stdlib
./tools/yack-watch.ts examples/stdlib/src/main.yack --fmt

# Vérification unique en CI
./tools/yack-watch.ts examples/stdlib/src/main.yack --once

# Surveiller aussi un paquet de dépendance
./tools/yack-watch.ts examples/httpserver/src/main.yack \
    --root examples/httpserver/src --root lib/net/src
```

Pour l'édition interactive, préférer `yack lsp`. Limites du watcher :

- `yack check` exige une fonction `main` ; le watcher prend donc un fichier
  d'entrée, pas un module isolé ;
- les diagnostics sont affichés en texte ; `yack lsp` fournit le protocole
  structuré ;
- pas de suggestions de correction automatique ;
- pas de daemon : le watcher est un processus de terminal.

# `alloc-report.py`

Rapport d'allocations par site à partir d'une exécution `--trace-alloc`.

```sh
yack alloc-map --json app.yack > /tmp/alloc-map.json
yack build --trace-alloc app.yack app-trace
./app-trace 2> /tmp/alloc.log
./tools/alloc-report.py --map /tmp/alloc-map.json /tmp/alloc.log
```

Chaque ligne du rapport donne les octets vivants, le nombre d'allocations et
de libérations, les octets alloués/libérés, puis le site source :

```text
    live B   live    alloc     free    alloc B     free B  site
        32      1        1        0         32          0  make 6:8  [arena] constructor Cell
```

Le code de sortie est `1` s'il reste des octets vivants, ce qui permet de
l'utiliser en CI.

