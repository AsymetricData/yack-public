# Performance notes

Objectif : éviter les algorithmes quadratiques dans le frontend.

## Lexer

- Un seul passage sur la source.
- `String.sub` est O(1) ; les identifiants et littéraux sont découpés avec
  `String.substring`, ce qui coûte la longueur du token. Total : **O(n)**.
- Les commentaires imbriqués sont consommés une seule fois.
- Les listes de mots-clés et de ponctuations sont constantes.

## Parser

- Les tokens sont convertis une fois en `array`.
- La position courante est un index mutable : `current` et `advance` sont O(1).
- Chaque token est consommé au plus une fois.
- La récupération après erreur avance vers le prochain `fun` ou EOF ; elle ne
  revient jamais en arrière. Total : **O(n)**.
- La détection des paramètres dupliqués utilise `StringSet`, un ensemble à
  hachage : coût attendu **O(k)** pour `k` paramètres, au lieu de O(k²) avec une
  association list.

## Diagnostics

- `Source.create` précalcule une fois les débuts de lignes.
- `Source.lineText` répond en **O(1)**.
- Le rendu d'un lot de diagnostics est donc O(source + diagnostics), et non
  O(diagnostics × source).

## Points encore à optimiser

- `Resolve`, `Check` et `Ownership` utilisent des association lists pour les
  portées, les fonctions et les statuts de mouvement. Une recherche coûte
  O(portée). C'est acceptable pour le noyau actuel, mais il faudra une table de
  symboles (ou une map équilibrée) avant les passes de lifetimes sur des
  portées profondes.
- Les diagnostics n'ont pas encore de sortie JSON ; le renderer terminal est
  volontairement simple.
- Aucun AST n'est cloné entre les passes ; chaque passe fait sa propre
  traversée.
