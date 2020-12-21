Projet de Lucas Foissey et Pierre Garcia.
À rendre pour le lundi 21 décembre.

Nous avons un analyseur syntaxique, un analyseur lexical et un verificateur de type.
Ces trois éléments sont suffisants pour traiter tout les code mini-C qui pourraient etre fournis avec les modèles donnés dans le sujet initial @(avant extension).

Pour compiler notre projet nous utilisons un make file. Les fichier produit par ce dernier (les .ml .mli ou .cmo) sont dans un un dossier out pour une gestion des fichier plus "propre". Pas l'executable qui s'appelle Mcc et qui est produit dans src.

Notre fragment de compilateur prend a l'execution un fichier en entré, il fait alors l'analse du code fournit peut relever differentes erreurs y compris les deux demandées. Le programme va aussi afficher dans le terminal un arbre de syntaxe correspondant au code analysé en syntaxe abstraite (ce qui s'est averer être une éxtension par la suite).

Les differents types d'erreurs sont donnés par les differents fichier dans testMcFile.
Et peuvent être obtenu en executant la commande:
./Mcc testsMcFile/nomDuFichier.mc
Depuis src

On peut relever plusieur details quand à notre projet:

-les variables globales dans nos fichiers mini-C peuvent être déclarés n'importe où et pas forcément en amont de toutes les fonctions.

-Il est possible de faire des fonction sans instructions.

-Il est possible d'avoir des instruction vide. (peut être utile dans les for)

-Le vérificateur de type accepte le transtypage des int vers les bool et inversement.

-Les fonctions recursives sont bien verifiées definies.

-Les erreurs de typages dans les instructions ou dans les branchements sont détéctés par notre parser (à priori).



Pour les extension, nous avons réalisé:

-Nous avons fait la boucle for avec sa propre definition (cela nous semblait plus facile).

-Les erreurs "did you mean '...'?" sur les variables et fonctions, quand il en existe déjà avec des noms similaires à deux caractère près. Pour cela nous avons utilisé la bibliothèque Spelll. (opam install Spelll)

-L'affichage du programme en syntaxe abstraite comme dit precedement.
