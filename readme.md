Projet de Lucas Foissey et Pierre Garcia.
À rendre pour le lundi 21 décembre.

Nous avons un analyseur syntaxique, un analyseur lexical et un verificateur de type,
suffisant pour traiter tout les code mini-C qui pourraient etre fournis avec les modèles donnés dans le sujet (avant extension).

pour compiler nous utilisons un make file et tout les fichier produit dans un un dossier out pour une gestion des fichier plus
"propre"

Notre fragment de compilateur prend a l'execution un fichier en entré, il fait alors l'analse du code fournit peut relever differentes erreurs y compris les deux demandées. Le programme va aussi afficher dans le terminal un arbre de syntaxe correspondant au code analysé en syntaxe abstraite (ce qui s'est averer être une éxtension par la suite).

Les differents types d'erreurs sont donnés par les different fichier dans testMcFile.

On peut relever plusieur details:

-les variables globales dans nos fichiers mini-C peuvent être décalrés n'importe où et pas forcémenten amont de toutes les fonctions.

-Il est possible de faire des fonction vide.

-Il est possible d'avoir des instruction vide.

-Nous avons fait du transtypage des int vers les bool et inversement.

-les fonctions recursives sont bien definies

-Nous avons relever toutes les erreurs de typage possible auquel nous avons pu penser



Pour les extension:

-Nous avons fait la boucle for avec sa propre definition (cela nous semblait plus facile).

-Toujours dans les erreurs les erreurs "did you mean '...'?" sur les variable et fonction, quand il en existe deja avec des nom similaire a deux caractère près. Pour cela nous avons utiliser le Topfind Spelll

-l'affichage du programme en syntaxe abstraite comme dit precedement.
