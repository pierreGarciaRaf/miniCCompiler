Projet de Lucas Foissey et Pierre Garcia.
À rendre pour le lundi 21 décembre.

L'etat actuel du dm est que nous avons un analyseur syntaxique, un analyseur lexical et un verificateur de type,
suffisant pour traiter tout les code mini-C qui pourraient etre fournis avec les modèles donnés dans le sujet (avant extension).
Notre fragement de compilateur prend a l'execution un fichier en entré, il fait alors l'analse du code fournit peut relever differentes erreurs y compris les deux demandées. Le programme va aussi afficher dans le terminal un arbre de syntaxe correspondant au code analysé. Les different type sont donnés par les different fichier dans testMcFile.
On peut relever un leger detail, les variables globales dans nos fichiers mini-C peuvent être décalrés n'importe où et pas forcémenten amont de toutes les fonctions.
