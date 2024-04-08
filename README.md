# APS : Analyse programme et sémantique Sorbonne université 
## Contributions  : 

-   Yanis Tabellout : 21307532
-   Salim Tabellout : 21307533
## Description
Il s'agit du projet APS délivré pour Les étudiants du Master 1 STL. Ce projet est divisé en 4 parties : 

1. APS0: **noyau d'APS**
2. APS1: **Instructions et mémoire**
3. APS1A: **Référence et valeur, paramètres procéduraux**
4. APS2: **Liste**

Chaque partie d'aps contient en plus de l'analyseur syntaxique et lexicale : 
-   **Typeur** 
-   **Évaluateur** 

# Structure de fichiers 
```
APS0
├───archive
├───expected
│   ├───defs
│   │   ├───const_1
│   │   ├──────eval.result
│   │   ├──────type.result
│   │   ├───...
│   ├───exprs
│   │   ├───abs_1
│   │   ├───...
│   └───...
├───results
│   ├───defs
│   │   ├───const_1
│   │   ├──────eval.result
│   │   ├──────type.result
│   │   ├───...
│   ├───exprs
│   │   ├───abs_1
│   │   ├───...
│   ├───...
│   │
│   └───results.csv
├───Samples
│   ├───archive
│   ├───defs
│   └──────const_1.aps
├───Setup.ipynb
├───type.sh
└───test.sh
```

-   **expected** : contient les résultats attendues pour chaque programme de samples (résultat du typeur et de l'évaluateur)
-   **results** : contient le résultat des programmes aps de samples, et le fichiers **results.csv** contient le résultat de tout les programmes
-   **Samples** : contient les programmes aps

# Lancer l'execution : 
Deux choix s'imposent, soit vous prenez le **test.sh** ou bien d'utiliser **setup.ipynb**. On vous conseille vivement d'utiliser le notebook car il permettra de sauvegarder tout les résultats des programmes (typage/évaluation) mais aussi d'avoir certaine statistques.
## Shell 
Dans un shell 
```bash
./test.sh
# si vous être sur windows faudra installer wsl et installer le compilateur d'ocaml puis faire  : 
wsl 
./test.sh # la commande devrait être 

``` 


## Notebook
Le notebook contient toutes les instructions nécessaires pour : 

-   Générer les éxecutables
-   Génrer les fichiers results
-   Faire des tests automatisés et les sauvegarders 
-   Afficher certaines statstiques

Veuillez installer Python ainsi que les extensions nécessaire pour le notebook (jupyter). On a testé le script sur **la version 3.10.5**.
Si vous êtes sur **Windows** veuillez installer **wsl** ainsi que le **compilateur d'ocaml dans wsl**. Si vous rencontrez des difficulutés veuillez installer les bons package, mais en cas de soucis veullez essayer d'installer les paquets suivants : 
```shell
pip install pandas 
pip install matplotlib 
pip install sklearn 
```
