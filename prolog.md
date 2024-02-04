Dans le projet, il est demandé d'implanter un typeur en Prolog, 
ou plus exactement, dans un dialecte de Prolog appelé SWI-Prolog.

https://www.swi-prolog.org

SWI-Prolog est installé à la PPTI.

Lancement d'un programme Prolog
-----


Un programme Prolog est une suite de faits. Par exemple, la phrase `papa(john,cat).` spécifie que `john` est le `papa` de `bob`.

```
$ echo "papa(john,bob)." > prog.pl
```

La commande `swipl` lance la boucle d'interaction de SWI-Prolog.
Le prompt `?-` s'affiche. On peut alors faire des requêtes, comme par exemple : `papa(X,Y).` qui demande au système Prolog de trouver des valeurs pour `X` et `Y`.

```
$ swipl prog.pl 
?- papa(X,Y).
X = john,
Y = bob.

?- papa(bob,Z).
false.

?- halt.
$
```
Commentaires
-----

Les commentaires en Prolog sont 
- soit multi-lignes `/* .... */` (comme en C)
- soit sur une ligne commençant par `%` (comme en Latex).

Programme Prolog avec un argument passé en ligne de commande
-----

Dans le cas du typeur APS, on veut que le programme Prolog lise en entrée
un terme Prolog et réponde `ok` si le programme est bien typé, ou `ko`.

Pour cela, le programme Prolog peut avoir un point d'entrée :

```
main :- read(user_input, X), write("hello "), write(X).
```

et il peut-être invoqué avec un argument comme suit :

```
$ echo "john." | swipl -g main -t halt prog.pl
hello john
```

Syntaxe des programmes Prolog
------

Voici un programme Prolog :
```
a.
b.
c.
d :- a,b,c.
```

C'est une suite de faits : *A est vraie*, *B est vraie*, *C est vraie*, *D est vraie si A et B et C sont vraies*

La grammaire ci-dessous définie une syntaxe simplifiée pour Prolog :
- Un programme `<prog>` est un suite de faits.
- Un fait `<fait>` est soit un prédicat `p` suivi d'un point (`.`), soit un prédicat suivi du symbole `:-` suivi d'une conjonction de prédicats `cp`. 
- Une conjonction de prédicats est un suite de prédicats `p` séparés par des virgules. 
- Un prédicat est un identificateur en minuscules `f` avec zero ou plusieurs arguments entre parenthèse. 
- Les arguments (`<args>`) sont des termes séparés par des virgules.
- Un terme `t` est soit une variable `x` (commençant par une majuscule) représentant un terme inconnu, soit un identificateur `f` en minuscules avec zero ou plusieurs arguments entre parenthèse.

```
<prog> ::= <fait>
         | <fait> <prog>

<fait> ::= p.
         | p :- cp.

cp ::= p
     | p, cp

p ::= f
    | f (<args>)

<args> ::= t
         | t, <args>

t ::= Y 
    | f
    | f(<args>) 
```
