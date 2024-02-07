
/******************************* UTILS ********************************/
/* insipré de : https://github.com/valeeraZ/Sorbonne_APS/blob/master/APS0/typeChecker.pl (on utilise pas leurs code plus qu'il n'est pas adapté à l'énoncé ) */
parcours(X, [(X,V)|_], V).
parcours(X, [_|XS], V) :- parcours(X, XS, V).

get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	type_expr([],A,T),
	get_type(ARGS,TYPES).

get_type_args([],[]).
get_type_args([(_,T)|ARGS],[T|RES]) :-
	get_type_args(ARGS,RES).
		
check_args(_,[],[]).
check_args(G,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	type_expr(G,ARG,ARGTYPE),
	check_args(G,ARGS,ARGSTYPE).

/******************************* UTILS ********************************/
/* g0 */
g0([
    (false, bool),
    (true, bool),
    (not, typeFunc([bool],bool)),
    (eq, typeFunc([int,int],bool)),
    (lt, typeFunc([int,int],bool)),
    (add, typeFunc([int,int],int)),
    (sub, typeFunc([int,int],int)),
    (mul, typeFunc([int,int],int)),
    (div, typeFunc([int,int],int))
]).

/******************************* EXPRESSIONS ********************************/

/* Num */
type_expr(_,num(N),int) :-
 	integer(N).

/* id */
type_expr(G,id(IDF),T):-
    parcours(IDF,G,T).

/* IF */
type_expr(G,if(E1,E2,E3),T) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,T),
    type_expr(G,E3,T).

/* lambda : [ARGS] e  */
type_expr(G,lambda(ARGS,E),T):- 
    append(ARGS,G,GU), 
    type_expr(GU,E,T).
/*?? app : e e1 e2 e3 ... 
  - vérifier que e est bien une fonction de type : (t1*t2*....*tn) -> T 
  - vérifier que : e1:t1 et e2:t2 ... en:fn 
  */
type_expr(G,app(E,ARGS),T):- 
    type_expr(G,E,typeFunc(ARGSTYPE,T)),
    check_args(G,ARGS,ARGSTYPE).

/******************************* INSTRUCTIONS ********************************/


/* echo */
type_stat(G,echo(E),void) :-
	type_expr(G,E,int).	
/******************************* DEFINITIONS ********************************/
/* const */
type_def(G,constant(id(X),T,E),[(X|T)|G]):-
	type_expr(G,E,T).	
/* func : FUN <NAME> <>
   - rajouter les arguments dans un env temporaire
   - évaluer le body ou l'expression relier à la fonction 
   - mettre à jours les types des arguments 
   - créer un nouvel environnement gamma' permettant d'enrichir le context gamma 
 */
type_def(G,function(id(funcName),T,args,E),GU):-
	append(ARGS,G,G_TEMP), 
	type_expr(G_TEMP,E,T),
	get_type_args(ARGS,RES),
	GU=[(funcName,typeFunc(RES,T))|G]. 
/* funcRec : FUN <NAME> <>
   - rajouter les arguments dans un env temporaire
   - évaluer le body ou l'expression relier à la fonction 
   - mettre à jours les types des arguments 
   - créer un nouvel environnement gamma' permettant d'enrichir le context gamma 
 */
type_def(G,functionRec(id(funcName),args,T,E),GU):-
	get_type_args(ARGS,RES),
	append(ARGS,G,G_TEMP), 
    G_TEMP_TEMP = [(ID,typeFunc(RES,T))|G_TEMP],
	type_expr(G_TEMP_TEMP,BODY,T),
	GU=[(funcName,typeFunc(RES,T))|G]. 

/******************************* CMDS ********************************/
/* defs */
type_cmds(G,[declaration(X)|Y],void) :-
	type_def(G,X,G_TEMP),
	type_cmds(G_TEMP,Y,void).

/* end */
type_cmds(_,[],void).
type_cmds(G,[X|Y],void) :-
	type_stat(G,X,void),
	type_cmds(G,Y,void).


/******************************* PROG ********************************/
type_prog(G,prog(X),void) :- type_cmds(G,X,void).

/* d:-a,f(X) veut dire : d est vrai ssi si a est vrai et f(X) est vrai*/   




main_stdin :-
	read(user_input,T),
	type_prog([],T,R),
	print(R).