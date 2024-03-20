
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
		
verifier_arguments(_,[],[]).
verifier_arguments(G,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	type_expr(G,ARG,ARGTYPE),
	verifier_arguments(G,ARGS,ARGSTYPE).


get_type_argsp([],[]).
get_type_argsp([(val(_),T)|ARGS],[T|RES]) :-
	get_type_argsp(ARGS,RES).
get_type_argsp([(var(_),T)|ARGS],[ref(T)|RES]) :-
	get_type_argsp(ARGS,RES).


modify_type_argsp([],[]).
modify_type_argsp([(var(IDF),T)|ARGS],[(IDF,ref(T))|RES]) :-
	modify_type_argsp(ARGS,RES).
modify_type_argsp([(val(IDF),T)|ARGS],[(IDF,T)|RES]) :-
	modify_type_argsp(ARGS,RES).

verifier_argumentsp(_,[],[]).
verifier_argumentsp(G,[ARGP|ARGSP],[ARGPTYPE|ARGSPTYPE]) :-
	type_expar(G,ARGP,ARGPTYPE),
	verifier_argumentsp(G,ARGSP,ARGSPTYPE).

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
type_expr(G,id(IDF),T):-
    parcours(IDF,G,ref(T)).

/* IF */
type_expr(G,if(E1,E2,E3),T) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,T),
    type_expr(G,E3,T).

/* lambda : [ARGS] e (abs) */
type_expr(G,lambda(ARGS,E),typeFunc(RES,T)):- 
    append(ARGS,G,GU), 
    get_type_args(ARGS,RES),
    type_expr(GU,E,T).
/* app : e e1 e2 e3 ... 
  - vérifier que e est bien une fonction de type : (t1*t2*....*tn) -> T 
  - vérifier que : e1:t1 et e2:t2 ... en:fn 
  e.g : g0(L),type_expr(L,app(id(add),[num(1),num(2)]),T).
*/
type_expr(G,app(E,ARGS),T):- 
    type_expr(G,E,typeFunc(ARGSTYPE,T)),
    verifier_arguments(G,ARGS,ARGSTYPE).


/* and 
    e.g : g0(L),type_expr(L,and(id(true),id(false)),T).
*/
type_expr(G,and(L,R),bool):-
    type_expr(G,L,bool),
    type_expr(G,R,bool).
/* or 
    e.g : g0(L),type_expr(L,or(id(true),id(false)),T).
*/
type_expr(G,or(L,R),bool):-
    type_expr(G,L,bool),
    type_expr(G,R,bool).


/*
    alloc 
*/ 
type_expr(G,alloc(E),vector(_)):-
    type_expr(G,E,int).    



/******************************* EXPAR ********************************/
/* REF */
type_expar(G,adr(ID),ref(T)):-
    type_expr(G,id(ID),ref(T)).
/* VAL */
type_expar(G,E,T):-
    type_expr(G,E,T).



/******************************* INSTRUCTIONS ********************************/


/* echo */
type_stat(G,echo(E),void) :-
	type_expr(G,E,int).
/* SET */
type_stat(G,set(id(VAR),E),void) :-
    /*chercher le type de VAR dans l'environnement puis vérifier */
    type_expr(G,id(VAR),ref(T)),
    type_expr(G,E,T).
/* IF */
type_stat(G,if(E,B_CONS,B_ALT),void) :-
    type_expr(G,E,bool),
    type_block(G,B_CONS,void),
    type_block(G,B_ALT,void).
/* While */
type_stat(G,while(C,B),void) :-
    type_expr(G,C,bool),
    type_block(G,B,void).
/* Call */
type_stat(G,call(X,ARGS),void) :-
    type_expr(G,X,typeFunc(ARGSTYPE,void)),
    verifier_argumentsp(G,ARGS,ARGSTYPE).
/******************************* DEFINITIONS ********************************/
/* const */
type_def(G,constant(X,T,E),[(X,T)|G]):-
	type_expr(G,E,T).	
/* func : FUN <NAME> <>
   - rajouter les arguments dans un env temporaire
   - évaluer le body ou l'expression relier à la fonction 
   - mettre à jours les types des arguments 
   - créer un nouvel environnement gamma' permettant d'enrichir le context gamma 
    e.g : type_def([],function(yanis,T,[(x,int)],num(1)),G).
 */
type_def(G,function(FUNC,T,ARGUMENTS,E),GU):-
	append(ARGUMENTS,G,G_TEMP),
	type_expr(G_TEMP,E,T),
	get_type_args(ARGUMENTS,RES),
	GU=[(FUNC,typeFunc(RES,T))|G]. 
/* funcRec : FUNRec <NAME> <>
   - rajouter les arguments dans un env temporaire
   - rajouter la définiton de la fonction elle même dans l'env temporaire
   - évaluer le body ou l'expression relier à la fonction 
   - mettre à jours les types des arguments 
   - créer un nouvel environnement gamma' permettant d'enrichir le context gamma 
    e.g : type_def([],functionRec(yanis,T,[(n,int)],app(id(yanis),[id(n)])),G).
 */
type_def(G,functionRec(FUNC,T,ARGUMENTS,E),GU):-
	get_type_args(ARGUMENTS,RES),
	append(ARGUMENTS,G,G_TEMP), 
    G_TEMP_TEMP =[(FUNC,typeFunc(RES,T))|G_TEMP],
	type_expr(G_TEMP_TEMP,E,T),
	GU=[(FUNC,typeFunc(RES,T))|G]. 
/* VAR : */
type_def(G,var(X,T),[(X,ref(T))|G]).

/* Proc */
type_def(G,proc(PROC,ARGSP,B),GU):-
    modify_type_argsp(ARGSP,ARGSP_U),
    append(ARGSP_U,G,G_TEMP),
	type_block(G_TEMP,B,void),
	get_type_argsp(ARGSP,RES),
	GU=[(PROC,typeFunc(RES,void))|G].
/* PROC REC */ 
type_def(G,procRec(PROC,ARGSP,B),GU):-
    modify_type_argsp(ARGSP,ARGSP_U),
    get_type_argsp(ARGSP,RES),
	append(ARGSP_U,G,G_TEMP), 
    G_TEMP_TEMP =[(PROC,typeFunc(RES,T))|G_TEMP],
	type_block(G_TEMP_TEMP,B,T),
	GU=[(PROC,typeFunc(RES,T))|G]. 

/******************************* CMDS ********************************/
/* defs 
    e.g : type_cmds([],[declaration(constant(yanis,int,num(1)))],G)
    e.g : type_cmds([],[declaration(constant(x,T,num(1))),echo(id(x))],G).
*/
type_cmds(G,[declaration(X)|Y],void) :-
	type_def(G,X,G_TEMP),
	type_cmds(G_TEMP,Y,void).

/* end */
type_cmds(_,[],void).
type_cmds(G,[X|Y],void) :-
	type_stat(G,X,void), 
	type_cmds(G,Y,void).


/******************************* BLOCK ********************************/

type_block(G,block(B),void):-
    type_cmds(G,B,void).

/******************************* PROG ********************************/
type_prog(G,prog(X),void) :- type_block(G,X,void).

/* d:-a,f(X) veut dire : d est vrai ssi si a est vrai et f(X) est vrai*/   




main_stdin :-
	read(user_input,T),
    g0(L),
	type_prog(L,T,R),
	print(R).