(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)


(* 
  les typ et types : https://github.com/valeeraZ/Sorbonne_APS/blob/master/APS0/parser.mly   
*)
type tprim = Int | Bool | Void

type typ = Type of tprim | TypeFunc of types * typ (*ARROW*)
and types = typ list (* multiple types a * b  *)
and arg = 
  Argument of string * typ 
and args = arg list
and argp = 
  ArgumentP of string * typ 
  | ArgumentPA of string * typ 
and argsp = argp list 

and expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTif of expr * expr * expr
  | ASTand of expr * expr
  | ASTor of expr * expr
  | ASTlambda of args * expr

and exprp = 
  ASTexpr of expr
  | ASTexpAddress of string 

and stat =
    ASTEcho of expr
    | ASTset of string * expr
    | ASTif of expr * block * block
    | ASTwhile of expr * block
    | ASTcall of string * exprp list
      
  
and def = 
  ASTconst of string * typ * expr 
  | ASTfunc of string * typ * args *expr
  | ASTfuncRec of string * typ * args *expr
  | ASTvar of string * typ 
  | ASTproc of string * argsp * block
  | ASTprocRec of string * argsp * block

and cmds =
  ASTStat of stat
  | ASTdef of def * cmds
  | ASTstatCmds of stat * cmds
and block = 
  ASTblock of cmds 



