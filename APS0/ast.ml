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
  https://github.com/valeeraZ/Sorbonne_APS/blob/master/APS0/parser.mly   
*)



type tprim = Int | Bool 

type typ = Type of tprim | TypeFunc of types * typ (*ARROW*)
and types = ASTType of typ | ASTTypes of typ * types (* multiple types a * b  *)



type expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTif of expr * expr * expr
  | ASTand of expr * expr
  | ASTor of expr * expr
  (* | ASTconst of string * typz * expr *)

type stat =
    ASTEcho of expr
      
type def = 
  ASTconst of string * typ * expr 
type cmds =
  ASTStat of stat
  | ASTdef of def * cmds




