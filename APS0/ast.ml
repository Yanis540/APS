(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

(* type TypesPrimitive = Int | Bool
type typz = Type TypesPrimitive |FunctionType types *  TypesPrimitive 
and 
  types = ASTType of typz | ASTTypes of typz * types
	

type def = 
  ASTconst of string * typz * expr  *)

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
      
type cmd =
    ASTStat of stat



