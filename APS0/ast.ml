(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)


type expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTif of expr * expr * expr
  | ASTand of expr * expr
  (* | ASTconst of string * CustomType * expr *)

type stat =
    ASTEcho of expr
      
type cmd =
    ASTStat of stat

(* type TypesPrimitives = Int | Bool

type CustomType = Type TypesPrimitives |FunctionType types *  TypesPrimitives 
and 
  types = ASTType of typ | ASTTypes of typ * typs
	 *)
