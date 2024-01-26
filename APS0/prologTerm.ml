(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

(* ! Printing types *)

let primitive_to_string p =
  match p with 
  | Int -> "int"
  | Bool -> "bool"



let rec print_typ t = 
  match t with 
  | Type (p:tprim) ->  Printf.printf "%s" (primitive_to_string p) 
  | TypeFunc (ts,t)-> 
      Printf.printf "Func def ";
      print_types ts ;  
      Printf.printf "->" ;
      print_typ t 

and print_types ts = 
  match ts with 
  | ASTType (t:typ) -> print_typ t 
  | ASTTypes (t,types) -> 
    print_typ t; 
    Printf.printf "* "  ; 
    print_typ t

(* ! Printing arguments *)

let print_arg a = 
  match a with 
  | Argument (idf,t)->
    Printf.printf "Argument %s :" idf ;  
    print_typ t


let rec print_args (argz) = 
  match argz with 
  |  ASTarg a -> 
    Printf.printf "( ";
    print_arg a;
    Printf.printf " )"
    
  |  ASTargs (a,argz') -> 
      Printf.printf"( ";
      print_arg a;
      print_args argz';
      Printf.printf ")"
  


(* ! Expression  *)
let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTif (c,cns,alt) ->
        Printf.printf "If \t"; 
        print_expr c; 
        Printf.printf "THEN  \t"; 
        print_expr cns; 
        Printf.printf "ELSE \t"; 
        print_expr alt; 
    | ASTand (l,r) ->
        Printf.printf "And statement : ";
        print_expr l; 
        Printf.printf "AND \t"; 
        print_expr r;
    | ASTor (l,r) ->
        Printf.printf "OR statement : ";
        print_expr l; 
        Printf.printf "OR \t"; 
        print_expr r;
    | ASTApp(e, es) -> (
	Printf.printf"app(";
	print_expr e;
	Printf.printf",[";
	print_exprs es;
	Printf.printf"])"
      )
and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
	print_expr e;
	print_char ',';
	print_exprs es
      )


(* ! Stat  *)

let print_stat s =
  match s with
      ASTEcho e -> (
	Printf.printf("echo(");
	print_expr(e);
	Printf.printf(")")
      )


let print_def d = 
  match d with 
    ASTconst (idf,t,e)->
      Printf.printf "Constant ";
      Printf.printf "%s" idf;
      print_typ t; 
      print_expr e
  | ASTfunc (name,t,argz,e) -> 
    Printf.printf "Function %s " name; 
    Printf.printf "  : ";  
    print_typ t;
    Printf.printf "  ";        
    print_args argz;      
    Printf.printf "  ";        
    print_expr e 
  | ASTfuncRec (name,t,argz,e) -> 
    Printf.printf "Function %s " name; 
    Printf.printf "  : ";  
    print_typ t;
    Printf.printf "  ";        
    print_args argz;      
    Printf.printf "  ";        
    print_expr e 
 
  
  
    
let rec print_cmd c =
  match c with
      ASTStat s -> print_stat s
      | ASTdef (d , c) ->
        print_def d; 
        print_cmd c
	
  
let print_prog p =
  Printf.printf("prog([");
  print_cmd p;
  Printf.printf("])")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
