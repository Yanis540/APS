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
      Printf.printf "typeFunc";
      Printf.printf "(";
      Printf.printf "[";
      print_types ts ;  
      Printf.printf "]";
      Printf.printf ",";
      print_typ t;
      Printf.printf ")" 

and print_types ts = 
  match ts with 
  | [] -> () 
  | [t] -> print_typ t
  | t::ts' ->   
    print_typ t; 
    Printf.printf ","  ; 
    print_types ts'

(* ! Printing arguments *)

let print_arg a = 
  match a with 
  | Argument (idf,t)->
    Printf.printf "(" ;  
    Printf.printf "%s" idf ;  
    Printf.printf ",";
    print_typ t;
    Printf.printf ")"


let rec print_args (argz) = 
  match argz with 
  |  [] -> ()
  |  [a] -> print_arg a
  |  a::argz' -> 
      print_arg a;
      Printf.printf ",";
      print_args argz'
  


(* ! Expression  *)
let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTif (c,cns,alt) ->
        Printf.printf "if"; 
        Printf.printf "("; 
        print_expr c; 
        Printf.printf ","; 
        print_expr cns; 
        Printf.printf ","; 
        print_expr alt; 
        Printf.printf ")"

    | ASTand (l,r) ->
        Printf.printf "and";
        Printf.printf "(";
        print_expr l; 
        Printf.printf ","; 
        print_expr r;
        Printf.printf ")"
    | ASTor (l,r) ->
        Printf.printf "or";
        Printf.printf "(";
        print_expr l; 
        Printf.printf ","; 
        print_expr r;
        Printf.printf ")" 
    | ASTApp(e, es) -> (
	      Printf.printf"app(";
	      print_expr e;
	      Printf.printf",[";
	      print_exprs es;
	      Printf.printf"])"
      )
    | ASTlambda(argz, e) -> (
        Printf.printf "lambda"; 
        Printf.printf "("; 
        Printf.printf "[";  
        print_args argz;      
        Printf.printf "]";  
        Printf.printf ",";  
        print_expr e ;
        Printf.printf ")"  
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
      Printf.printf "constant";
      Printf.printf "(";
      Printf.printf "%s" idf;
      Printf.printf ",";
      print_typ t; 
      Printf.printf ",";
      print_expr e;
      Printf.printf ")"
  | ASTfunc (name,t,argz,e) -> 
    Printf.printf "function"; 
    Printf.printf "("; 
    Printf.printf "%s" name; 
    Printf.printf ",";  
    print_typ t;
    Printf.printf ",";  
    Printf.printf "[";        
    print_args argz;      
    Printf.printf "]";        
    Printf.printf ",";        
    print_expr e;
    Printf.printf ")"
  | ASTfuncRec (name,t,argz,e) -> 
    Printf.printf "functionRec"; 
    Printf.printf "("; 
    Printf.printf "%s" name; 
    Printf.printf ",";  
    print_typ t;
    Printf.printf ",";  
    Printf.printf "[";        
    print_args argz;      
    Printf.printf "]";        
    Printf.printf ",";        
    print_expr e;
    Printf.printf ")"
 
  
  
    
let rec print_cmd c =
  match c with
      ASTStat s -> print_stat s
      | ASTdef (d , c) ->
        Printf.printf "declaration";
        Printf.printf "(";
        print_def d; 
        Printf.printf ")";
        Printf.printf ",";
        print_cmd c
      | ASTstatCmds (s , c) ->
        print_stat s; 
        Printf.printf ",";
        print_cmd c
	
let print_block b = 
  match b with 
  | ASTblock cs -> 
    Printf.printf"block"; 
    Printf.printf"(["; 
    print_cmd cs;
    Printf.printf"])" 
  
let print_prog p =
  Printf.printf("prog(");
  print_block p;
  Printf.printf(")")
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
      
