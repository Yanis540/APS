(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: eval.ml                                                     == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

type prim = 
  Add
  | Sub 
  | Eq
  | Lt
  | Mul
  | Div
  | Not

 
(*! Types & environnement  *)
type environnement = binding list 
and binding = Binding of  string * value
and value = 
  InZ of int 
  | InF of expr * string list * environnement 
  | InFR of  expr * string * string list * environnement
  | InP of prim

(*! Getters for env  *)

let rec get_ident_value_from_env (ident  : string) (env : environnement)= 
  match env with 
  [] -> failwith "Variable not in env" 
  | (Binding (s,v)):: xs ->
    if (compare ident s) ==  0 then v 
    else get_ident_value_from_env ident xs


let get_int_value (v:value)=
  match v with 
  | InZ(n) -> n 
  | _ -> failwith "Not an integer value" 


let get_bool_value (v:value)=
  match v with 
  | (InZ(0 as n)| InZ(1 as n)) -> 
      n!=0
  | _ -> failwith "Not an boolean value" 


let get_argument_ident (arg) = 
  match arg with 
  Argument (ident,_) -> ident  
  

let rec get_arguments_in_string_list (argz) : (string list) = 
  match argz with 
  |  [] -> []
  |  a::argz' -> 
      (get_argument_ident a)::(get_arguments_in_string_list argz')
      

(* let rec create_arguments_to_env (argz : string list) (values : value list) (env) : environnement =  *)
let add_variables_to_env (argz : string list) (values : value list) (env : environnement) : environnement =
  let rec add_to_env_aux argz vals acc =
    match argz, vals with
    | [], [] -> acc
    | a :: rest_argz, v :: rest_values ->
      let binding = Binding(a, v) in
      add_to_env_aux rest_argz rest_values (binding :: acc)
    | _ -> failwith "Function arguments mismatch"
  in
  add_to_env_aux argz values env

(*! Basic environnement *)

let env0 = [
  Binding ("true" , InZ(1));
  Binding ("false" , InZ(0));
  Binding ("add" , InP (Add));
  Binding ("sub" , InP (Sub));
  Binding ("eq" , InP (Eq));
  Binding ("lt" , InP (Lt));
  Binding ("mul" , InP (Mul));
  Binding ("div" , InP (Div));
  Binding ("not" , InP (Not));
]

let pi_unary p v1  =
  match p,v1 with 
  | InP Not,(InZ(0 as n)|InZ(1 as n)) -> 
    if(n==0) then InZ(1) else InZ(0)
  | _ -> failwith "No unary operation"

  
let pi_binary p v1 v2    =
  match p with 
  | InP Eq -> if get_int_value(v1) == get_int_value(v2) then InZ(1) else InZ(0)
  | InP Lt -> if get_int_value(v1) < get_int_value(v2) then InZ(1) else InZ(0)
  | InP Add -> InZ(get_int_value(v1)+get_int_value(v2))
  | InP Mul -> InZ(get_int_value(v1)*get_int_value(v2))
  | InP Div -> InZ(get_int_value(v1)/get_int_value(v2))
  | _-> failwith "No such binary operation"



(*! Evaluate expression  *)

let rec eval_expr e env= 
  match e with 
  | ASTNum(n) -> InZ(n)
  | ASTId(n) -> get_ident_value_from_env (n) (env)
  | ASTif(cond,cons,alt) -> 
      if get_bool_value(eval_expr cond env ) == true then 
        eval_expr (cons) env 
      else
        eval_expr (alt) env 
  | ASTand(e1,e2) -> 
      if (get_bool_value(eval_expr e1 env) && get_bool_value(eval_expr e2 env)) == true 
      then InZ(1) 
      else InZ(0)
  | ASTor(e1,e2) -> 
      if(get_bool_value(eval_expr e1 env) ||  get_bool_value(eval_expr e2 env))== true 
      then InZ(1) 
      else InZ(0)
  | ASTlambda(argz,e)-> 
    let argz_string = get_arguments_in_string_list (argz) in 
    InF (e,argz_string,env)

  | ASTApp(expr,exprs)->
    let ve:value = eval_expr expr env in 
    let v_i = eval_exprs exprs env  in 
    match ve with
    | InZ (n)-> failwith "Expected function but got integer" 
    | InF (body_function,argz_string,env_function)-> 
      (* e1 .. en sont déjà évalué (représenter par v_i) et on a juste à rajouter dans l'enviornnment vi:valeur(ei)*)
      (* rajouter les couples (var,value) dans l'environnement de la fonction et l'évaluer dans cette environnement *)
      let env_function' = add_variables_to_env (argz_string) (v_i) (env_function) in 
      eval_expr (body_function) (env_function')
      | InFR (body_function,functionName,argz_string,env_function)-> 
        (* rajouter les couples (var,value) dans l'environnement de la fonction et l'évaluer dans cette environnement *)
        (* puis rajouter la définition de la fonction pour permettre les accès récusrif dans le body *)
        let rec_func_def = InFR  (body_function,functionName,argz_string,env_function) in 
        let argz_string_function_rec = functionName::argz_string in 
        let v_i_function_rec = rec_func_def::v_i in 
        let env_function' = add_variables_to_env (argz_string_function_rec) (v_i_function_rec) (env_function) in 
        eval_expr (body_function) (env_function')
        
    | InP _ -> 
      match List.length exprs with 
      | 1 -> pi_unary ve (List.nth v_i 0)
      | 2 -> pi_binary ve (List.nth v_i 0) (List.nth v_i 1)
      | _ -> failwith "No Such arity for primary functions"
    

  


and eval_exprs es env = 
  match es with  
  | []-> [] 
  (* | [e]-> [eval_expr e env]   *)
  | e::es'-> (eval_expr e env)::eval_exprs es' env  
      

let eval_stat s env = 
  match s with 
  | ASTEcho e -> 
    get_int_value(eval_expr e env)



let eval_def d env = 
  match d with 
  | ASTconst(idf,t,e)-> 
    let v = eval_expr e env in 
    let bind = Binding (idf,v) in 
    bind:: env
  | ASTfunc(functionName,t,argz,e)-> 
    let argz_string = get_arguments_in_string_list (argz) in 
    let v = InF (e, argz_string ,env) in 
    let bind = Binding(functionName,v) in 
    bind::env
  | ASTfuncRec(functionName,t,argz,e)-> 
    let argz_string = get_arguments_in_string_list (argz) in 
    let v = InFR ( e, functionName,argz_string, env) in
    let bind = Binding(functionName,v) in 
    bind::env


let rec eval_cmd c env = 
  match c with 
  | ASTStat s -> eval_stat s env 
  | ASTdef (d , c) -> 
    let env' = eval_def (d) env in 
    eval_cmd c env'

let eval_prog p env0= 
  eval_cmd p env0
;;


let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p env0;
  with Lexer.Eof ->
    exit 0
        