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
and address = InA of int
and bloc = InB of address * int 
and value = 
  InZ of int 
  | InF of expr * string list * environnement 
  | InFR of  expr * string * string list * environnement
  | InPrim of prim
  | InAddress of address
  | InP of block * string list * environnement
  | InPR of block * string *  string list * environnement
  | InBloc of bloc 
  | None  
;;

type memory =  memory_element list
and memory_element = Memory of address * value    


let prim_to_string (p:prim) : string = 
  match p with 
  Add -> "Add"
  | Sub -> "Sub"
  | Eq -> "Eq"
  | Lt -> "Lt"
  | Mul -> "Mul"
  | Div -> "Div"
  | Not-> "Not"

;;

let value_to_string (v:value): string = 
  match v with 
  | InZ _ -> "Integer"
  | InF (_,_,_) -> "Function"
  | InFR (_,_,_,_) -> "Function"
  | InPrim p -> (prim_to_string p )
  | InAddress _ -> "Address"
  | InP (_,_,_) -> "Procedure"
  | InPR (_,_,_,_) -> "Procedure"
  | None  -> "None"
;;

(*! Memory allocation *)
let get_memory_address (a:address) = 
  match a with 
  | InA a -> a
;;

let get_memory_address_from_value (v : value) : address = 
  match v with
  | InAddress (a) -> a   
  | _ -> failwith "Not a memory address" 
;;  


let memRef = ref 0 ;; 
let rec get_address_value_from_memory (address: address) (mem:memory) : value   = 
  match mem with 
  | [] -> failwith "No such address"
  | m::mem' -> 
    match m with 
    Memory(na,v)-> 
      if get_memory_address(na) == get_memory_address(address) then 
        v
      else
        get_address_value_from_memory address (mem') 
;;

(* alloc(σ) = (a, σ′) *)
let alloc (mem:memory) = 
  let allocation = (InA(!memRef),(Memory(InA(!memRef),None)::mem)) in 
  memRef := !memRef +1; 
  allocation
;;

(* allocn(σ,n) = (a, σ′) *)
let allocn (mem:memory)(n:int)  = 
  match n<=0 with 
  | True -> failwith "Allocate functions expectes only positif numbers"
  | _ -> 
    let a = InA(!memRef) in 
    let rec  allocate_multiple_to_memory mem n = 
      match n with 
      | 0 -> mem
      | _ ->
        let mem' = (InA(!memRef),(Memory(InA(!memRef),None)::mem)) in 
          memRef := !memRef +1; 
        allocate_multiple_to_memory (mem')
    in
    let mem' =  allocate_multiple_to_memory (mem) (n) in 
    (a,mem')
;;



(* returns : (old_value,new_memory) *)
let update_address_value (mem:memory) (address:address) (v:value) = 
  let rec update_mem_aux mem address v= 
    match mem with 
    | [] -> failwith "address not found"
    | m::mem' -> 
      match m with 
      Memory (na,old_v)-> 
        if get_memory_address(na) == get_memory_address(address) then 
          (old_v,(Memory(na,v)::mem'))
        else 
          let (o,mem'') =  update_mem_aux (mem') (address) (v) in 
          (o,m::mem'')
  in 
    update_mem_aux mem address v

;;

(*@retrurns : (InAddress(address),memory) *)
let init_mem_value (v:value) (mem:memory) : (value*memory) = 
  let (a,mem') = alloc(mem) in 
  let (_,mem'') = update_address_value (mem') (a) (v) in 
  (InAddress(a),mem'')


(* Merci Louic pour la remarque :))) *)
let print_value value =
  match value with
    InZ(n) -> Printf.printf "%d\n" n
  | _ -> failwith "Can't print non integer type"
 
;;

let print_value_from_memory (a:address) (mem :memory)  = 
  let v = get_address_value_from_memory(a) (mem) in 
  print_value v
;;


let print_generic_value (v : value) (mem:memory)= 
  match v with 
  | InAddress a -> print_value_from_memory (a) (mem)
  | _ -> print_value v
;;

(*! Getters for env  *)

let rec get_ident_value_from_env (ident  : string) (env : environnement)  : value= 
  match env with 
  [] -> failwith "Variable not in env" 
  | (Binding (s,v)):: xs ->
      if (compare ident s) ==  0 then v
      else get_ident_value_from_env ident xs 
;;

let get_int_value (v:value)=
  match v with 
  | InZ(n) -> n 
  | _ -> failwith "Not an integer value" 


let get_bool_value (v:value):bool=
  match v with 
  | (InZ(0 as n)| InZ(1 as n)) -> 
      n!=0
  | _ -> failwith "Not a boolean value" 


let get_arg_ident (arg) = 
  match arg with 
  Argument (ident,_) -> ident  
  

let rec get_args_in_string_list (argz) : (string list) = 
  match argz with 
  |  [] -> []
  |  a::argz' -> 
      (get_arg_ident a)::(get_args_in_string_list argz')
;;

let get_argp_ident (argp:argp) : (string) = 
  match argp with 
  | ArgumentP (ident,_) -> ident  
  | ArgumentPA (ident,_) -> ident
;;  
let rec get_argsp_in_string_list (argzp) : (string list) = 
  match argzp with 
  |  [] -> []
  |  a::argz' -> 
      (get_argp_ident a)::(get_argsp_in_string_list argz')
  ;;



(* returns : env *)
let add_variables_to_env (argz : string list) (values : value list) (env : environnement) : (environnement) =
  let rec add_to_env_aux argz vals acc_env  =
    match argz, vals with
    | [], [] -> (acc_env)
    | arg :: rest_argz, v :: rest_values ->
      let binding = Binding(arg, v) in
      let env' = binding :: acc_env in 
      add_to_env_aux rest_argz rest_values (env') 
    | _ -> failwith "Arguments and values mismatch"
  in
  add_to_env_aux argz values env 
;;

let add_variable_to_env (arg : string) (v : value) (env : environnement)  : (environnement) = 
  add_variables_to_env ([arg]) ([v]) (env)
;;


(* returns : (env,mem) *)
let add_variables_mem_to_env (argz : string list) (values : value list) (env : environnement) (mem : memory) : (environnement*memory) =
  let rec add_to_mem_env_aux argz vals acc_env acc_mem =
    match argz, vals with
    | [], [] -> (acc_env,acc_mem)
    | arg :: rest_argz, v :: rest_values ->
      let (address,mem') = init_mem_value (v) (acc_mem) in 
      let binding = Binding(arg, address) in
      let env' = binding :: acc_env in 
      add_to_mem_env_aux rest_argz rest_values (env') (mem')
    | _ -> failwith "Arguments and values mismatch"
  in
  add_to_mem_env_aux argz values env mem


let add_variable_to_mem_env (arg : string) (v : value) (env : environnement) (mem : memory) : (environnement*memory) = 
  add_variables_mem_to_env ([arg]) ([v]) (env) (mem)



(*! Basic environnement *)

let env0 = [
  Binding ("true" , InZ(1));
  Binding ("false" , InZ(0));
  Binding ("add" , InPrim (Add));
  Binding ("sub" , InPrim (Sub));
  Binding ("eq" , InPrim (Eq));
  Binding ("lt" , InPrim (Lt));
  Binding ("mul" , InPrim (Mul));
  Binding ("div" , InPrim (Div));
  Binding ("not" , InPrim (Not));
]

let pi_unary p v1  =
  match p,v1 with 
  | InPrim Not,(InZ(0 as n)|InZ(1 as n)) -> 
    if(n==0) then InZ(1) else InZ(0)
  | _ -> failwith "No unary operation"

  
let pi_binary p v1 v2    =
  match p with 
  | InPrim Add -> InZ(get_int_value(v1)+get_int_value(v2))
  | InPrim Sub -> InZ(get_int_value(v1)-get_int_value(v2))
  | InPrim Eq -> if get_int_value(v1) == get_int_value(v2) then InZ(1) else InZ(0)
  | InPrim Lt -> if get_int_value(v1) < get_int_value(v2) then InZ(1) else InZ(0)
  | InPrim Mul -> InZ(get_int_value(v1)*get_int_value(v2))
  | InPrim Div -> InZ(get_int_value(v1)/get_int_value(v2))
  | _-> failwith "No such binary operation"



(*! Evaluate expression  *)
(*  @returns : (value) *)
let rec eval_expr e env mem= 
  match e with 
  | ASTNum(n) -> InZ(n)
  | ASTId(n) -> 
      let v = get_ident_value_from_env (n) (env) in 
      (
        match v with 
        | InAddress (a) -> get_address_value_from_memory (a) (mem) 
        | v -> v 
      )
 
  | ASTif(cond,cons,alt) -> 
      if get_bool_value(eval_expr cond env mem ) == true then 
        eval_expr (cons) env mem
      else
        eval_expr (alt) env mem
  | ASTand(e1,e2) -> 
      if (get_bool_value(eval_expr e1 env mem) && get_bool_value(eval_expr e2 env mem)) == true 
      then InZ(1) 
      else InZ(0)
  | ASTor(e1,e2) -> 
      if(get_bool_value(eval_expr e1 env mem) ||  get_bool_value(eval_expr e2 env mem))== true 
      then InZ(1) 
      else InZ(0)
  | ASTlambda(argz,e)-> 
    let argz_string = get_args_in_string_list (argz) in 
    InF (e,argz_string,env)

  | ASTApp(expr,exprs)->
    let ve:value = eval_expr expr env mem in 
    let v_i = eval_exprs exprs env mem in 
    match ve with
    | InZ (n)-> InZ(n) (* ça marche je ne sais pas pourquoi *)
    | InF (body_function,argz_string,env_function)-> 
      (* e1 .. en sont déjà évalué (représenter par v_i) et on a juste à rajouter dans l'enviornnment vi:valeur(ei)*)
      (* rajouter les couples (var,value) dans l'environnement de la fonction et l'évaluer dans cette environnement *)
      let env_function' = add_variables_to_env (argz_string) (v_i) (env_function) in 
      eval_expr (body_function) (env_function') (mem)
    | InFR (body_function,functionName,argz_string,env_function)-> 
      (* rajouter les couples (var,value) dans l'environnement de la fonction et l'évaluer dans cette environnement *)
      (* puis rajouter la définition de la fonction pour permettre les accès récusrif dans le body *)
      let rec_func_def = InFR  (body_function,functionName,argz_string,env_function) in 
      let argz_string_function_rec = functionName::argz_string in 
      let v_i_function_rec = rec_func_def::v_i in 
      let env_function' = add_variables_to_env (argz_string_function_rec) (v_i_function_rec) (env_function) in 
      eval_expr (body_function) (env_function') (mem)
    | InPrim _ -> 
      (match List.length exprs with 
      | 1 -> pi_unary ve (List.nth v_i 0)
      | 2 -> pi_binary ve (List.nth v_i 0) (List.nth v_i 1)
      | _ -> failwith "No Such arity for primary functions"
      )
    | v -> failwith ("Expected function but got "^ (value_to_string v))  
   
    

  


and eval_exprs es env mem = 
  match es with  
  | []-> [] 
  (* | [e]-> [eval_expr e env]   *)
  | e::es'-> (eval_expr e env mem)::(eval_exprs es' env mem)
and eval_expar x env mem : value= 
  match x with 
  (* Ref : (adr x ) -> inA(a) *)
  | ASTexpAddress name ->  
    let v = get_ident_value_from_env (name) (env) in 
    (
      match v with 
      | InAddress (a) -> InAddress(a)
      |  _ -> failwith ("Variable "^name^" is type of :  "^ (value_to_string v) ^" But expected Memory") 
    )
  (* Val  *)
  | ASTexpr e ->  eval_expr e env mem      
and eval_exprsp es env mem = 
  match es with 
  | [] -> []
  | e::es' -> 
    let e_i = eval_expar e env mem in 
    (e_i) :: (eval_exprsp es' env mem)
;;

(* @return : (new_memory,new_output) *)
let rec eval_stat s env mem output= 
  match s with 
  | ASTEcho e -> 
      (mem,(eval_expr e env mem )::output)
  | ASTset (varName,e)-> 
      (*! TODO *)
      (* récupérer la valeur de l'env *)
      let value_address = get_ident_value_from_env (varName) (env) in 
      (* récupérer l'address  *)
      let address = get_memory_address_from_value(value_address) in 
      let new_value = eval_expr e env mem  in 
      let (old_value, mem') = update_address_value (mem) (address) (new_value) in 
      (mem',output)
  | ASTif (cond,b_cons,b_alt)-> 
      let cond_value = eval_expr cond env mem in 
      let c  = get_bool_value(cond_value) in 
      if( c == true) then 
        let (mem',output')= eval_block b_cons env mem output in 
        (mem',output')
      else 
        let (mem',output')=eval_block b_alt env mem output in 
        (mem',output')
  | ASTwhile (cond,b_while)-> 
      let cond_value = eval_expr cond env mem in 
      let c  = get_bool_value(cond_value) in 
      if( c == false) then 
        (mem,output)
      else 
        let (mem',output') = eval_block b_while env mem output in  
        let (mem'',output'' ) = eval_stat s env mem' output' in 
        (mem'',output'')
  | ASTcall (name,exprsp)-> 
      let v = get_ident_value_from_env (name) (env) in 
      let v_i = eval_exprsp exprsp env mem in 
      match v with 
      | InP(body_proc,argz_string,env_proc) ->
        let env_proc' = add_variables_to_env (argz_string) (v_i) (env_proc) in 
        let (mem',output') = eval_block (body_proc) (env_proc') (mem) (output) in 
        (mem',output')
      | InPR(body_proc,procName,argz_string,env_proc) ->
        let rec_proc_def = InPR  (body_proc,procName,argz_string,env_proc) in 
        let argz_string_proc_rec = procName::argz_string in 
        let v_i_proc_rec = rec_proc_def::v_i in 
        let env_proc' = add_variables_to_env (argz_string_proc_rec) (v_i_proc_rec) (env_proc) in 
        let (mem',output') = eval_block (body_proc) (env_proc') (mem) (output) in 
        (mem',output')
      | _ -> failwith "Expected procedure but receieved other type"
    


(* @retruns : (new_env,new_mem) *)
and eval_def d env mem = 
  match d with 
  | ASTconst(idf,t,e)-> 
    let v = eval_expr e env mem in 
    let bind = Binding (idf,v) in 
    let env' = (bind :: env) in 
    (env',mem)
  | ASTfunc(functionName,t,argz,e)-> 
    let argz_string = get_args_in_string_list (argz) in 
    let v = InF (e, argz_string ,env) in 
    let bind = Binding(functionName,v) in 
    let env' = (bind :: env) in 
    (env',mem)
  | ASTfuncRec(functionName,t,argz,e)-> 
    let argz_string = get_args_in_string_list (argz) in 
    let v = InFR ( e, functionName,argz_string, env) in
    let bind = Binding(functionName,v) in 
    let env' = (bind :: env) in 
    (env',mem)
  | ASTvar(name,t)-> 
    let (address,mem') = init_mem_value (None) (mem) in 
    let bind = Binding(name,address) in
    let env' = (bind :: env) in  
    (env',mem')
  | ASTproc(name,argz,b)-> 
    let argz_string = get_argsp_in_string_list (argz) in 
    let v= InP(b,argz_string,env) in 
    let binding = Binding(name,v) in 
    let env' =(binding::env) in 
    (env',mem)
  | ASTprocRec(name,argz,b)->
    let argz_string = get_argsp_in_string_list (argz) in 
    let v= InPR(b,name,argz_string,env) in 
    let binding = Binding(name,v) in 
    let env' =(binding::env) in 
    (env',mem)

(* @returns : (mem,output) *)
and eval_cmd c env mem output = 
  match c with 
  | ASTStat s -> eval_stat s env mem output
  | ASTdef (d , c) -> 
    let (env',mem') = eval_def d env mem in 
    eval_cmd c env' mem' output
  | ASTstatCmds (s , c) -> 
    let (mem',output') = eval_stat s env  mem output in 
    let (mem'',output'') = eval_cmd c env mem' output' in 
    (mem'',output'')
(* @returns : (new_mem,new_output) *)
and eval_block b env mem output = 
  match b with 
  | ASTblock cs -> eval_cmd cs env mem output
  

let rec print_output mem output =
  List.iter (function v -> print_generic_value v mem) (List.rev output) 

let eval_prog p env0= 
  let (mem',output') =  eval_block p env0 [] [] in 
  print_output (mem')(output')
;;


let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p env0;
      Printf.printf "eval\n"
  with Lexer.Eof ->
    exit 0
        