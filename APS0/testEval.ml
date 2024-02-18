
open Ast

type environnement = binding list 
and binding = Binding of  string * value
and value = 
  Int of int | InF of expr * string list * environnement | InFR of  expr * string * string list * environnement
  | InP of prim
and prim = 
  Add
  | Sub 
  | Eq
  | Lt
  | Mul
  | Div
  | Not

  let add_variables_to_env (argz : string list) (values : value list) (env : environnement) : environnement =
    let rec add_to_env_aux argz vals acc =
      match argz, vals with
      | [], [] -> acc
      | a :: rest_argz, v :: rest_values ->
        let binding = Binding (a, v) in
        add_to_env_aux rest_argz rest_values (binding :: acc)
      | _ -> failwith "Function arguments mismatch"
    in
    add_to_env_aux argz values env


let afficher_environnement (env : environnement) : unit =
  let rec afficher_binding (binding : binding) =
    match binding with
    | Binding (var, value) -> Printf.printf "%s -> %s\n" var (string_of_value value)
  and string_of_value (v : value) : string =
    match v with
    | Int i -> string_of_int i
    | InF (e, sl, _) -> Printf.sprintf "InF(%s, [%s])" (string_of_expr e) (String.concat "; " sl)
    | InFR (e, s1, sl, _) -> Printf.sprintf "InFR(%s, %s, [%s])" (string_of_expr e) s1 (String.concat "; " sl)
    | InP p -> string_of_prim p
  and string_of_expr (e : expr) : string =
    match e with
    | _ -> "expr"
   
  and string_of_prim (p : prim) : string =
    match p with
    | Add -> "Add"
    | Sub -> "Sub"
    | Eq -> "Eq"
    | Lt -> "Lt"
    | Mul -> "Mul"
    | Div -> "Div"
    | Not -> "Not"
  in
  List.iter afficher_binding env



let env_initial = [Binding("x", Int 10); Binding("y", Int 20)] ;;
let variables = ["a"; "b"] ;;
let valeurs = [Int 5; Int 15] ;;
let nouvel_env = add_variables_to_env variables valeurs env_initial;; 
afficher_environnement nouvel_env