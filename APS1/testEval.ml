
open Ast
type prim = 
  Add
  | Sub 
  | Eq
  | Lt
  | Mul
  | Div
  | Not


type environnement = binding list 
and binding = Binding of  string * value
and address = InA of int
and value = 
  InZ of int 
  | InF of expr * string list * environnement 
  | InFR of  expr * string * string list * environnement
  | InP of prim
  | Address  
  | None  
;;

type memory =  memory_element list
and memory_element = Memory of address * value     
;;


(*! Memory allocation *)
let get_memory_address (a:address) = 
  match a with 
  | InA a -> a
;;
let memRef = ref 0 ;; 
let rec get_from_memory (mem:memory) (address: address) : value = 
  match mem with 
  | [] -> failwith "No such address"
  | m::mem' -> 
    match m with 
    Memory(na,v)-> 
      if get_memory_address(na) == get_memory_address(address) then 
        v
      else
        get_from_memory (mem') address
;;

(* alloc(σ) = (a, σ′) *)
let alloc (mem:memory) = 
  let allocation = (InA(!memRef),(Memory(InA(!memRef),None)::mem)) in 
  memRef := !memRef +1; 
  allocation
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






let env = [Binding("x", InZ(10)); Binding("y", InZ(20))]

(* Test de la fonction alloc *)
let test_alloc = 
  let mem = [] in
  let (address, new_mem) = alloc mem in
  (* On s'attend à ce que l'adresse soit égale à 0 car c'est la première allocation *)
  assert (address = InA(0));
  (* On s'attend à ce que la mémoire après allocation contienne une seule entrée avec l'adresse 0 et la valeur None *)
  assert (new_mem = [Memory(InA(0), None)])

(* Test de la fonction update_address_value *)
let test_update_address_value =
  let mem = [Memory(InA(0), InZ(5))] in
  let (old_value, new_mem) = update_address_value mem (InA(0)) (InZ(10)) in
  (* On s'attend à ce que l'ancienne valeur soit Some(InZ(5)) *)
  assert (old_value = InZ(5));
  (* On s'attend à ce que la nouvelle mémoire contienne une seule entrée avec l'adresse 0 et la valeur InZ(10) *)
  assert (new_mem = [Memory(InA(0), InZ(10))])

(* Test de la fonction get_from_memory *)
let test_get_from_memory =
  let mem = [Memory(InA(0), InZ(5)); Memory(InA(1), InZ(10))] in
  let value = get_from_memory (mem) (InA(1)) in
  (* On s'attend à ce que la valeur obtenue soit Some(InZ(10)) *)
  assert (value = InZ(10))

(* Exécution des tests *)
let () =
  test_alloc;
  test_update_address_value;
  test_get_from_memory;
  Printf.printf "Tous les tests ont réussi !\n"