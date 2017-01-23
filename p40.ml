open Core.Std

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval2 mappings = function
  | Var s             -> List.Assoc.find_exn mappings s
  | Not expr1         -> not(eval2 mappings expr1)
  | And(expr1, expr2) -> eval2 mappings expr1 && eval2 mappings expr2
  | Or(expr1, expr2)  -> eval2 mappings expr1 || eval2 mappings expr2

let table mapping value_of_expr =
  let rec aux cur acc values =
    match values with
      | []     -> (cur,(eval2 cur value_of_expr))::acc
      | h :: t ->
          aux ((h,false)::cur) (aux ((h,true)::cur) acc t) t
  in
  aux [] [] mapping

let check ~f_mem list1 list2 =
  List.length list1 = List.length list2 &&
  (List.fold ~init:true ~f:(fun acc item ->
    if acc then f_mem list2 item else false) list1)

let rec mem_item lst (a,b) =
  match lst with
  | []     -> false
  | (bools, cur) :: t ->
      if (check ~f_mem:(List.mem ?equal:None) bools a) && cur = b
      then true
      else mem_item t (a, b)

let check_correct list1 list2 =
  check ~f_mem:mem_item list1 list2

let () =
  assert((check_correct
  [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
   ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
  [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
   ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
  ) = true);
  assert(check_correct [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
   ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
     (table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")))) = true);
  assert((check_correct (let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))))) 
    [([("a", true); ("b", true); ("c", true)], true);
     ([("a", true); ("b", true); ("c", false)], true);
     ([("a", true); ("b", false); ("c", true)], true);
     ([("a", true); ("b", false); ("c", false)], false);
     ([("a", false); ("b", true); ("c", true)], false);
     ([("a", false); ("b", true); ("c", false)], false);
     ([("a", false); ("b", false); ("c", true)], false);
     ([("a", false); ("b", false); ("c", false)], false)]) = true)
