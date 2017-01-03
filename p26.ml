open Core.Std

(* CR: don't use "list" as a variable name since it conflicts with the keyword list *)
let extract k lst = 
  let rec aux k cur acc = function
    | [] ->  acc
    | h :: t -> 
        if k = 1 
        then aux k cur ((h::cur)::acc) t 
        else aux k cur (aux (k-1) (h::cur) acc t) t
  in 
  List.map ~f:List.rev (aux k [] [] lst)

let check list1 list2 =
  List.fold ~init:true ~f:(fun acc item -> 
    if acc then List.mem list2 item else false) list1;;

let () = 
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true);
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = false);
  assert(check (extract 2 ["a";"b";"c";"d"]) 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true)
