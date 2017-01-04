open Core.Std

let extract k lst = 
  let rec aux k cur acc = function
    | [] ->  acc
    | h :: t -> 
        if k = 1 
        then aux k cur ((h::cur)::acc) t 
        else aux k cur (aux (k-1) (h::cur) acc t) t
  in 
  List.map ~f:List.rev (aux k [] [] lst)

  (* Check lengths to take care of possible duplicates issue,
   * doesn't explicitly check for duplicates but assumes properly formed test case *)
let check list1 list2 =
  List.length list1 = List.length list2 &&
  (List.fold ~init:true ~f:(fun acc item -> 
    if acc then List.mem list2 item else false) list1)

let () = 
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true);
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = false);
  assert(check (extract 2 ["a";"b";"c";"d"]) 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true)
