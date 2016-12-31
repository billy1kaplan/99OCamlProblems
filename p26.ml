open Core.Std

let extract k list = 
  let rec aux k cur acc = function
    | [] -> List.map ~f:List.rev acc
    | h :: t -> 
        if k = 1 then aux k cur ((h::cur)::acc) t else
          aux k cur (aux (k-1) (h::cur) acc t) t
  in 
  aux k [] [] list

let check list1 list2 =
  let find ~f:f list =
    is_some (List.find ~f:f list)
  in  
  let rec contains items lst =
    match items with
    | [] -> true
    | hd :: tl -> if find ~f:((=) hd) lst then contains tl lst else false
  in
  let rec aux list1 list2 = 
    match list1 with
    | [] -> true
    | hd :: tl -> if find ~f:(contains hd) list2 then aux tl list2
    else false
  in (&&) (aux list1 list2) (aux list2 list1)

let () = 
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true);
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = false);
  assert(check (extract 2 ["a";"b";"c";"d"]) 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true)

