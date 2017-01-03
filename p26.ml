open Core.Std

(* CR: don't use "list" as a variable name since it conflicts with the keyword list *)
let extract k list = 
  let rec aux k cur acc = function
    | [] -> List.map ~f:List.rev acc
    | h :: t -> 
        (* CR:
            if ...
            then ...
            else ...
            unless it's all one line
            if ... then ... else ... *)
        (* CR: dam *)
        if k = 1 then aux k cur ((h::cur)::acc) t else
          aux k cur (aux (k-1) (h::cur) acc t) t
  in 
  aux k [] [] list

(* CR: a lot here can be rewritten to be less complex and shorter, since the function passing becomes confusing 
   like find and contains can be combined into one shorter function, since that's how you're using it anyways *)
let check list1 list2 =
  (* CR: List.mem 
      val mem : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> bool
      or List.exists
      val exists : 'a t -> f:('a -> bool) -> bool
      instead of find *)
  (* CR: only need ~f, not ~f:f *)
  (* CR: name isn't very appropirate since you're not finding anything, you're checking existential property *)
  let find ~f:f list =
    is_some (List.find ~f:f list)
  in  
  let rec contains items lst =
    (* CR: List.fold with List.mem
       val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
       something like
       List.fold items ~f:(acc item ->
         if acc then List.mem item else false) *)
    match items with
    | [] -> true
    | hd :: tl -> if find ~f:((=) hd) lst then contains tl lst else false
  in
  (* CR: doesn't this fail to find duplicates (unless you don't care about that) since you're only
     checking is everything in list1 is in list 2? *)
  let rec aux list1 list2 = 
    match list1 with
    | [] -> true
    | hd :: tl -> if find ~f:(contains hd) list2 then aux tl list2
    else false
  (* CR: (aux list1 list2) && (aux list2 list1) *)
  in (&&) (aux list1 list2) (aux list2 list1)

let () = 
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true);
  assert(check [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] 
  [["c"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = false);
  assert(check (extract 2 ["a";"b";"c";"d"]) 
  [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]] = true)

