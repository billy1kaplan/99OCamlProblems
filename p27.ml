open Core.Std

let group list sizes = 
  let rec aux k base cur acc unused list sizes = 
    match list,sizes with
    | [],_ -> acc
    | h :: t,[] -> aux k base cur (((h::base)::cur)::acc) [] t sizes
    | h :: t, i :: rest -> 
        if k = 1 then
          let f = (aux i [] ((h::base)::cur) acc [] (t@unused) rest) in
          aux k base cur f (h :: unused) t sizes else
            aux k base cur (aux (k-1) (h::base) cur acc unused t sizes) (h::unused) t sizes
          in
  let pop = function
    | [] -> (0,[])
    | h :: t -> (h,t)
  in 
  let (k,sizes) = pop sizes
  in
  aux  k [] [] [] [] list sizes

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
  in 
  let confirm list1 list2 =
   (&&) (aux list1 list2) (aux list2 list1)
  in
  let rec aux1 list1 list2 =  
    match list1 with
    | [] -> true
    | hd :: tl -> if find ~f:(confirm hd) list2 then aux1 tl list2
    else false
  in (&&) (aux1 list1 list2) (aux1 list2 list1)

let () = assert(check (group ["a";"b";"c";"d"] [2;1]) [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] = true)
