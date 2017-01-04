open Core.Std

let group list sizes = 
  let rec aux k base cur acc unused list sizes = 
    match list,sizes with
    | [],_ -> acc
    | h :: t,[] -> aux k base cur (((h::base)::cur)::acc) [] t sizes
    | h :: t, i :: rest -> 
        if k = 1 
        then let f = 
          aux i [] ((h::base)::cur) acc [] (t@unused) rest in
        aux k base cur f (h :: unused) t sizes else
          aux k base cur (aux (k-1) (h::base) cur acc unused t sizes) (h::unused) t sizes
        in
  let pop = function
    | [] -> (0,[])
    | h :: t -> (h,t)
  in 
  let (k,sizes) = pop sizes
  in
  List.map ~f:(fun x -> List.rev (List.map ~f:(List.rev) x)) (aux k [] [] [] [] list sizes)

let check list1 list2 =
  let f lst x = 
    List.mem lst x ||
    List.mem lst (List.rev x) in
  List.fold ~init:true ~f:(fun acc item ->
    if acc then f list1 item else false) list2

let () = assert(check (group ["a";"b";"c";"d"] [2;1]) [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] = true)
