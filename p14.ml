open Core.Std

let duplicate list = 
  let rec aux base = function
    | [] -> base
    | hd :: tl -> aux (hd::hd::base) tl
  in List.rev(aux [] list)

let () = assert(duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])
