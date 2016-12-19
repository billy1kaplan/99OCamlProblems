(* run-length encoding *)

open Core.Std

let encode list = 
  let rec aux count = function
    | [] -> [] (* only if initial list is empty *)
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (count+1) tl
    | hd :: tl -> (count, hd)::aux 1 tl
  in aux 1 list

let () = assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])

