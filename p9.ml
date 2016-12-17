open Core.Std

let pack list = 
  let rec aux base build = function
    | [] -> build
    | [x] -> (x::base)::build
    | hd :: (md :: _ as tl) when hd = md -> aux (md::base) build tl
    | hd :: tl -> aux [] ((hd::base)::build) tl
  in List.rev (aux [] [] list)

let () = assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]])
