open Core.Std

let rec insert_at x index = function
  | [] -> [x]
  | hd :: tl as l -> if index = 0 then x::l else hd :: insert_at x (index - 1) tl

let () = assert(insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]);
assert(insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);
assert(insert_at "alfa" 4 ["a";"b";"c";"d"] =  ["a"; "b"; "c"; "d"; "alfa"]);
assert(insert_at "alfa" 10 ["a";"b";"c";"d"] =  ["a"; "b"; "c"; "d"; "alfa"])
