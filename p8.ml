open Core.Std

let rec compress = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> compress tl
  | hd :: tl -> hd::compress tl

let () = assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a";"b";"c";"a";"d";"e"])
