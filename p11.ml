(* run-length encoding *)

open Core.Std

type 'a rle = 
  | One of 'a
  | Many of int * 'a

let encode list = 
  let rec aux count = function
    | [] -> [] (* only if initial list is empty *)
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (count+1) tl
    | hd :: tl -> if count = 1 then (One hd)::aux 1 tl else (Many (count, hd))::aux 1 tl
  in aux 1 list

let () = assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")])

