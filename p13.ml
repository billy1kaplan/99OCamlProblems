open Core.Std

type 'a rle = 
  | One of 'a
  | Many of int * 'a

let encode list = 
  let rec aux count res = function
    | [] -> res
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (count+1) res tl
    | hd :: tl -> if count = 1 then aux 1 ((One hd)::res) tl else aux 1 ((Many(count,hd))::res) tl
  in List.rev (aux 1 [] list)

let () = assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")])
