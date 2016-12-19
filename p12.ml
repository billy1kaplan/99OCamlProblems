open Core.Std

type 'a rle = 
  | One of 'a
  | Many of int * 'a

let rec decode list = 
  let rec repeat acc n a = 
    if n = 0 then acc else repeat (a::acc) (n-1) a in 
  match list with
  | [] -> []
  | (One a) :: tl -> a::decode tl
  | (Many (n, a)) :: tl -> (repeat [] n a)@decode tl

let () = assert( decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
 = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
