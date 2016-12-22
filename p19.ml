open Core.Std

let rotate list n = 
  let len = List.length list in
  let index = if n < 0 then len - ((-n) mod len) else n mod len in
  let rec aux base x = function
    | [] -> List.rev base
    | hd :: tl as l -> if x = index then l@(List.rev base) else aux (hd::base) (x+1) tl
  in aux [] 0 list

let () = assert(rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"])
let () = assert(rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"])
