open Core.Std

let drop list n = 
  let rec aux base count = function
    | [] -> base
    | hd :: tl -> if count=n then aux base 1 tl else aux (hd::base) (count+1) tl
  in List.rev (aux [] 1 list)


let () = assert(drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])
