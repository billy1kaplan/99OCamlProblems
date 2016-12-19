open Core.Std

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::tl -> last tl

<<<<<<< HEAD
let f = function
=======
(* CR: use Option.value instead of match *)
let f x = 
  match x with
>>>>>>> 06d0075b0736f76bebabac4bf7917bcf860e40ad
  | Some c -> print_endline c 
  | None -> print_endline "None"

let () = f (last ["a"; "b"; "c"; "d"])
let () = f (last [])
