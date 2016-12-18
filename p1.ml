open Core.Std

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::tl -> last tl

(* CR: use Option.value instead of match *)
let f x = 
  match x with
  | Some c -> print_endline c 
  | None -> print_endline "None"

let () = f (last ["a"; "b"; "c"; "d"])
let () = f (last [])
