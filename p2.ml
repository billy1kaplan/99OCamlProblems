open Core.Std

let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some (x, y)
  | _::tl -> last_two tl 
;;

let f x = 
  match x with
  | Some (a, b) -> print_endline (a ^ ", " ^ b)
  | None -> print_endline "None"
;;

f (last_two ["a"; "b"; "c"; "d"]);;
f (last_two []);;
