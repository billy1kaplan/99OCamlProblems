open Core.Std

let rec last list =
  match list with 
  [] -> None
  | [last_item] -> Some last_item
  | hd::tl -> last tl
;;

let a = last ["a";"b";"c";"d"];;
let b = last [];;

let f x = 
  match x with
  Some c -> print_string (c ^ "\n")
  | None -> print_string ("None" ^ "\n")
;;

f a;;
f b;;
