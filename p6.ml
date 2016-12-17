open Core.Std

let is_palindrome list = 
  let rec aux reversed = function 
    | [] -> reversed
    | hd :: tl -> aux (hd::reversed) tl
  in aux [] list = list

let is_palindrome_2 list = 
  list = List.rev list

let print_case list = 
  let print_el el = print_string (el ^ ";") in
  List.iter ~f:print_el list;
  print_endline "";
  let res = is_palindrome list in 
  if res then print_string "True" else print_string "False";
  print_endline ""

let () = print_case [ "x" ; "a" ; "m" ; "a" ; "x" ]
let () = print_case [ "x" ; "a" ; "m" ; "a" ]
