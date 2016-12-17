open Core.Std

let rev list = 
  let rec aux reversed = function
    | [] -> reversed
    | hd::tl -> aux (hd::reversed) tl
  in aux [] list


let print_list list = 
  let print_el el = print_string (el ^ ";") in 
  List.iter ~f:print_el list;
  print_endline ""

let () = print_list (rev ["a"; "b"; "c"; "d"; "e"])
