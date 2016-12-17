open Core.Std

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten nodes = 
  let rec aux base = function 
    | [] -> base
    | One a::tl -> aux (base@[a]) tl
    | Many entries::tl -> aux (aux base entries) tl
  in aux [] nodes

let print_list list = 
  List.iter ~f:print_string list;
  print_endline ""

let () = print_list (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])
