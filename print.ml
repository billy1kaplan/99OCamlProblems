open Core.Std

(* i for int, s for string *)
let print_list list =
  print_string "[";
  List.iter ~f:(fun x -> printf "%s;" x) list;
  print_endline "]"
