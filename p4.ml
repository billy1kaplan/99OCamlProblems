open Core.Std

let rec length ?n:(n=0) = function
  | [] -> n
  | _::tl -> length ~n:(n+1) tl

let () = print_int (length ["a"; "b"; "c"])
let () = print_newline ()
let () = print_int (length [])
let () = print_newline ()

(*Safer version
 * let length list = 
   * let rec aux n = function
     * | [] -> n
     * | _::tl -> aux (n+1) tl
   * in aux 0 list;;
*)
