open Core.Std

let rec at k = function
  | [] -> None
  | x::tl -> if k = 1 then Some x else at (k-1) tl

let f = function
  | None -> print_endline "None"
  | Some v -> print_endline v

let () = f (at 3 ["a"; "b"; "c"; "d"; "e"])
let () = f (at 3 ["a"])
