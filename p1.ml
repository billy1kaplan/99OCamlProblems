open Core.Std

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::tl -> last tl

let f x =
  Option.value x ~default:"None"
  |> print_endline

let () = f (last ["a"; "b"; "c"; "d"])
let () = f (last [])
