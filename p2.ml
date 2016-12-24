open Core.Std

let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some (x, y)
  | _::tl -> last_two tl

let f x = 
  Option.value_map  x ~default:"None" ~f:(fun x -> 
  let (a,b) = x in (a^", "^b))
  |> print_endline

(* XCR: indentation wrong
       use semicolon instead of multiple main statements *)
let () = f (last_two ["a"; "b"; "c"; "d"]);
         f (last_two [])
