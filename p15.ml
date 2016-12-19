open Core.Std

let replicate list n = 
  let rec repeat a x acc = 
    if x = 0 then acc else repeat a (x-1) (a::acc) in
  let rec walk acc = function
    | [] -> acc
    | hd :: tl -> walk (repeat hd n acc) tl in
  walk [] (List.rev list)

let () = assert(replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])
