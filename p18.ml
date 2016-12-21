open Core.Std

let slice list start stop = 
  let rec walk index = function 
    | [] -> failwith "Expected start index in range"
    | ( _ :: tl ) as l -> if index = start then l else walk (index+1) tl
  in 
  let rec capture i base = function
    | [] -> List.rev base
    | hd :: tl -> if i > stop then List.rev base else capture (i+1) (hd::base) tl
  in capture start [] (walk 0 list)

let () = assert( slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"])
let () = assert( slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 15 = 
  ["c";"d";"e";"f";"g";"h";"i";"j"])
