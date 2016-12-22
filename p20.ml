open Core.Std

let rec remove_at i = function
    | [] -> []
    | hd :: tl -> if i = 0 then tl else hd :: remove_at (i-1) tl


let () = assert( remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"] )
