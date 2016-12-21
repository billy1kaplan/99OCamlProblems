open Core.Std

let split list n = 
  let rec aux first second x = function
    | [] -> (List.rev first, List.rev second)
    | hd :: tl -> if x < n then aux (hd::first) second (x+1) tl else aux first (hd::second) x tl
  in aux [] [] 0 list

let () = assert( split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =    
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]))

let () = assert(split ["a";"b";"c";"d"] 5 = 
  (["a"; "b"; "c"; "d"], []))

(* Better solution *)
let split1 list n = 
  let rec aux first x = function
    | [] -> (List.rev first,[])
    | (hd :: tl) as l -> if x < n then aux (hd::first) (x+1) tl else (List.rev first, l)
  in aux [] 0 list

let () = assert( split1 ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =    
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]))

let () = assert(split1 ["a";"b";"c";"d"] 5 = 
  (["a"; "b"; "c"; "d"], []))
