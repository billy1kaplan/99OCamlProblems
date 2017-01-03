open Core.Std

let length_sort list =
  let mapped = List.map ~f:(fun x -> List.length x,x) list
  in
  let sorted = List.map ~f:snd (List.sort ~cmp:compare mapped)
  in sorted

let () = assert(length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; 
                              ["d";"e"];["i";"j";"k";"l"]; ["m";"n"]; ["o"] ] =
                                [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; 
                                ["f"; "g"; "h"]; ["i"; "j"; "k"; "l"]])
     
