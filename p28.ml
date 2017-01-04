open Core.Std

let length_sort lst =
  let mapped = List.map ~f:(fun x -> List.length x,x) lst in
  List.map ~f:snd (List.sort ~cmp:compare mapped)
  

let frequency_sort lst =
  let counts =
    let rec aux cur base count = function
      | [] -> base
      | [x] -> (count + 1,(x::cur))::base
      | hd :: (hd' :: _ as tl) -> 
          if List.length hd = List.length hd'
          then aux (hd::cur) base (count + 1) tl
          else aux [] ((count + 1,(hd::cur))::base) 0 tl
    in List.rev (aux [] [] 0 (length_sort lst)) 
  in
  List.fold ~init:[] ~f:(fun acc x -> acc@(List.rev (snd x))) (List.sort ~cmp:compare counts)

let () = assert(length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; 
                              ["d";"e"];["i";"j";"k";"l"]; ["m";"n"]; ["o"] ] =
                                [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; 
                                ["f"; "g"; "h"]; ["i"; "j"; "k"; "l"]]);
         assert(frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                            ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ] = 
                              [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; 
                              ["d"; "e"]; ["d"; "e"]; ["m"; "n"]])
