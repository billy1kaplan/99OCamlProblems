open Core.Std

let factors n = 
  let rec count x n  = 
    if n = 1 
    then []
    else 
      if n mod x = 0 then x :: count x (n/x) else
        count (x+1) n
  in count 2 n

let () = assert(factors 315 = [3;3;5;7])
