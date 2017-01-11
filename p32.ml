open Core.Std
open P31

let phi n = 
  let rec aux k = 
    if k < 1
    then 0
    else
      if coprime k n 
      then 1 + aux (k-1)
      else aux (k-1)
  in 
  if n = 1 then 1 else aux n

let phi1 n =
  let rec count_coprime acc d = 
    if d < n then 
      count_coprime (
        if coprime n d
        then acc + 1
        else acc) (d + 1)
    else acc
  in 
  if n = 1 then 1 else count_coprime 0 1;;

let () = assert(phi 10 = 4);
assert(phi 13 = 12)
