open Core.Std

let is_prime n =
  let rec aux i =
    if i * i > n
    then true
    else if n mod i = 0
    then false
    else aux (i+1)
  in 
  aux 2 && n <> 1

  (* cleaner way to write the same thing *)
let is_prime1 n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n <> 1 && is_not_divisor 2;;

let () = assert(is_prime 7 = true);
assert(is_prime 1 = false);
assert(is_prime 12 = false)
