open Core.Std

let is_prime n =
  let rec aux d =
    d * d > n || (n mod d <> 0 && aux (d+1))
  in
  n <> 1 && aux 2

let goldbach n =
  let rec search d =
    if is_prime d && is_prime (n-d)
    then (d, n-d)
    else search (d+1)
  in
  search 2

let () =
  assert(goldbach 28 = (5, 23))
