open Core.Std

let goldbach_list low high =
  let low = low + (if low mod 2 <> 0 then 1 else 0) in
  let rec aux cur =
    if cur > high
    then []
    else (cur, P37.goldbach cur) :: aux (cur+2)
  in aux low

let goldbach_limit low high min_limit =
  let low = low + (if low mod 2 <> 0 then 1 else 0) in
  let rec aux cur =
    if cur > high
    then []
    else 
      let a,b = P37.goldbach cur in
      if a > min_limit && b > min_limit
      then (cur, (a,b)) :: aux (cur+2)
      else aux (cur+2)
  in aux low

let goldbach_limit1 low high min_limit =
  List.filter ~f:(fun (_, (a,b)) -> a > min_limit && b > min_limit) (goldbach_list low high)

let () =
  assert(goldbach_list 9 20 = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
   (20, (3, 17))]);
   assert(goldbach_limit 1 2000 50 = [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
    (1928, (61, 1867))]);
   assert(goldbach_limit1 1 2000 50 = [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
    (1928, (61, 1867))])
