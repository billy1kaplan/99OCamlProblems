open Core.Std

let factors n = 
  let l = P33.factors n in
  let rec rle count lst =
    match lst with
    | [] -> []
    | [h] -> [(h,count+1)]
    | h :: (h' :: _ as t) -> 
        if h = h'
        then rle (count+1) t
        else (h,count+1)::rle 0 t
  in rle 0 l

let factors1 n = 
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n/d) with
        | (h,n) :: t when h = d -> (h,n+1) :: t
        | l -> (d,1) :: l
      else aux (d+1) n
  in
  aux 2 n

let () = 
  assert(factors 315 = [(3, 2); (5, 1); (7, 1)]);
  assert(factors1 315 = [(3, 2); (5, 1); (7, 1)])
