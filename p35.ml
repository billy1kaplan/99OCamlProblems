open Core.Std

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
      let b = pow a (n/2) in
      b * b * (if n mod 2 = 0 then 1 else a)

let phi_improved n = 
  let rec aux acc = function
    | []         -> acc
    | (p,m) :: t -> aux ((p-1)*(pow p (m-1))*acc) t
  in aux 1 (P34.factors1 n)
  
let () = 
  assert(phi_improved 10 = 4);
  assert(phi_improved 13 = 12)
