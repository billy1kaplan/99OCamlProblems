open Core.Std

let rec gcd n1 n2 = 
  if n2 = 0
  then n1
  else gcd n2 (n1 mod n2)

let () = assert(gcd 13 27 = 1);
assert(gcd 20536 7826 = 2)
