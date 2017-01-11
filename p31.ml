open Core.Std

let coprime n1 n2 =
  P30.gcd n1 n2 = 1

let () = assert(coprime 13 27 = true);
assert(coprime 20536 7826 = false)
