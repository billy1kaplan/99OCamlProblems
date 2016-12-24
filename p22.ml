open Core.Std

let rec range start stop = 
  let op = if stop > start then (+) else (-) in
  if start = stop then [start] else start :: range (op start 1) stop

let () = assert( range 4 9 = [4; 5; 6; 7; 8; 9]);
assert( range 9 4 = [9; 8; 7; 6; 5; 4])
