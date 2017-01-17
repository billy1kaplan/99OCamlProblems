open Core.Std

let timeit f n = 
  let t0 = Unix.gettimeofday() in
  ignore(f n);
  let t1 = Unix.gettimeofday() in
  t1 -. t0

let () = 
  print_float (timeit P32.phi 100900);
  print_endline "";
  print_float (timeit P35.phi_improved 100900)
