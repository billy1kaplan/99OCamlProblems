open Core.Std

let lotto_select r n =
  let rec picked x = function
    | [] -> false
    | hd :: tl -> if hd = x then true else picked x tl
  in 
  let rec select prev = 
    let rnd = 1 + Random.int n in
    if picked rnd prev then select prev else rnd
  in
  let rec choose r base = 
    if r = 0 then base else choose (r-1) ((select base)::base) 
  in choose r []

let lotto_select1 r n = 
  P23.rand_select1 (P22.range 1 n) r

let conv_list list = 
  List.map ~f:(fun x -> string_of_int x) list

let () = 
  print_endline "Using example lotto_select 6 49";
  print_string "Technique 1: ";
  P23.print_list (conv_list (lotto_select 6 49));
  print_string "Technique 2: ";
  P23.print_list (conv_list (lotto_select1 6 49))
