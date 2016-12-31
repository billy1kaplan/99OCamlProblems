open Core.Std

let permutation list = 
  P23.rand_select1 list (List.length list)

let () = 
  let l = ["a"; "b"; "c"; "d"; "e"; "f"] in
  print_string "Using sample list: ";
  Print.print_list l;
  Print.print_list (permutation l);
  Print.print_list (permutation l);
  Print.print_list (permutation l);
  Print.print_list (permutation l)
