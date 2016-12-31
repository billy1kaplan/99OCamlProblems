open Core.Std
open P23

let l = ["a";"b";"c";"d";"e";"f";"g";"h"]
let range  = (List.length l) + 1
let print_list lst = 
  print_string "[";
  List.iter ~f:(fun x -> printf "%s;" x) lst;
  print_endline "]"

let () =
  print_string "Using example list: ";
  print_list l;
  printf "Sample size? %!";
  match In_channel.input_line stdin with
  | None -> failwith "Must provide sample size"
  | Some n ->
      for _  = 1 to int_of_string(n) do
        let size = Random.int range in
        printf "Random size: %i " size;
        print_string "With replacement: ";
        print_list (rand_select l size);
        print_string "Without replacement: ";
        print_list (rand_select1 l size)
      done
