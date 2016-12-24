open Core.Std

let rand_select list n =
  let select_range = 
    List.length list in
  let rec indices n = 
    if n = 0 then [] else (Random.int select_range) :: indices (n-1) in
  List.map ~f:(fun x -> Option.value ~default:"None" (List.nth list x)) (indices n)

let rand_select1 list n = 
  let rec extract base i = function
    | [] -> failwith "Out of range" 
    | hd :: tl ->
        if i = 0 then (hd, base@tl) else extract (hd::base) (i-1) tl
  in 
  let extract_random l = 
    extract [] (Random.int (List.length l)) l
  in 
  let rec choose n acc list = 
    if n = 0 then acc else 
      let picked, rest = extract_random list in
      choose (n-1) (picked :: acc) rest
  in
  choose (min n (List.length list)) [] list

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
