(* STL read_file, from https://stackoverflow.com/a/5775024 *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
  ;;

let create_interval str =
  let lst = String.split_on_char '-' str in
  ((int_of_string (List.hd lst)), (int_of_string (List.hd (List.tl lst))))
;;

let does_contain ((s1, e1), (s2, e2)) =
  if s1 <= s2 && e1 >= e2
    then 1
    else
      if s1 >= s2 && e1 <= e2 then 1
      else 0
;;

let does_overlap ((s1, e1), (s2, e2)) =
  if s1 <= s2 && e1 >= s2
    then 1
    else
      if s2 <= s1 && e2 >= s1 then 1
      else 0
;;

let () =
  let lines = read_file "inputs/04.txt" in
  let pairs = List.map (fun x -> String.split_on_char ',' x) lines in
  let intervals = List.map (fun x -> (create_interval (List.hd x)), (create_interval (List.hd (List.tl x)))) pairs in
  let does_contain_sum = List.fold_left (fun acc pair -> acc + (does_contain pair)) 0 intervals in
  print_string "Part 1: ";
  print_int does_contain_sum;
  let does_overlap_sum = List.fold_left (fun acc pair -> acc + (does_overlap pair)) 0 intervals in
  print_string "\nPart 2: ";
  print_int does_overlap_sum;
  print_string "\n"
;;
