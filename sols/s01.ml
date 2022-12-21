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

let rec count_calories input ans =
  match input with
    | [] -> ans
    | hd :: tl -> count_calories tl (match hd with
     | "" -> 0 :: ans
     | s -> int_of_string hd + (List.hd ans) :: List.tl ans)
;;

let sum_first_three list =
  match list with
   | (h1::h2::h3::tl) -> h1 + h2 + h3
   | _ -> -1
;;

let () =
  let lines = read_file "inputs/01.txt" in
  let calorie_counts = count_calories lines [0] in
  let inv_compare = (fun x y -> ~- (compare x y)) in
  let sorted = List.sort inv_compare calorie_counts in
  print_string "Part 1: ";
  print_int (List.hd sorted);
  print_string "\nPart 2: ";
  print_int (sum_first_three sorted);
  print_string "\n"
;;
