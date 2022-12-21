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


(* explode from https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

module CS = Set.Make(Char) ;;

let rec split_rucksack str h1 num_left =
  if num_left == 0 then (h1, (List.fold_right CS.add str CS.empty))
  else
    split_rucksack (List.tl str) (List.fold_right CS.add ([List.hd str]) h1) (num_left - 1)
;;

let split_helper str =
  let half_length = (String.length str) / 2 in
  split_rucksack (explode str) CS.empty half_length;;

let print_set s =
  CS.iter print_char s;;

let singleton_set_intersection (s1,s2) =
  let inters = CS.inter s1 s2 in
  List.hd (CS.elements inters)
;;

let singleton_three_set_inter s1 s2 s3 =
  singleton_set_intersection ((CS.inter s1 s2), s3)
;;

let char_to_priority char =
  let ascii = (Char.code char) - 64 in
  if char == Char.lowercase_ascii char
    then ascii - 32
    else ascii + 26
;;

let explode_and_inter s1 s2 s3 =
  let c1 = (List.fold_right CS.add (explode s1) CS.empty) in
  let c2 = (List.fold_right CS.add (explode s2) CS.empty) in
  let c3 = (List.fold_right CS.add (explode s3) CS.empty) in
  let char = singleton_three_set_inter c1 c2 c3 in
  char_to_priority char
;;

let rec parse_lines_by_threes lines ans =
  match lines with
    | l1::l2::l3::t -> parse_lines_by_threes t ((explode_and_inter l1 l2 l3) + ans)
    | _ -> ans
;;

let p2_sol lines =
  parse_lines_by_threes lines 0;;


let () =
  let lines = read_file "inputs/03.txt" in
  let splits = List.map split_helper lines in
  let inters = List.map singleton_set_intersection splits in
  let sum = List.fold_left (fun acc x -> acc + (char_to_priority x)) 0 inters in
  print_string "Part 1: ";
  print_int sum;
  let p2_ans = p2_sol lines in
  print_string "\nPart 2: ";
  print_int p2_ans;
  print_string "\n"
;;
