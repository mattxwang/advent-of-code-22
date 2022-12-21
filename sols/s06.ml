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

let rec sublist k xs = match xs with
  | x::xs -> if k=1 then [x] else x :: sublist (k-1) xs
  | _ -> failwith "oopsie"
;;

module CS = Set.Make(Char) ;;

let uniq lst num =
  CS.cardinal (List.fold_right CS.add lst CS.empty) == num

let rec find_diff lst num index =
  if (uniq (sublist num lst) num) then index
  else find_diff (List.tl lst) num (index + 1)

let helper line num = find_diff (explode line) num num;;

let () =
  let lines = read_file "inputs/06.txt" in
  let line = List.hd lines in
  print_string "Part 1: ";
  print_int (helper line 4);
  print_string "\nPart 2: ";
  print_int (helper line 14);
  print_string "\n"
;;
