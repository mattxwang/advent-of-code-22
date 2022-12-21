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

type move = Rock | Paper | Scissors
type outcome = Loss | Draw | Win

let char_to_move char =
  match char with
    | 'A' -> Rock
    | 'X' -> Rock
    | 'B' -> Paper
    | 'Y' -> Paper
    | _ -> Scissors
;;

let char_to_outcome char =
  match char with
    | 'X' -> Loss
    | 'Y' -> Draw
    | _ -> Win
;;

let move_score move =
  match move with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
;;

let outcome_score outcome =
  match outcome with
    | Loss -> 0
    | Draw -> 3
    | Win -> 6

let input_beats move =
  match move with
   | Rock -> Scissors
   | Paper -> Rock
   | Scissors -> Paper
;;

let moves_to_outcome m1 m2 =
  if m1 == m2 then Draw
  else if input_beats m1 == m2 then Win else Loss
;;

let calculate_score move outcome =
  (move_score move) + (outcome_score outcome)
;;

let get_move_from_outcome m2 outcome =
  match outcome with
    | Draw -> m2
    | Win -> (match m2 with
        | Scissors -> Rock
        | Rock -> Paper
        | Paper -> Scissors
      )
    | Loss -> input_beats m2
;;
(* explode from https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

let line_to_score_1 line =
  match explode line with
    | x2 :: ' ' :: x1 :: _ ->
      let m1 = char_to_move x1 in
      let m2 = char_to_move x2 in
      let outcome = moves_to_outcome m1 m2 in
      calculate_score m1 outcome
    | _ -> -1
;;

let line_to_score_2 line =
  match explode line with
    | x2 :: ' ' :: x1 :: _ ->
      let m2 = char_to_move x2 in
      let outcome = char_to_outcome x1 in
      let m1 = get_move_from_outcome m2 outcome in
      calculate_score m1 outcome
    | _ -> -1
;;


let () =
  let lines = read_file "inputs/02.txt" in
  let scores_1 = List.map line_to_score_1 lines in
  let sum_1 = List.fold_left (fun acc x -> acc + x) 0 scores_1 in
  print_string "Part 1: ";
  print_int sum_1;
  let scores_2 = List.map line_to_score_2 lines in
  let sum_2 = List.fold_left (fun acc x -> acc + x) 0 scores_2 in
  print_string "\nPart 2: ";
  print_int sum_2;
  print_string "\n"
;;
