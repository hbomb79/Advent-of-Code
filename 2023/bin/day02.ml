type game_round =
  { r : int
  ; g : int
  ; b : int
  }

let parse_gems acc count =
  match count with
  | n, "red" -> { r = acc.r + n; g = acc.g; b = acc.b }
  | n, "green" -> { r = acc.r; g = acc.g + n; b = acc.b }
  | n, "blue" -> { r = acc.r; g = acc.g; b = acc.b + n }
  | _ -> acc
;;

let parse_round round =
  let split = String.split_on_char ',' round |> List.map String.trim in
  let split_again =
    List.map
      (fun gem ->
        let split = String.split_on_char ' ' gem in
        List.hd split |> int_of_string, List.rev split |> List.hd)
      split
  in
  List.fold_left parse_gems { r = 0; g = 0; b = 0 } split_again
;;

let parse_rounds rounds = List.map parse_round rounds

let parse_game line =
  let split = String.split_on_char ':' line in
  let game_id =
    String.split_on_char ' ' (split |> List.hd) |> List.rev |> List.hd |> int_of_string
  in
  let rounds = split |> List.rev |> List.hd in
  let rounds_list = String.split_on_char ';' rounds in
  let parsed_rounds = List.map String.trim rounds_list |> parse_rounds in
  game_id, parsed_rounds
;;

let round_possible round constraints =
  round.r <= constraints.r && round.g <= constraints.g && round.b <= constraints.b
;;

let rec rounds_possible rounds constraints =
  match rounds with
  | [] -> true
  | r :: rs when round_possible r constraints -> rounds_possible rs constraints
  | _ -> false
;;

let rec rounds_minimum rounds acc =
  match rounds with
  | [] -> acc
  | round :: rs ->
    rounds_minimum
      rs
      { r = max round.r acc.r; g = max round.g acc.g; b = max round.b acc.b }
;;

(* Part One *)
let () =
  let lines = Advent.read_lines "./inputs/day02.txt" in
  let games = List.map parse_game lines in
  let constraints = { r = 12; g = 13; b = 14 } in
  let f acc game rounds =
    if rounds_possible rounds constraints then acc + game else acc
  in
  let valid = List.fold_left (fun acc (id, g) -> f acc id g) 0 games in
  Printf.printf "Part one: Sum of IDs: %d\n" valid
;;

(* Part Two *)
let () =
  let lines = Advent.read_lines "./inputs/day02.txt" in
  let games = List.map parse_game lines in
  let minimum = List.map (fun (_, g) -> rounds_minimum g { r = 0; g = 0; b = 0 }) games in
  let powers = List.map (fun min -> min.r * min.g * min.b) minimum in
  let result = List.fold_left (fun acc x -> acc + x) 0 powers in
  Printf.printf "Part two: Sum of cube powers: %d\n" result
;;
