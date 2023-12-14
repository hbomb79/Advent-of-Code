open Core
module IntSet = CCSet.Make (Int)

type scratchcard =
  { id : int
  ; winning_cards : IntSet.t
  ; cards : IntSet.t
  }

let int_power base exponent = int_of_float (float_of_int base ** float_of_int exponent)
let matches_to_points matches = 1 * int_power 2 (matches - 1)

let get_card_score numbers winning_numbers =
  let intersection = IntSet.inter numbers winning_numbers in
  let inter_match = IntSet.to_seq intersection |> Seq.length in
  matches_to_points inter_match
;;

let parse_line line =
  let split = String.split ~on:':' line in
  let card_sect = String.split ~on:' ' (List.nth_exn split 0) in
  let card_split = String.split ~on:'|' (List.nth_exn split 1) in
  let winning_cards = List.nth_exn card_split 0 in
  let held_cards = List.nth_exn card_split 1 in
  { id = int_of_string (List.last_exn card_sect)
  ; winning_cards = Advent.Strings.extract_numbers winning_cards |> IntSet.of_list
  ; cards = Advent.Strings.extract_numbers held_cards |> IntSet.of_list
  }
;;

let apply_clones card_quantities bucket_idx matches count =
  List.fold_left
    ~init:card_quantities
    ~f:(fun acc idx ->
      match Stdlib.List.assoc_opt idx acc with
      | Some qty -> (idx, qty + count) :: Stdlib.List.remove_assoc idx acc
      | None -> acc)
    (List.init matches ~f:(fun i -> i + bucket_idx + 1))
;;

let rec clone_wars card_quantities bucket_index card_matches =
  match Stdlib.List.assoc_opt bucket_index card_quantities with
  | Some qty when qty > 0 ->
    let newCardQuantities =
      apply_clones
        card_quantities
        bucket_index
        (Stdlib.List.assoc bucket_index card_matches)
        qty
    in
    clone_wars newCardQuantities (bucket_index + 1) card_matches
  | _ -> card_quantities
;;

let () =
  let scratchcards =
    Advent.Strings.read_lines "./inputs/day04.txt" |> List.map ~f:parse_line
  in
  let p1_score =
    List.map
      ~f:(fun { cards; winning_cards; _ } -> get_card_score cards winning_cards)
      scratchcards
    |> List.reduce_exn ~f:( + )
  in
  let card_quantities = List.init (List.length scratchcards) ~f:(fun i -> i + 1, 1) in
  let p2_score =
    List.map
      ~f:(fun { id; cards; winning_cards; _ } ->
        let matches = IntSet.inter cards winning_cards |> IntSet.to_seq |> Seq.length in
        id, matches)
      scratchcards
    |> clone_wars card_quantities 1
    |> List.fold_left ~f:(fun acc (_, qty) -> acc + qty) ~init:0
  in
  let _ = Printf.printf "Part One: %d\n" p1_score in
  let _ = Printf.printf "Part Two: %d\n" p2_score in
  ()
;;
