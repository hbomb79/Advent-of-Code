module IntSet = Set.Make (Int)

type scratchcard =
  { id : int
  ; winning_cards : int list
  ; cards : int list
  }

let int_power base exponent = int_of_float (float_of_int base ** float_of_int exponent)

let extract_int_list s =
  let rec extract_tokens acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      (match int_of_string_opt hd with
       | Some num -> extract_tokens (num :: acc) tl
       | None -> extract_tokens acc tl)
  in
  String.split_on_char ' ' s |> extract_tokens []
;;

let matches_to_points matches =
  match matches with
  | 0 -> 0
  | 1 -> 1
  | n -> 1 * int_power 2 (n - 1)
;;

let get_card_score numbers winning_numbers =
  let intersection = IntSet.inter numbers winning_numbers in
  let inter_match = IntSet.to_seq intersection |> Seq.length in
  matches_to_points inter_match
;;

(*
   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
*)
let parse_line line =
  let split = String.split_on_char ':' line in
  let card_sect = String.split_on_char ' ' (List.nth split 0) in
  let lst = String.trim (List.nth card_sect (-1 + List.length card_sect)) in
  let card_id = int_of_string lst in
  let card_split = String.split_on_char '|' (List.nth split 1) in
  let winning_cards = List.nth card_split 0 in
  let held_cards = List.nth card_split 1 in
  { id = card_id
  ; winning_cards = extract_int_list winning_cards
  ; cards = extract_int_list held_cards
  }
;;

(* Part One *)
let () =
  let lines = Advent.read_lines "./inputs/day04.txt" in
  let cards = List.map parse_line lines in
  let results =
    List.map
      (fun { id; cards; winning_cards; _ } ->
        let cards_set = IntSet.of_list cards in
        let winning_cards_set = IntSet.of_list winning_cards in
        id, get_card_score cards_set winning_cards_set)
      cards
  in
  let _ =
    Printf.printf
      "Part One: Cards have score %d\n"
      (List.fold_left (fun acc (_, x) -> acc + x) 0 results)
  in
  ()
;;

let apply_clones card_quantities bucket_idx matches count =
  List.fold_left
    (fun acc idx ->
      match List.assoc_opt idx acc with
      | Some qty -> (idx, qty + count) :: List.remove_assoc idx acc
      | None -> acc)
    card_quantities
    (List.init matches (fun i -> i + bucket_idx + 1))
;;

(*
   Takes the cards in play (including the hands), and the number of each card that
   the player has (starts at 1 and is increased when a card wins, causing clones of itself).

   Say a card is checked, number 9 - this card may have 5 clones of itself, if card nine
   has 2 numbers in common with the winning numbers, then 5 clones of 10 and 11 are spawned (number of
   cards for the current card dictates quantity, and number of matches dictates 'reach').
*)
let rec clone_wars card_matches card_quantities bucket_index =
  match List.assoc_opt bucket_index card_quantities with
  | Some qty when qty > 0 ->
    let newCardQuantities =
      apply_clones card_quantities bucket_index (List.assoc bucket_index card_matches) qty
    in
    clone_wars card_matches newCardQuantities (bucket_index + 1)
  | _ -> card_quantities
;;

(* Part Two *)
let () =
  let lines = Advent.read_lines "./inputs/day04.txt" in
  let cards = List.map parse_line lines in
  let card_quantities = List.init (List.length cards) (fun i -> i + 1, 1) in
  let results =
    List.map
      (fun { id; cards; winning_cards; _ } ->
        let cards_set = IntSet.of_list cards in
        let winning_cards_set = IntSet.of_list winning_cards in
        let matches =
          IntSet.inter cards_set winning_cards_set |> IntSet.to_seq |> Seq.length
        in
        id, matches)
      cards
  in
  let cloned_quantities = clone_wars results card_quantities 1 in
  let total = List.fold_left (fun acc (_, qty) -> acc + qty) 0 cloned_quantities in
  let _ = Printf.printf "Part Two: TOTAL %d\n" total in
  ()
;;
