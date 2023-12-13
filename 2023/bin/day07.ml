type card =
  | Ace
  | King
  | Queen
  | Joker
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
[@@deriving ord]

type kind =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
[@@deriving ord]

type hand =
  { kind : kind
  ; cards : card list
  ; bet : string
  }

let is_n_of_a_kind hand n =
  let card_counts =
    List.fold_left
      (fun counts card ->
        let count =
          try counts |> List.assoc card with
          | Not_found -> 0
        in
        (card, count + 1) :: List.remove_assoc card counts)
      []
      (List.init (String.length hand) (String.get hand))
  in
  List.exists (fun (_, count) -> count >= n) card_counts
;;

let is_one_pair hand =
  let card_counts =
    List.fold_left
      (fun counts card ->
        let count =
          try counts |> List.assoc card with
          | Not_found -> 0
        in
        (card, count + 1) :: List.remove_assoc card counts)
      []
      (List.init (String.length hand) (String.get hand))
  in
  List.exists (fun (_, count) -> count >= 2) card_counts
;;

let is_two_pair hand =
  let card_counts =
    List.fold_left
      (fun counts card ->
        let count =
          try counts |> List.assoc card with
          | Not_found -> 0
        in
        (card, count + 1) :: List.remove_assoc card counts)
      []
      (List.init (String.length hand) (String.get hand))
  in
  let pair_counts = List.filter (fun (_, count) -> count >= 2) card_counts in
  List.length pair_counts = 2
;;

let is_full_house hand =
  let card_counts =
    List.fold_left
      (fun counts card ->
        let count =
          try counts |> List.assoc card with
          | Not_found -> 0
        in
        (card, count + 1) :: List.remove_assoc card counts)
      []
      (List.init (String.length hand) (String.get hand))
  in
  let has_three_of_a_kind = List.exists (fun (_, count) -> count >= 3) card_counts in
  let has_pair = List.exists (fun (_, count) -> count = 2) card_counts in
  has_three_of_a_kind && has_pair
;;

let parse_card ch =
  match ch with
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Joker
  | 'T' -> Ten
  | '9' -> Nine
  | '8' -> Eight
  | '7' -> Seven
  | '6' -> Six
  | '5' -> Five
  | '4' -> Four
  | '3' -> Three
  | '2' -> Two
  | _ -> Failure "Invalid card" |> raise
;;

let determine_hand cards =
  match cards with
  | hand when is_n_of_a_kind hand 5 -> FiveOfAKind
  | hand when is_n_of_a_kind hand 4 -> FourOfAKind
  | hand when is_full_house hand -> FullHouse
  | hand when is_n_of_a_kind hand 3 -> ThreeOfAKind
  | hand when is_two_pair hand -> TwoPair
  | hand when is_one_pair hand -> OnePair
  | _ -> HighCard
;;

let rec compare_hand_cards a b =
  match a, b with
  | a :: ass, b :: bs ->
    let c = compare_card a b in
    if c == 0 then compare_hand_cards ass bs else c
  | _ -> raise (Failure "invalid hand")
;;

let compare_hand a b =
  let c = compare_kind a.kind b.kind in
  if c == 0 then compare_hand_cards a.cards b.cards else c
;;

let parse_line line =
  match String.split_on_char ' ' line with
  | [ hand; bet ] ->
    let parsed_hand = List.map parse_card (String.to_seq hand |> List.of_seq) in
    let kind = determine_hand hand in
    { kind; cards = parsed_hand; bet }
  | _ -> Failure "Invalid line" |> raise
;;

let () =
  let lines = Advent.read_lines "./inputs/day07.txt" in
  let hands = List.map parse_line lines in
  let sorted = List.sort compare_hand hands |> List.rev in
  let winnings =
    List.mapi (fun index hand -> (index + 1) * int_of_string hand.bet) sorted
  in
  let total = List.fold_left (fun acc i -> acc + i) 0 winnings in
  Printf.printf "done %d\n" total
;;
