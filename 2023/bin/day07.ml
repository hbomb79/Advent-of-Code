type card =
  | Ace
  | King
  | Queen
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  | Joker
[@@deriving ord, variants]

type kind =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
[@@deriving ord, variants]

type hand =
  { kind : kind
  ; cards : card list
  ; bet : string
  }

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

let get_joker_counts counts =
  let card_counts = List.map (fun (ch, count) -> parse_card ch, count) counts in
  let _, joker_counts =
    try List.find (fun (a, _) -> a = Joker) card_counts with
    | _ -> Joker, 0
  in
  joker_counts
;;

let get_counts hand =
  List.fold_left
    (fun counts card ->
      let count =
        try counts |> List.assoc card with
        | Not_found -> 0
      in
      (card, count + 1) :: List.remove_assoc card counts)
    []
    (List.init (String.length hand) (String.get hand))
;;

let compare_count a b =
  let (_, aCount), (_, bCount) = a, b in
  compare bCount aCount
;;

let determine_kind count second_count =
  match count, second_count with
  | 5, _ -> FiveOfAKind
  | 4, _ -> FourOfAKind
  | 3, 2 -> FullHouse
  | 3, _ -> ThreeOfAKind
  | 2, 2 -> TwoPair
  | 2, _ -> OnePair
  | _ -> HighCard
;;

let determine_hand_jokered cards =
  let card_counts = get_counts cards in
  let cards_sans_joker = List.filter (fun (a, _) -> parse_card a != Joker) card_counts in
  let joker_count = get_joker_counts card_counts in
  let sorted_cards = List.sort compare_count cards_sans_joker in
  match sorted_cards with
  | (_, x) :: (_, y) :: _ -> determine_kind (x + joker_count) y
  | (_, x) :: _ -> determine_kind (x + joker_count) 0
  | _ -> determine_kind joker_count 0
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
    let kind = determine_hand_jokered hand in
    { kind; cards = parsed_hand; bet }
  | _ -> Failure "Invalid line" |> raise
;;

let () =
  let lines = Advent.read_lines "./inputs/day07.txt" in
  let hands = List.map parse_line lines in
  let sorted = List.sort compare_hand hands |> List.rev in
  (* let _ = List.map (fun hand -> print_endline (show_hand hand)) sorted in *)
  let winnings =
    List.mapi (fun index hand -> (index + 1) * int_of_string hand.bet) sorted
  in
  let total = List.fold_left (fun acc i -> acc + i) 0 winnings in
  Printf.printf "%d\n" total
;;
