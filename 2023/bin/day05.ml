(*
   Represents the numerical range for some category, used to determine if a specific
   identifier lands within the range, and for determining the offset to apply to
   the destination range (see mapping)
*)
type range =
  { start : int
  ; stop : int
  }

let range_contains range identifier =
  range.start <= identifier && range.stop >= identifier
;;

(* Returns the identifier after being mapped to the given mapping, or none
   if the mapping's source range does not contain this identifier *)
let apply_mapping identifier mapping =
  let source, dest = mapping in
  if range_contains source identifier
  then Some (dest.start + (identifier - source.start))
  else None
;;

(*
   Given a section (which is a list of ranges) and an identifier which
   corresponds to the 'source' category, this function will return a matching
   'destination' category identifier if found, otherwise the original identifier.
*)
let apply_section identifier section =
  let _, ranges = section in
  let found_ident = List.find_map (apply_mapping identifier) ranges in
  match found_ident with
  | Some i -> i
  | None -> identifier
;;

let apply_sections sections identifier =
  List.fold_left (fun acc section -> apply_section acc section) identifier sections
;;

(*
   Parses the start and length of the source and destination mapping from a given
   list, and returns the range mapping.

   NB: while the puzzle input is destination then source, the data is stored in the reverse order
*)
let read_mapping line =
  match String.split_on_char ' ' line |> List.map int_of_string with
  | [ dst_start; src_start; len ] ->
    ( { start = src_start; stop = src_start + len }
    , { start = dst_start; stop = dst_start + len } )
  | _ -> failwith "invalid mapping"
;;

let read_section lines =
  let title_line = List.nth lines 0 in
  let title = String.split_on_char ' ' title_line in
  let sections = Advent.drop 1 lines |> List.map read_mapping in
  List.nth title 0, sections
;;

let read_seeds line =
  Advent.string_tail line 7 |> String.split_on_char ' ' |> List.map int_of_string
;;

let min_list lst = List.fold_left min (List.hd lst) (List.tl lst)

(* Part One *)
let () =
  let lines = Advent.read_lines "./inputs/day05.txt" in
  let partitioned = Advent.partition_lines lines [] [] in
  let seeds = read_seeds (List.nth (List.nth partitioned 0) 0) in
  let sections = List.map read_section (Advent.drop 1 partitioned) in
  let fully_mapped =
    List.map (fun seed_identifier -> apply_sections sections seed_identifier) seeds
  in
  Printf.printf "Min: %d\n" (min_list fully_mapped)
;;

let rec read_seed_pairs seeds acc =
  match seeds with
  | [] -> acc
  | start :: range :: rest ->
    let exploded = List.init range (fun i -> start + i) in
    read_seed_pairs rest acc @ [ exploded ]
  | _ -> Advent.unreachable ()
;;

(* Part Two *)
(* TODO: Way too naive, does not work *)
let () =
  let lines = Advent.read_lines "./inputs/day05.txt" in
  let partitioned = Advent.partition_lines lines [] [] in
  let seeds = read_seeds (List.nth (List.nth partitioned 0) 0) in
  let _ = List.map (Printf.printf "Seed: %d\n") seeds in
  let all_seeds = read_seed_pairs seeds [] |> List.flatten in
  let _ = List.map (Printf.printf "Exploded seed: %d\n") all_seeds in
  let sections = List.map read_section (Advent.drop 1 partitioned) in
  let fully_mapped =
    List.map (fun seed_identifier -> apply_sections sections seed_identifier) all_seeds
  in
  Printf.printf "Min: %d\n" (min_list fully_mapped)
;;
