open Advent.Ranges
(* type section = string * (range * range) list [@@deriving show] *)

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
    ( { start = src_start; stop = src_start + len - 1 }
    , { start = dst_start; stop = dst_start + len - 1 } )
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

let rec read_seed_ranges seeds acc =
  match seeds with
  | [] -> acc
  | start :: range :: rest ->
    read_seed_ranges rest (acc @ [ { start; stop = start + range - 1 } ])
  | _ -> Advent.unreachable ()
;;

(*
   Takes a list of ranges to apply to the section. This function iterates over
   each range in the section, and tries to apply the given identifier ranges
*)
let apply_rangewise_section
  (identifier_ranges : range list)
  (section_ranges : (range * range) list)
  =
  List.fold_left
    (fun id_ranges (source, dest) ->
      List.map
        (fun identifier_range ->
          let out = process_ranges identifier_range source dest in
          let _ =
            Printf.printf
              "Processed\n\
              \    identifier range %s\n\
              \    source range %s\n\
              \    dest range %s\n\
               Out:\n\
               %s\n\n"
              (show_range identifier_range)
              (show_range source)
              (show_range dest)
              (List.map show_range out |> String.concat "\n")
          in
          out)
        id_ranges
      |> List.flatten)
    identifier_ranges
    section_ranges
;;

(*
   Given a list of ranges and a section (which itself has a list of ranges), this function
   will apply each range to each section, splitting the ranges as required. The new split
   ranges are then returned for use with subsequent calculation
*)
let process_sections ranges section =
  List.fold_left apply_rangewise_section ranges section
;;

(* Part Two *)
let () =
  let lines = Advent.read_lines "./inputs/day05-test.txt" in
  let partitioned = Advent.partition_lines lines [] [] in
  let seeds = read_seeds (List.nth (List.nth partitioned 0) 0) in
  let _ = List.map (Printf.printf "Seed: %d\n") seeds in
  let all_seeds = read_seed_ranges seeds [] in
  let _ =
    List.map
      (fun { start; stop } -> Printf.printf "Seed ranges: %d %d\n" start stop)
      all_seeds
  in
  let sections = List.map read_section (Advent.drop 1 partitioned) in
  let mapped =
    process_sections all_seeds (List.map (fun (_, x) -> x) sections)
    |> List.sort (fun a b -> compare a.start b.start)
  in
  let _ = Printf.printf " Mapped ranges: \n" in
  let _ = List.map (fun r -> Printf.printf " - Range: %s\n" (show_range r)) mapped in
  let lowest = List.hd mapped in
  let _ = Printf.printf "Lowest: %d\n" lowest.start in
  ()
;;

(* incorrect 149524933 too high *)
(* incorrect 5872218 too low *)
