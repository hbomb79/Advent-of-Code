open Advent.Iter
open Advent.Strings
open Advent.Math

type direction =
  | Left
  | Right

module NodeMap = CCMap.Make (String)

let parse_node line =
  let label = String.sub line 0 3 in
  let left = String.sub line 7 3 in
  let right = String.sub line 12 3 in
  label, (left, right)
;;

let parse_direction ch =
  match ch with
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "invalid character"
;;

let parse_directions line =
  String.to_seq line
  |> List.of_seq
  |> List.map parse_direction
  |> create_infinite_iterator
;;

let rec count_steps directions nodes count current target =
  let direction = directions () in
  match current with
  | c when c = target -> count
  | _ ->
    let newCurrent =
      match List.assoc current nodes, direction with
      | (l, _), Left -> l
      | (_, r), Right -> r
    in
    count_steps directions nodes (count + 1) newCurrent target
;;

(* Part One *)
let () =
  let lines = read_lines "./inputs/day08.txt" in
  let directions = parse_directions (List.nth lines 0) in
  let nodes = List.map parse_node (Core.List.drop lines 2) in
  let steps = count_steps directions nodes 0 "AAA" "ZZZ" in
  let _ = Printf.printf "Part One: %d\n" steps in
  ()
;;

(* Finds the number of iterations required for the given directions and starting
   position to 'cycle' when finding a tile ending in 'Z' *)
let rec find_ghost_cycle directions nMap count current acc =
  match acc with
  | [ a; b ] -> b - a
  | _ ->
    let direction = directions () in
    let newAcc = if String.get current 2 = 'Z' then acc @ [ count ] else acc in
    let newCurrent =
      match NodeMap.get current nMap, direction with
      | Some (l, _), Left -> l
      | Some (_, r), Right -> r
      | _ -> failwith "unreachable"
    in
    find_ghost_cycle directions nMap (count + 1) newCurrent newAcc
;;

(* Part Two

   The number of iterations required for each ghost to land on a 'Z' tile follows a cyclic pattern. By extracting
   the length of this pattern for EACH ghost, and calculating the lowest common multiplier for all of the lengths,
   we can find how many iterations it would take for all ghosts to simulatneously land on 'Z' tiles
*)
let () =
  let lines = read_lines "./inputs/day08.txt" in
  let directions = parse_directions (List.nth lines 0) in
  let nodes = List.map parse_node (Core.List.drop lines 2) in
  let nMap = NodeMap.of_list nodes in
  let ghost_cycle_frequency =
    List.filter (fun (label, _) -> String.ends_with ~suffix:"A" label) nodes
    |> List.map (fun (n, _) -> n)
    |> List.map (fun start -> find_ghost_cycle directions nMap 0 start [])
  in
  let cycles_lcm =
    List.fold_left
      (fun acc n -> lcm acc n)
      (List.nth ghost_cycle_frequency 0)
      (Core.List.drop ghost_cycle_frequency 1)
  in
  let _ = Printf.printf "LCM of cycles: %d\n" cycles_lcm in
  ()
;;
