open Core
open Advent.Plane
open Advent.Plane.Direction
open Advent.Plane.ArrayGrid

type point_type =
  | Rolling
  | Cube
  | Air
[@@deriving variants]

let sum_load grid =
  let base_load = grid.height in
  Array.filter_mapi
    ~f:(fun idx elt ->
      if is_rolling elt
      then (
        let y = idx / grid.width in
        Some (base_load - y))
      else None)
    grid.points
  |> Array.reduce_exn ~f:( + )
;;

let is_roll_at_boundary grid direction (x, y) =
  match direction with
  | North when y = grid.height -> Some (x + 1, 0)
  | South when y = -1 -> Some (x + 1, grid.height - 1)
  | East when x = -1 -> Some (grid.width - 1, y + 1)
  | West when x = grid.width -> Some (0, y + 1)
  | _ -> None
;;

let is_roll_complete grid direction (x, y) =
  match direction with
  | North | South -> x = grid.width
  | East | West -> y = grid.height
;;

let roll direction grid =
  let should_reset = is_roll_at_boundary grid direction in
  let should_complete = is_roll_complete grid direction in
  let rec aux point limit =
    match should_reset point, should_complete point with
    | _, true -> grid
    | Some p, _ -> aux p p
    | _ ->
      (match ArrayGrid.point_at_xy grid point with
       | Some Rolling ->
         let _ = ArrayGrid.swap_points_mut grid point limit in
         aux (Point.shift_away point direction) (Point.shift_away limit direction)
       | Some Cube ->
         let next = Point.shift_away point direction in
         aux next next
       | _ -> aux (Point.shift_away point direction) limit)
  in
  let start =
    match direction with
    | North -> 0, 0
    | South -> 0, grid.height - 1
    | East -> grid.width - 1, 0
    | West -> 0, 0
  in
  aux start start
;;

let perform_cycles grid num =
  let rec aux remaining =
    match remaining with
    | 0 -> ()
    | _ ->
      let _ = roll North grid |> roll West |> roll South |> roll East in
      aux (remaining - 1)
  in
  aux num
;;

let find_cycle grid expected =
  let rec aux i acc =
    match acc with
    | _ when List.length acc = 2 -> acc
    | _ ->
      perform_cycles grid 1;
      if sum_load grid = expected then aux (i + 1) (i :: acc) else aux (i + 1) acc
  in
  aux 0 []
;;

let read_char ch =
  match ch with
  | 'O' -> Rolling
  | '#' -> Cube
  | _ -> Air
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day14.txt" in
  let grid = ArrayGrid.from_lines lines ~f:read_char in
  (* TODO: hardcoded cycle start number, dynamically find this *)
  let _ =
    Printf.printf "Part one: %d\n%!" (roll North (ArrayGrid.copy grid) |> sum_load)
  in
  (* Part two
     --------

     NB: Lots of mutability here as we're using a 1D array for our point storage, and rather than copying
     all the time we simply apply cycles to the same grid as we go.
  *)
  let cycles = find_cycle grid 102837 |> List.rev in
  let freqs = Advent.Lists.elt_diffs cycles in
  let cycle_frequency =
    match Advent.Lists.all_same ~compare:equal_int freqs with
    | Some freq -> freq
    | None -> failwith "invalid cycle frequency detected"
  in
  let cycle_start = List.last_exn cycles in
  let remainder = (1_000_000_000 - cycle_start) % cycle_frequency in
  perform_cycles grid (remainder - 1);
  let _ =
    Printf.printf
      "Part two: %d [Cycle start at %d, with frequency of %d, leaving a remainder of %d]\n"
      (sum_load grid)
      cycle_start
      cycle_frequency
      remainder
  in
  ()
;;
