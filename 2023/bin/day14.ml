open Core

type point_type =
  | Rolling
  | Cube
  | Air
[@@deriving variants]

module IntTuple = struct
  type t = int * int [@@deriving compare]
end

module IntTupleMap = CCMap.Make (IntTuple)

type grid =
  { width : int
  ; height : int
  ; points : point_type array
  }

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

type direction =
  | North
  | East
  | South
  | West

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

let advance_point (x, y) direction =
  match direction with
  | North -> x, y + 1
  | South -> x, y - 1
  | East -> x - 1, y
  | West -> x + 1, y
;;

let get_point_at grid (x, y) =
  let idx = (y * grid.width) + x in
  grid.points.(idx)
;;

let swap_points grid (x1, y1) (x2, y2) =
  let idx1 = (y1 * grid.width) + x1 in
  let idx2 = (y2 * grid.width) + x2 in
  let p1 = grid.points.(idx1) in
  let p2 = grid.points.(idx2) in
  let _ = grid.points.(idx2) <- p1 in
  let _ = grid.points.(idx1) <- p2 in
  ()
;;

let roll direction grid =
  let should_reset = is_roll_at_boundary grid direction in
  let should_complete = is_roll_complete grid direction in
  let rec aux point limit =
    match should_reset point, should_complete point with
    | _, true -> grid
    | Some p, _ -> aux p p
    | _ ->
      (match get_point_at grid point with
       | Rolling ->
         let _ = swap_points grid point limit in
         aux (advance_point point direction) (advance_point limit direction)
       | Cube ->
         let next = advance_point point direction in
         aux next next
       | _ -> aux (advance_point point direction) limit)
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
  let width = List.nth_exn lines 0 |> String.length in
  let points = Advent.Strings.read_chars_as_grid lines ~f:read_char in
  let grid =
    { width
    ; height = List.length lines
    ; points = Array.of_list points |> Array.map ~f:(fun (_, r) -> r)
    }
  in
  (* TODO: hardcoded cycle start number, dynamically find this *)
  let _ =
    Printf.printf
      "Part one: %d\n%!"
      (roll North { grid with points = Array.copy grid.points } |> sum_load)
  in
  (* Part two
     --------

     NB: Lots of mutability here as we're using a 1D array for our point storage, and rather than copying
     all the time we simply apply cycles to the same grid as we go.
  *)
  let cycles = find_cycle grid 102837 in
  let freqs = Advent.Lists.elt_diffs (List.rev cycles) in
  let cycle_frequency =
    let fst = List.hd_exn freqs in
    if List.for_all ~f:(equal_int fst) freqs
    then fst
    else failwith "invalid cycle frequency detected"
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
      ((1_000_000_000 - cycle_start) % cycle_frequency)
  in
  ()
;;
