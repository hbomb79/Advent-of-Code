open Core

type rock =
  | Rounded
  | Solid
[@@deriving variants]

module IntTuple = struct
  type t = int * int [@@deriving compare]
end

module IntTupleMap = CCMap.Make (IntTuple)

type grid =
  { width : int
  ; height : int
  ; points : rock IntTupleMap.t
  }

let sum_load grid =
  let base_load = grid.height in
  List.map
    ~f:(fun ((_, py), _) -> base_load - py)
    (IntTupleMap.filter (fun _ t -> is_rounded t) grid.points |> IntTupleMap.to_list)
  |> List.reduce_exn ~f:( + )
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

let roll direction grid =
  let should_reset = is_roll_at_boundary grid direction in
  let should_complete = is_roll_complete grid direction in
  let rec aux acc point limit =
    match should_reset point, should_complete point with
    | _, true -> { grid with points = acc }
    | Some (px, py), _ -> aux acc (px, py) (px, py)
    | _ ->
      (match IntTupleMap.get point grid.points with
       | Some Rounded ->
         let new_acc = IntTupleMap.remove point acc in
         aux
           (IntTupleMap.add limit Rounded new_acc)
           (advance_point point direction)
           (advance_point limit direction)
       | Some Solid ->
         let next = advance_point point direction in
         aux acc next next
       | None -> aux acc (advance_point point direction) limit)
  in
  let start =
    match direction with
    | North -> 0, 0
    | South -> 0, grid.height - 1
    | East -> grid.width - 1, 0
    | West -> 0, 0
  in
  aux grid.points start start
;;

let perform_cycles grid num =
  let rec aux remaining acc =
    match remaining with
    | 0 -> acc
    | _ ->
      let cycled = roll North acc |> roll West |> roll South |> roll East in
      aux (remaining - 1) cycled
  in
  aux num grid
;;

let find_cycle grid expected =
  let rec aux i acc g =
    match acc with
    | _ when List.length acc = 2 -> acc, g
    | _ ->
      let cycled = perform_cycles g 1 in
      if sum_load cycled = expected
      then aux (i + 1) (i :: acc) cycled
      else aux (i + 1) acc cycled
  in
  aux 0 [] grid
;;

let read_char ch =
  match ch with
  | 'O' -> Some Rounded
  | '#' -> Some Solid
  | _ -> None
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day14.txt" in
  let width = List.nth_exn lines 0 |> String.length in
  let points = Advent.Strings.read_chars_as_grid lines ~f:read_char in
  let grid = { width; height = List.length lines; points = IntTupleMap.of_list points } in
  (* TODO: hardcoded cycle start number, dynamically find this *)
  let _ = Printf.printf "Part one: %d\n" (roll North grid |> sum_load) in
  (* Part two *)
  let cycles, g = find_cycle grid 102837 in
  let freqs = Advent.Lists.elt_diffs (List.rev cycles) in
  let cycle_frequency =
    let fst = List.hd_exn freqs in
    if List.for_all ~f:(equal_int fst) freqs
    then fst
    else failwith "invalid cycle frequency detected"
  in
  let cycle_start = List.last_exn cycles in
  (* we know that starting at 'cycle_start', the pattern repeats itself every 'cycle_frequency'. Therefore
     we can skip a HUGE number of iterations by checking how many cycles remain from the given 1_000_000_000 after we
     subtract the start and take the remainder after dividing by the freq *)
  let remainder = (1_000_000_000 - cycle_start) % cycle_frequency in
  let compl = perform_cycles g (remainder - 1) in
  let _ =
    Printf.printf
      "Part two: %d [Cycle start at %d, with frequency of %d, leaving a remainder of %d]\n"
      (sum_load compl)
      cycle_start
      cycle_frequency
      ((1_000_000_000 - cycle_start) % cycle_frequency)
  in
  ()
;;
