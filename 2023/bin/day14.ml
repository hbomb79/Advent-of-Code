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

let read_char ch =
  match ch with
  | 'O' -> Some Rounded
  | '#' -> Some Solid
  | _ -> None
;;

let read_line y line =
  List.filter_mapi
    ~f:(fun x ch -> read_char ch |> Option.map ~f:(fun r -> (x, y), r))
    (String.to_list line)
;;

let read_lines height lines =
  let rec aux points y =
    match y with
    | y when y = height -> points
    | _ ->
      let l = List.nth_exn lines y in
      aux (points @ read_line y l) (y + 1)
  in
  aux [] 0
;;

let sum_load grid =
  let base_load = grid.height in
  List.map
    ~f:(fun ((_, py), _) -> base_load - py)
    (IntTupleMap.filter (fun _ t -> is_rounded t) grid.points |> IntTupleMap.to_list)
  |> List.reduce_exn ~f:( + )
;;

(* let print_grid grid = *)
(*   let ps = IntTupleMap.to_list grid.points in *)
(*   let num_of_rolling_rocks = *)
(*     List.filter ~f:(fun (_, t) -> is_rounded t) ps |> List.length *)
(*   in *)
(*   let num_of_solid = List.filter ~f:(fun ((_, _), t) -> is_solid t) ps |> List.length in *)
(*   let _ = *)
(*     Printf.printf *)
(*       "Rounded: %d -- Solid: %d -- North Load: %d\n" *)
(*       num_of_rolling_rocks *)
(*       num_of_solid *)
(*       (sum_load grid) *)
(*   in *)
(*   for y = 0 to grid.height - 1 do *)
(*     for x = 0 to grid.width - 1 do *)
(*       let ch = IntTupleMap.get (x, y) grid.points in *)
(*       let s = *)
(*         match ch with *)
(*         | Some Rounded -> 'O' *)
(*         | Some Solid -> '#' *)
(*         | None -> '.' *)
(*       in *)
(*       Printf.printf "%c" s *)
(*     done; *)
(*     Printf.printf "\n" *)
(*   done *)
(* ;; *)

(* TODO these duplicated-ish methods make me feel bad... improve this *)

let roll_north grid =
  let rec aux acc x y min_y =
    if y = grid.height
    then aux acc (x + 1) 0 0
    else if x = grid.width
    then { grid with points = acc }
    else (
      match IntTupleMap.get (x, y) grid.points with
      | Some Rounded ->
        let new_acc = IntTupleMap.remove (x, y) acc in
        aux (IntTupleMap.add (x, min_y) Rounded new_acc) x (y + 1) (min_y + 1)
      | Some Solid -> aux acc x (y + 1) (y + 1)
      | None -> aux acc x (y + 1) min_y)
  in
  aux grid.points 0 0 0
;;

let roll_west grid =
  let rec aux acc x y min_x =
    if x = grid.width
    then aux acc 0 (y + 1) 0
    else if y = grid.height
    then { grid with points = acc }
    else (
      match IntTupleMap.get (x, y) grid.points with
      | Some Rounded ->
        let new_acc = IntTupleMap.remove (x, y) acc in
        aux (IntTupleMap.add (min_x, y) Rounded new_acc) (x + 1) y (min_x + 1)
      | Some Solid -> aux acc (x + 1) y (x + 1)
      | None -> aux acc (x + 1) y min_x)
  in
  aux grid.points 0 0 0
;;

let roll_east grid =
  let rec aux acc x y max_x =
    if x = -1
    then aux acc (grid.width - 1) (y + 1) (grid.width - 1)
    else if y = grid.height
    then { grid with points = acc }
    else (
      match IntTupleMap.get (x, y) grid.points with
      | Some Rounded ->
        let new_acc = IntTupleMap.remove (x, y) acc in
        aux (IntTupleMap.add (max_x, y) Rounded new_acc) (x - 1) y (max_x - 1)
      | Some Solid -> aux acc (x - 1) y (x - 1)
      | None -> aux acc (x - 1) y max_x)
  in
  aux grid.points (grid.width - 1) 0 (grid.width - 1)
;;

let roll_south grid =
  let rec aux acc x y max_y =
    if y = -1
    then aux acc (x + 1) (grid.height - 1) (grid.height - 1)
    else if x = grid.width
    then { grid with points = acc }
    else (
      match IntTupleMap.get (x, y) grid.points with
      | Some Rounded ->
        let new_acc = IntTupleMap.remove (x, y) acc in
        aux (IntTupleMap.add (x, max_y) Rounded new_acc) x (y - 1) (max_y - 1)
      | Some Solid -> aux acc x (y - 1) (y - 1)
      | None -> aux acc x (y - 1) max_y)
  in
  aux grid.points 0 (grid.height - 1) (grid.height - 1)
;;

let perform_cycles grid num =
  let rec aux remaining acc =
    match remaining with
    | 0 -> acc
    | _ ->
      let cycled = roll_north acc |> roll_west |> roll_south |> roll_east in
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

let calc_diffs = List.fold_left ~init:[] ~f:(fun acc (a, b) -> acc @ [ a - b ])

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day14.txt" in
  let width = List.nth_exn lines 0 |> String.length in
  let points = read_lines (List.length lines) lines in
  let grid = { width; height = List.length lines; points = IntTupleMap.of_list points } in
  (* TODO: hardcoded cycle start number, dynamically find this *)
  let _ = Printf.printf "Part one: %d\n" (roll_north grid |> sum_load) in
  (* Part two *)
  let cycles, g = find_cycle grid 102837 in
  let freqs = calc_diffs (Advent.Lists.paired cycles) in
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
