open Core
module IntSet = CCSet.Make (Int)
module IntTupleSet = CCSet.Make (Advent.IntTuple)

let read_char x y ch =
  match ch with
  | '#' -> Some (x, y)
  | _ -> None
;;

let read_line y line =
  List.filter_mapi ~f:(fun index ch -> read_char index y ch) (String.to_list line)
;;

let read_lines height lines =
  let rec aux empty_rows galaxys y =
    match y with
    | y when y = height -> empty_rows, galaxys
    | _ ->
      let l = List.nth_exn lines y in
      (match read_line y l with
       | [] -> aux (y :: empty_rows) galaxys (y + 1)
       | gs -> aux empty_rows (galaxys @ gs) (y + 1))
  in
  aux [] [] 0
;;

let read_map width height lines =
  let empty_rows, galaxies = read_lines height lines in
  let occupied_cols = List.map ~f:(fun (x, _) -> x) galaxies |> IntSet.of_list in
  let all_cols = List.init width ~f:(fun x -> x) |> IntSet.of_list in
  let empty_cols = IntSet.diff all_cols occupied_cols |> IntSet.to_list in
  empty_rows, empty_cols, galaxies
;;

let push_galaxies_apart empty_rows empty_cols coefficient ~galaxies =
  let rec aux acc list =
    match list with
    | [] -> acc
    | (gx, gy) :: gs ->
      let dy = List.count ~f:(fun r -> r < gy) empty_rows in
      let dx = List.count ~f:(fun r -> r < gx) empty_cols in
      let adjusted = gx + (coefficient * dx), gy + (coefficient * dy) in
      aux (adjusted :: acc) gs
  in
  aux [] galaxies
;;

let manhattan_dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)
let distinct_pairs points = Advent.Lists.combinations 2 points

let sum_distances galaxies =
  distinct_pairs galaxies
  |> List.fold_left ~init:0 ~f:(fun acc pair ->
    match pair with
    | [ start; target ] -> acc + manhattan_dist start target
    | _ -> failwith "found non-pair")
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day11.txt" in
  let height = List.length lines in
  let width = List.nth_exn lines 0 |> String.length in
  let empty_rows, empty_cols, galaxies = read_map width height lines in
  let adjuster = push_galaxies_apart empty_rows empty_cols ~galaxies in
  let adjusted_single = adjuster 1 in
  let adjusted_million = adjuster 999999 in
  let _ = Printf.printf "Part One: %d\n" (sum_distances adjusted_single) in
  let _ = Printf.printf "Part Two: %d\n" (sum_distances adjusted_million) in
  ()
;;
