open Core

type category =
  | Ash
  | Rock
[@@deriving hash, compare]

module Point = struct
  type t = int * int * category [@@deriving hash, compare]
end

type points = Point.t list [@@deriving hash, compare]

module IntTupleSet = CCSet.Make (Point)

type grid =
  { width : int
  ; height : int
  ; points : IntTupleSet.t
  }

type axis =
  | X of int
  | Y of int

(*
   Returns a set of points on one side of the given plane. The side from which the points are taken
   depends on how close the axis is to the edge of the map (e.g. an axis defined on the Y plane which
   is closer to 0 than it is to the right-edge of the grid will take points from the _left_ of the axis).
*)
let get_test_points grid axis =
  let filter_predicate =
    match axis with
    | X axis_x when grid.width - axis_x - 1 > axis_x -> fun (x, _, _) -> x <= axis_x
    | X axis_x -> fun (x, _, _) -> x > axis_x
    | Y axis_y when grid.height - axis_y - 1 > axis_y -> fun (_, y, _) -> y <= axis_y
    | Y axis_y -> fun (_, y, _) -> y > axis_y
  in
  IntTupleSet.filter filter_predicate grid.points
;;

(* Reflects the given point such that it sits on the other side of the
   axis provided.

   NB: Axis specifies only one int, however this represents a line which
   sits between this point and the next point, so special rules are applied
   to the points nearest to the axis
*)
let reflect_point axis (x, y, cat) =
  match axis with
  | X axis_x when x = axis_x -> x + 1, y, cat
  | X axis_x when x = axis_x + 1 -> x - 1, y, cat
  | X axis_x ->
    let diff = x - axis_x in
    x - (diff * 2) + 1, y, cat
  | Y axis_y when y = axis_y -> x, y + 1, cat
  | Y axis_y when y = axis_y + 1 -> x, y - 1, cat
  | Y axis_y ->
    let diff = y - axis_y in
    x, y - (diff * 2) + 1, cat
;;

let is_sym grid axis =
  let points = get_test_points grid axis |> IntTupleSet.map (reflect_point axis) in
  IntTupleSet.fold
    (fun reflected acc ->
      if IntTupleSet.mem reflected grid.points then acc else reflected :: acc)
    points
    []
;;

let find_sym ~pred grid =
  let x = List.init (grid.width - 1) ~f:(fun x -> X x) in
  let y = List.init (grid.height - 1) ~f:(fun y -> Y y) in
  match List.find ~f:(pred grid) (x @ y) with
  | Some axis -> axis
  | _ -> failwith "No axis of symmetry found for this grid."
;;

let read_char x y ch =
  match ch with
  | '#' -> x, y, Rock
  | '.' -> x, y, Ash
  | _ -> failwith "invalid char"
;;

let read_line y line =
  List.mapi ~f:(fun index ch -> read_char index y ch) (String.to_list line)
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

let read_grid lines =
  let width = List.nth_exn lines 0 |> String.length in
  let height = List.length lines in
  let points = read_lines height lines |> IntTupleSet.of_list in
  { width; height; points }
;;

let sum_syms ~pred =
  List.fold ~init:0 ~f:(fun acc g ->
    match find_sym ~pred g with
    | X x -> acc + x + 1
    | Y y -> acc + (100 * (1 + y)))
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day13.txt" in
  let groups = Advent.Lists.partition_lines lines [] [] in
  let grids = List.map ~f:read_grid groups in
  let result =
    sum_syms ~pred:(fun grid axis -> is_sym grid axis |> List.length = 0) grids
  in
  let altresult =
    sum_syms ~pred:(fun grid axis -> is_sym grid axis |> List.length = 1) grids
  in
  let _ = Printf.printf "Part one: %d\n" result in
  let _ = Printf.printf "Part two: %d\n" altresult in
  ()
;;
