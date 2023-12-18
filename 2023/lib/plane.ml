(* Module for the creation and manipulation of points on a grid *)
open Core

module Direction = struct
  type t =
    | North
    | West
    | South
    | East
  [@@deriving show, compare]

  let opposite d =
    match d with
    | North -> South
    | South -> North
    | East -> West
    | West -> East
  ;;

  let is_vertical dir =
    match dir with
    | North | South -> true
    | _ -> false
  ;;

  let is_horizontal dir =
    match dir with
    | East | West -> true
    | _ -> false
  ;;

  let rotate_clockwise dir =
    match dir with
    | North -> East
    | South -> West
    | East -> North
    | West -> South
  ;;

  let rotate_anticlockwise dir =
    match dir with
    | North -> West
    | South -> East
    | East -> South
    | West -> North
  ;;
end

module Point = struct
  open Direction

  type t = int * int

  let shift_towards (x, y) direction =
    match direction with
    | North -> x, y - 1
    | South -> x, y + 1
    | East -> x + 1, y
    | West -> x - 1, y
  ;;

  let shift_away (x, y) direction = shift_towards (x, y) (Direction.opposite direction)
end

module ArrayGrid = struct
  type 'a t =
    { width : int
    ; height : int
    ; points : 'a array
    }

  let point_at_xy grid (x, y) =
    if x < 0 || y < 0 || x >= grid.width || y >= grid.height
    then None
    else (
      let idx = (y * grid.width) + x in
      Some grid.points.(idx))
  ;;

  let point_at_idx grid idx = grid.points.(idx)
  let copy grid = { grid with points = Array.copy grid.points }
  let from_points points = List.map ~f:(fun (_, v) -> v) points

  let swap_points_mut grid (x1, y1) (x2, y2) =
    let idx1 = (y1 * grid.width) + x1 in
    let idx2 = (y2 * grid.width) + x2 in
    let p1 = grid.points.(idx1) in
    let p2 = grid.points.(idx2) in
    let _ = grid.points.(idx2) <- p1 in
    let _ = grid.points.(idx1) <- p2 in
    ()
  ;;

  let swap_points grid a b =
    swap_points_mut { grid with points = Array.copy grid.points } a b
  ;;

  let from_lines ~f lines =
    let points = Strings.read_chars_as_grid lines ~f |> from_points |> Array.of_list in
    { width = List.nth_exn lines 0 |> String.length; height = List.length lines; points }
  ;;

  let width grid = grid.width
  let height grid = grid.height
end

module Grid = struct
  type 'a t =
    { width : int
    ; height : int
    ; points : 'a list
    }

  let point_at_xy grid (x, y) =
    if x < 0 || y < 0 || x >= grid.width || y >= grid.height
    then None
    else (
      let idx = (y * grid.width) + x in
      List.nth grid.points idx)
  ;;

  let point_at_idx grid idx = List.nth grid.points idx
  let from_points points = List.map ~f:(fun (_, v) -> v) points

  let from_lines ~f lines =
    let points = Strings.read_chars_as_grid lines ~f |> from_points in
    { width = List.nth_exn lines 0 |> String.length; height = List.length lines; points }
  ;;

  let width grid = grid.width
  let height grid = grid.height
end
