(* Module for the creation and manipulation of points on a grid *)
open Core

module Direction = struct
  type t =
    | North
    | West
    | South
    | East

  let opposite d =
    match d with
    | North -> South
    | South -> North
    | East -> West
    | West -> East
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
    let idx = (y * grid.width) + x in
    grid.points.(idx)
  ;;

  let point_at_idx grid idx = grid.points.(idx)
  let copy grid = { grid with points = Array.copy grid.points }
  let from_points points = List.map ~f:(fun (_, v) -> v) points

  let swap_points grid (x1, y1) (x2, y2) =
    let idx1 = (y1 * grid.width) + x1 in
    let idx2 = (y2 * grid.width) + x2 in
    let p1 = grid.points.(idx1) in
    let p2 = grid.points.(idx2) in
    let _ = grid.points.(idx2) <- p1 in
    let _ = grid.points.(idx1) <- p2 in
    ()
  ;;

  let from_lines ~f lines =
    let points = Strings.read_chars_as_grid lines ~f |> from_points |> Array.of_list in
    { width = List.nth_exn lines 0 |> String.length; height = List.length lines; points }
  ;;
end
