open Core
open Advent.Plane
open Advent.Plane.Direction

module Tile = struct
  type t =
    | Empty
    | VerticalSplitter
    | HorizontalSplitter
    | AntiClockwiseMirror
    | ClockwiseMirror

  let read ch =
    match ch with
    | '.' -> Empty
    | '-' -> HorizontalSplitter
    | '|' -> VerticalSplitter
    | '/' -> ClockwiseMirror
    | '\\' -> AntiClockwiseMirror
    | _ -> failwith "illegal char"
  ;;
end

module TilePos = struct
  type t = (int * int) * Direction.t [@@deriving compare]
end

module TileSet = CCSet.Make (TilePos)
module IntTupleSet = CCSet.Make (Advent.IntTuple)

let sim_rays grid start =
  let open Tile in
  let rec aux energized rays =
    match rays with
    | [] ->
      TileSet.to_list energized
      |> List.map ~f:(fun (pos, _) -> pos)
      |> IntTupleSet.of_list
      |> IntTupleSet.to_list
    | ((ray, direction) as cray) :: rs ->
      let new_energized = TileSet.add (ray, direction) energized in
      let continue () =
        let new_ray = Point.shift_towards ray direction, direction in
        aux new_energized (new_ray :: rs)
      in
      (match Grid.point_at_xy grid ray with
       | None -> aux energized rs
       | Some _ when TileSet.mem cray energized -> aux energized rs
       | Some Empty -> continue ()
       | Some HorizontalSplitter when Direction.is_horizontal direction -> continue ()
       | Some VerticalSplitter when Direction.is_vertical direction -> continue ()
       | Some AntiClockwiseMirror ->
         let new_direction = Direction.rotate_anticlockwise direction in
         let new_ray = Point.shift_towards ray new_direction, new_direction in
         aux new_energized (new_ray :: rs)
       | Some ClockwiseMirror ->
         let new_direction = Direction.rotate_clockwise direction in
         let new_ray = Point.shift_towards ray new_direction, new_direction in
         aux new_energized (new_ray :: rs)
       | Some HorizontalSplitter ->
         let new_rays =
           (Point.shift_towards ray East, East)
           :: (Point.shift_towards ray West, West)
           :: rs
         in
         aux new_energized new_rays
       | Some VerticalSplitter ->
         let new_rays =
           (Point.shift_towards ray South, South)
           :: (Point.shift_towards ray North, North)
           :: rs
         in
         aux new_energized new_rays)
  in
  aux (TileSet.of_list []) [ start ]
;;

let find_max_energized grid =
  let grid_right = Grid.width grid - 1 in
  let grid_bottom = Grid.height grid - 1 in
  let hor =
    List.init (Grid.width grid) ~f:(fun x ->
      match x with
      | 0 ->
        [ (0, 0), East; (0, 0), South; (0, grid_bottom), North; (0, grid_bottom), East ]
      | n when n = grid_right ->
        [ (grid_right, 0), West
        ; (grid_right, 0), South
        ; (grid_right, grid_bottom), West
        ; (grid_right, grid_bottom), North
        ]
      | _ -> [ (x, 0), South; (x, 0), North ])
  in
  let vert =
    List.init
      (Grid.height grid - 2)
      ~f:(fun y -> [ (0, y + 1), East; (grid_right, y + 1), West ])
  in
  let percentage_done d total =
    int_of_float (float_of_int d /. float_of_int total *. 100.)
  in
  let all = Stdlib.List.flatten (hor @ vert) in
  let total = List.length all in
  List.mapi all ~f:(fun i s ->
    let _ =
      Printf.printf
        "\r%d%% Complete [%d of %d]...%!"
        (percentage_done (i + 1) total)
        (i + 1)
        total
    in
    sim_rays grid s)
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day16.txt" in
  let g = Grid.from_lines ~f:Tile.read lines in
  let energized = sim_rays g ((0, 0), East) in
  let _ = Printf.printf "Part One: %d\n" (List.length energized) in
  let max =
    find_max_energized g
    |> List.sort ~compare:(fun a b -> compare_int (List.length a) (List.length b))
    |> List.last_exn
  in
  let _ = Printf.printf "\nPart Two: %d\n" (List.length max) in
  ()
;;
