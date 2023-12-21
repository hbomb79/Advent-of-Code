open Core
open Advent.Plane

type cell =
  | Rock
  | Start
  | Garden
[@@deriving variants]

let read_char ch =
  match ch with
  | '.' -> Garden
  | '#' -> Rock
  | 'S' -> Start
  | _ -> failwith "invalid char"
;;

module Seen = struct
  type t = int * int [@@deriving compare, hash]
end

module SeenS = CCSet.Make (Seen)

let ( mod ) x y = ((x mod y) + y) mod y

let steps_reachable (grid : cell ArrayGrid.t) dist =
  let size = ArrayGrid.width grid in
  let parity = dist % 2 in
  let rec aux queue seen total =
    match queue with
    | [] -> total
    | (_, depth) :: qs when depth > dist -> aux qs seen total
    | (point, _) :: qs when SeenS.mem point seen -> aux qs seen total
    | (point, depth) :: qs ->
      let seen' = SeenS.add point seen in
      let total' = if depth mod 2 = parity then total + 1 else total in
      let ns =
        Point.neighbors point
        |> List.filter ~f:(fun n -> not (SeenS.mem n seen))
        |> List.filter_map ~f:(fun (x, y) ->
          ArrayGrid.point_at_xy grid (x mod size, y mod size)
          |> Option.filter ~f:(fun c -> not (is_rock c))
          |> Option.map ~f:(fun _ -> x, y))
      in
      let to_add = List.map ~f:(fun p -> p, depth + 1) ns in
      let qs' = qs @ to_add in
      aux qs' seen' total'
  in
  let x, y, _ =
    ArrayGrid.find_point grid ~f:(fun (_, _, cell) -> is_start cell) |> Option.value_exn
  in
  let initial_queue = [ (x, y), 0 ] in
  aux initial_queue (SeenS.of_list []) 0
;;

let _ =
  let grid =
    Advent.Strings.read_lines "./inputs/day21.txt"
    |> Advent.Plane.ArrayGrid.from_lines ~f:read_char
  in
  let reachable = steps_reachable grid 64 in
  let _ = Printf.printf "Part one: %d\n" reachable in
  let a = steps_reachable grid 65 in
  let b = steps_reachable grid (65 + 131) in
  let c = steps_reachable grid (65 + (131 * 2)) in
  let n = 26501365 / ArrayGrid.width grid in
  let b' = b - a in
  let c' = c - b in
  let ans2 = a + (b' * n) + (n * (n - 1) / 2 * (c' - b')) in
  let _ = Printf.printf "Part Two: %d\n" ans2 in
  ()
;;
