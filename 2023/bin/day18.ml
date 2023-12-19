open Core
open Advent.Plane

module Instruction = struct
  module M = CCMap.Make (Char)

  let dir_map =
    M.of_list
      [ 'U', Direction.North
      ; 'D', Direction.South
      ; 'L', Direction.West
      ; 'R', Direction.East
      ; '3', Direction.North
      ; '1', Direction.South
      ; '2', Direction.West
      ; '0', Direction.East
      ]
  ;;

  let read line =
    let splits = String.split ~on:' ' line in
    let dir = String.nget (List.nth_exn splits 0) 0 in
    M.find dir dir_map, int_of_string (List.nth_exn splits 1)
  ;;

  let read_hex line =
    let section = List.nth_exn (String.split ~on:' ' line) 2 in
    let dist_hex = String.sub section ~pos:2 ~len:5 in
    M.find (String.nget section 7) dir_map, int_of_string ("0x" ^ dist_hex)
  ;;
end

let extract_vertices_and_perimeter instructions =
  List.fold_left
    instructions
    ~init:([ 0, 0 ], 0)
    ~f:(fun (accv, accp) (dir, dist) ->
      let dx, dy = Direction.get_delta dir in
      let lastx, lasty = List.hd_exn accv in
      (lastx + (dx * dist), lasty + (dy * dist)) :: accv, accp + dist)
;;

let shoelace vertices =
  let area =
    List.fold_left
      (Advent.Lists.paired vertices)
      ~init:0
      ~f:(fun acc ((ax, ay), (bx, by)) -> acc + ((ax * by) - (bx * ay)))
    |> abs
  in
  area / 2
;;

let calc_area ~f lines =
  let instructions = List.map ~f lines in
  let vertices, perimeter = extract_vertices_and_perimeter instructions in
  shoelace vertices + ((perimeter / 2) + 1)
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day18.txt" in
  let _ = Printf.printf "Part One: %d\n" (calc_area ~f:Instruction.read lines) in
  let _ = Printf.printf "Part Two: %d\n" (calc_area ~f:Instruction.read_hex lines) in
  ()
;;
