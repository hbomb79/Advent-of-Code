open Core
open Advent.Plane

module Node = struct
  (* x, y, direction, momentum, heat_loss *)
  type t = int * int * Direction.t * int [@@deriving compare, show, equal]

  let pos (x, y, _, _) = x, y
end

module AStar = Advent.Pathfinding.AStar (Node)

(* Given the grid and the maximum allowed momentum (the number of cells
   a node is allowed to move in one direction before it must turn), this function
   will return a list of valid neighboring nodes *)
let adj_nodes grid min_momentum max_momentum (x, y, dir, momentum) =
  let open Direction in
  (* If not yet at maximum momentum, then produce a neighbor which is 'in front' of this node *)
  let continue =
    if momentum < max_momentum
    then (
      let moved = Point.shift_towards (x, y) dir in
      [ fst moved, snd moved, dir, momentum + 1 ])
    else []
  in
  (* Offer left/right turns (direction relative) if and only if the node has satisfied the min required momentum *)
  let turns =
    if momentum >= min_momentum
    then
      [ rotate_clockwise dir; rotate_anticlockwise dir ]
      |> List.map ~f:(fun new_dir ->
        let moved = Point.shift_towards (x, y) new_dir in
        fst moved, snd moved, new_dir, 1)
    else []
  in
  (* Only keep neighbors which actually exist in the grid *)
  List.filter ~f:(fun n -> ArrayGrid.point_inside grid (Node.pos n)) (continue @ turns)
;;

let g_of_node grid (x, y, _, _) = ArrayGrid.point_at_xy grid (x, y) |> Option.value_exn
let h_of_node (tx, ty) (x, y, _, _) = abs (tx - x) + abs (ty - y)

let is_goal target minm (x, y, _, momentum) =
  Point.equal (x, y) target && momentum >= minm
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day17.txt" in
  let grid = ArrayGrid.from_lines lines ~f:(fun ch -> int_of_string (String.make 1 ch)) in
  let start_seeds = [ 0, 0, Direction.East, 1; 0, 0, Direction.South, 1 ] in
  let target = ArrayGrid.width grid - 1, ArrayGrid.height grid - 1 in
  let find_path =
    AStar.find_shortest_path grid start_seeds ~h:(h_of_node target) ~g:(g_of_node grid)
  in
  let part_one_hl, _ = find_path ~goal:(is_goal target 0) ~adj:(adj_nodes grid 0 3) in
  let part_two_hl, _ = find_path ~goal:(is_goal target 4) ~adj:(adj_nodes grid 4 10) in
  let _ = Printf.printf "Part One: %d\nPart Two: %d\n" part_one_hl part_two_hl in
  ()
;;
