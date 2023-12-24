open Core
open Advent.Plane
module S = CCSet.Make (Point)
module M = CCMap.Make (Point)

type tile =
  | Path
  | Forest
  | Slope of Direction.t
[@@deriving variants]

let read_char ch =
  match ch with
  | '.' -> Path
  | '#' -> Forest
  | '>' -> Slope Direction.East
  | '<' -> Slope Direction.West
  | '^' -> Slope Direction.North
  | 'v' -> Slope Direction.South
  | _ -> failwith "illegal char"
;;

let adj ignore_slopes grid point =
  let allowed_dirs =
    match ArrayGrid.point_at_xy grid point with
    | Some (Slope dir) when not ignore_slopes -> [ dir ]
    | Some _ -> [ Direction.North; Direction.South; Direction.East; Direction.West ]
    | None -> []
  in
  let point_is_not_forest point =
    ArrayGrid.point_at_xy grid point |> Option.exists ~f:(fun t -> not (is_forest t))
  in
  Point.neighbors ?dirs:(Some allowed_dirs) point |> List.filter ~f:point_is_not_forest
;;

(* Looks for points of interest in the grid, and performs flood-filling from
   each point until the next - this is then logged as a node in the graph, complete
   with it's x,y and 'distance' (edge weight) *)
let find_pois ignore_slopes grid =
  let start = 1, 0 in
  let dest = ArrayGrid.width grid - 2, ArrayGrid.height grid - 1 in
  let pois =
    Array.filter_mapi grid.points ~f:(fun idx tile ->
      match tile with
      | Forest -> None
      | _ ->
        let x = idx / grid.width in
        let y = idx % grid.width in
        let neighbors = adj ignore_slopes grid (x, y) in
        Option.some_if (List.length neighbors >= 3) (x, y))
    |> Array.to_list
  in
  start :: dest :: pois
;;

let construct_graph ignore_slopes grid =
  let pois = find_pois ignore_slopes grid in
  let poi_set = S.of_list pois in
  let rec aux origin stack seen graph =
    match stack with
    | [] -> graph
    | (x, y, cost) :: qs when cost <> 0 && S.mem (x, y) poi_set ->
      let inner = M.find origin graph |> M.add (x, y) cost in
      let graph' = M.add origin inner graph in
      aux origin qs seen graph'
    | (x, y, cost) :: qs ->
      let ns =
        adj ignore_slopes grid (x, y) |> List.filter ~f:(fun n -> not (S.mem n seen))
      in
      let seen' = S.add_list seen ns in
      let stack' = List.append qs (List.map ~f:(fun (x, y) -> x, y, cost + 1) ns) in
      aux origin stack' seen' graph
  in
  List.fold_left
    ~init:(List.map ~f:(fun p -> p, M.of_list []) pois |> M.of_list)
    ~f:(fun graph_acc (x, y) -> aux (x, y) [ x, y, 0 ] (S.of_list [ x, y ]) graph_acc)
    pois
;;

let dfs start target graph =
  let rec aux point seen =
    let running_max = Float.infinity |> int_of_float in
    if Point.equal point target
    then 0
    else (
      let seen' = S.add point seen in
      M.find point graph
      |> M.to_list
      |> List.filter ~f:(fun (n, _) -> not (S.mem n seen))
      |> List.fold_left ~init:running_max ~f:(fun acc_max (p, cost) ->
        Int.max acc_max (aux p seen' + cost)))
  in
  aux start (S.of_list [])
;;

let _ =
  let grid =
    Advent.Strings.read_lines "inputs/day23.txt" |> ArrayGrid.from_lines ~f:read_char
  in
  let start, dest = (1, 0), (ArrayGrid.width grid - 2, ArrayGrid.height grid - 1) in
  let _ = Printf.printf "Part One: %d\n" (dfs start dest (construct_graph false grid)) in
  let _ = Printf.printf "Part Two: %d\n" (dfs start dest (construct_graph true grid)) in
  ()
;;
