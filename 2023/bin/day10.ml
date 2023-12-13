open Core
open Advent.Strings

type direction =
  | North
  | South
  | West
  | East

type pipe_type =
  | Vertical
  | Horizontal
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  | Start
[@@deriving variants]

let read_char ch =
  match ch with
  | '|' -> Some Vertical
  | '-' -> Some Horizontal
  | 'L' -> Some NorthEast
  | 'J' -> Some NorthWest
  | '7' -> Some SouthWest
  | 'F' -> Some SouthEast
  | 'S' -> Some Start
  | _ -> None
;;

module IntTuple = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
  ;;
end

module IntTupleMap = CCMap.Make (IntTuple)

let tuple_equal (aX, aY) (bX, bY) = compare aX bX = 0 && compare aY bY = 0
let tile_equal (a, _) (b, _) = tuple_equal a b

let get_lattice_delta direction =
  match direction with
  | North -> 0, -1
  | South -> 0, 1
  | East -> 1, 0
  | West -> -1, 0
;;

let get_opposite_direction direction =
  match direction with
  | North -> South
  | South -> North
  | East -> West
  | West -> East
;;

let can_connect_to tile direction =
  match direction with
  | North -> is_northeast tile || is_northwest tile || is_vertical tile || is_start tile
  | South -> is_southeast tile || is_southwest tile || is_vertical tile || is_start tile
  | East -> is_northeast tile || is_southeast tile || is_horizontal tile || is_start tile
  | West -> is_northwest tile || is_southwest tile || is_horizontal tile || is_start tile
;;

let get_neighbor_of tile_map ((x, y), _) direction =
  let dx, dy = get_lattice_delta direction in
  let npos = x + dx, y + dy in
  let found_tile = IntTupleMap.get npos tile_map in
  Option.filter found_tile ~f:(fun tile_type ->
    can_connect_to tile_type (get_opposite_direction direction))
  |> Option.map ~f:(fun t -> npos, t)
;;

let find_neighbours tile_map tile =
  let directions =
    match snd tile with
    | Vertical -> [ North; South ]
    | Horizontal -> [ East; West ]
    | NorthEast -> [ North; East ]
    | NorthWest -> [ North; West ]
    | SouthEast -> [ South; East ]
    | SouthWest -> [ South; West ]
    | Start -> [ North; East; South; West ]
  in
  List.filter_map ~f:(fun dir -> get_neighbor_of tile_map tile dir) directions
;;

(* Removes seen nodes from the tiles provided - If no nodes remain after this filtering, AND the start tile exists
   in the given list of tiles, then this Start tile is returned to allow the loop to complete *)
let filter_neighbors seen tiles =
  let start_tile = List.find tiles ~f:(fun (_, t) -> is_start t) in
  let filtered =
    List.filter tiles ~f:(fun t -> not (List.mem ~equal:tile_equal seen t))
  in
  match start_tile, filtered with
  | Some st, [] -> [ st ]
  | _, ns -> ns
;;

let find_loop tile_map start =
  let rec aux path tile =
    match tile with
    | _, Start when List.length path <> 0 -> Some (List.rev path)
    | _ ->
      let neighbors = find_neighbours tile_map tile |> filter_neighbors path in
      List.find_map ~f:(fun neighbor -> aux (tile :: path) neighbor) neighbors
  in
  aux [] start
;;

let read_map lines =
  List.foldi
    ~init:[]
    ~f:(fun yIndex tile_acc line ->
      let newTiles =
        List.filter_mapi
          ~f:(fun xIndex ch ->
            match read_char ch with
            | Some t -> Some ((xIndex, yIndex), t)
            | _ -> None)
          (String.to_list line)
      in
      tile_acc @ newTiles)
    lines
;;

let cast_ray start_x start_y width height points =
  let rec aux x y intersections =
    if x >= width || y >= height
    then intersections
    else (
      match IntTupleMap.get (x, y) points with
      | None | Some NorthEast | Some SouthWest -> aux (x + 1) (y + 1) intersections
      | _ -> aux (x + 1) (y + 1) (intersections + 1))
  in
  aux start_x start_y 0
;;

let enclosed_tiles width height points =
  let count = ref 0 in
  for x = 0 to width do
    for y = 0 to height do
      if IntTupleMap.mem (x, y) points
      then ()
      else (
        let intersections = cast_ray x y width height points in
        if intersections mod 2 = 1 then count := !count + 1)
    done
  done;
  !count
;;

let () =
  let lines = read_lines "./inputs/day10.txt" in
  let tiles = read_map lines in
  let tileMap = IntTupleMap.of_list tiles in
  let start_tile = List.find_exn tiles ~f:(fun (_, t) -> is_start t) in
  let result = find_loop tileMap start_tile in
  let width = List.nth_exn lines 0 |> String.length in
  let height = List.length lines in
  let path =
    match result with
    | None -> failwith "no path"
    | Some path -> path
  in
  let path_map = IntTupleMap.of_list path in
  let replaced_map = IntTupleMap.add (fst start_tile) NorthEast path_map in
  let enc = enclosed_tiles width height replaced_map in
  let _ = Printf.printf "Part One: %d\n" (List.length path / 2) in
  let _ = Printf.printf "Part Two: %d\n" enc in
  ()
;;
