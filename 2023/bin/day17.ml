open Core

module Node = struct
  (* x, y, direction, momentum, heat_loss *)
  type t = int * int * Advent.Plane.Direction.t * int [@@deriving compare, show, equal]

  let pos (x, y, _, _) = x, y
end

module NodeWithHeat = struct
  (* x, y, direction, momentum, heat_loss *)
  type t = int * (int * int * Advent.Plane.Direction.t * int) [@@deriving show]

  let compare (a, _) (b, _) = compare a b
  let pos (_, (x, y, _, _)) = x, y
end

module PQ = CCHeap.Make_from_compare (NodeWithHeat)
module S = CCSet.Make (Node)
module M = CCMap.Make (Node)

let construct_path_to node tracks =
  let rec aux acc current =
    match M.get current tracks with
    | None -> acc
    | Some parent ->
      let _ =
        Printf.printf "Parent of %s is %s\n" (Node.show current) (Node.show parent)
      in
      aux (parent :: acc) parent
  in
  aux [ node ] node
;;

(* let print_search_state grid o c = *)
(*   let _ = Printf.printf "\n%!" in *)
(*   for y = 0 to Advent.Plane.ArrayGrid.height grid - 1 do *)
(*     let _ = Printf.printf "\n" in *)
(*     for x = 0 to Advent.Plane.ArrayGrid.width grid - 1 do *)
(*       let s = *)
(*         if PQ.filter (fun n -> Advent.Plane.Point.equal (NodeWithHeat.pos n) (x, y)) o *)
(*            |> PQ.size *)
(*            > 0 *)
(*         then "O" *)
(*         else if S.filter (fun n -> Advent.Plane.Point.equal (Node.pos n) (x, y)) c *)
(*                 |> S.to_list *)
(*                 |> List.length *)
(*                 > 0 *)
(*         then "C" *)
(*         else "." *)
(*       in *)
(*       let _ = Printf.printf "%s" s in *)
(*       () *)
(*     done *)
(*   done *)
(* ;; *)

let find_path grid adj (sx, sy) target =
  let rec aux open_nodes closed_nodes tracks =
    (* let _ = print_search_state grid open_nodes closed_nodes in *)
    match PQ.take open_nodes with
    | None -> failwith "no path to target"
    | Some (_, (hl, node)) when Advent.Plane.Point.equal (Node.pos node) target -> hl
    | Some (open_nodes', (_, node)) when S.mem node closed_nodes ->
      aux open_nodes' closed_nodes tracks
    | Some (open_nodes', (current_hl, current_node)) ->
      let closed_nodes' = S.add current_node closed_nodes in
      let neighbors =
        adj current_node |> List.filter ~f:(fun n -> not (S.mem n closed_nodes))
      in
      let open_nodes'' =
        List.fold_left
          ~init:open_nodes'
          ~f:(fun onodes (x, y, dir, momentum) ->
            let neighbor_hl =
              Advent.Plane.ArrayGrid.point_at_xy grid (x, y) |> Option.value_exn
            in
            PQ.add onodes (current_hl + neighbor_hl, (x, y, dir, momentum)))
          neighbors
      in
      aux open_nodes'' closed_nodes' tracks
  in
  let a = sx, sy, Advent.Plane.Direction.East, 1 in
  let b = sx, sy, Advent.Plane.Direction.South, 1 in
  aux (PQ.of_list [ 0, a; 0, b ]) (S.of_list []) (M.of_list [])
;;

(* Given the grid and the maximum allowed momentum (the number of cells
   a node is allowed to move in one direction before it must turn), this function
   will return a list of valid neighboring nodes *)
let adj_nodes grid max_momentum (x, y, dir, momentum) =
  let open Advent.Plane.Direction in
  (* If not yet at maximum momentum, then produce a neighbor which is 'in front' of this node *)
  let continue =
    if momentum < max_momentum
    then (
      let moved = Advent.Plane.Point.shift_towards (x, y) dir in
      [ fst moved, snd moved, dir, momentum + 1 ])
    else []
  in
  (* We can always consider the 'left'/'right' neighbors of a node (direction relative) *)
  let turns =
    [ rotate_clockwise dir; rotate_anticlockwise dir ]
    |> List.map ~f:(fun new_dir ->
      let moved = Advent.Plane.Point.shift_towards (x, y) new_dir in
      fst moved, snd moved, new_dir, 1)
  in
  (* Only keep neighbors which actually exist in the grid *)
  List.filter
    ~f:(fun n -> Advent.Plane.ArrayGrid.point_inside grid (Node.pos n))
    (continue @ turns)
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day17.txt" in
  let grid =
    Advent.Plane.ArrayGrid.from_lines lines ~f:(fun ch ->
      int_of_string (String.make 1 ch))
  in
  let hl =
    find_path
      grid
      (adj_nodes grid 3)
      (0, 0)
      (Advent.Plane.ArrayGrid.width grid - 1, Advent.Plane.ArrayGrid.height grid - 1)
  in
  let _ = Printf.printf "Part One: %d\n" hl in
  ()
;;
