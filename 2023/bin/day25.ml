open Core

type vertex =
  | Single of string
  | Super of vertex ref list
[@@deriving compare, equal, show]

type edge = vertex ref * vertex ref [@@deriving equal]

module VertexSet = CCSet.Make (struct
    type t = vertex ref [@@deriving compare]
  end)

type graph =
  { vertices : VertexSet.t
  ; edges : edge list
  }

(*
   Given two vertices representing the two endpoints of an edge, this function
   will ensure that the 'Super' variant of the vertex is handled by correctly
   merging the references of any single variant in to the vertex ref list held
   by the super variant.
*)
let form_super_node (a, b) =
  let a' =
    match !a with
    | Single _ -> [ a ]
    | Super vs -> vs
  in
  let b' =
    match !b with
    | Single _ -> [ b ]
    | Super vs -> vs
  in
  Super (VertexSet.of_list (a' @ b') |> VertexSet.to_list)
;;

let contract_random_edge graph =
  let ix = Random.int (List.length graph.edges) in
  let edge_to_remove = List.nth_exn graph.edges ix in
  let a, b = edge_to_remove in
  let new_vertex = form_super_node edge_to_remove |> ref in
  let vertices' =
    graph.vertices |> VertexSet.remove a |> VertexSet.remove b |> VertexSet.add new_vertex
  in
  let edges' =
    (* Remove all edges between `a` and `b`. *)
    List.filter ~f:(fun edge -> not (equal_edge edge edge_to_remove)) graph.edges
    (* Map all vertex names to `new_vertex`. `rev_map` is more efficient than
       `map`. *)
    |> List.rev_map ~f:(fun (x, y) ->
      let x' = if equal_vertex !x !a || equal_vertex !x !b then new_vertex else x in
      let y' = if equal_vertex !y !a || equal_vertex !y !b then new_vertex else y in
      x', y')
  in
  { vertices = vertices'; edges = edges' }
;;

let rec karger graph =
  if VertexSet.cardinal graph.vertices > 2 && not (List.is_empty graph.edges)
  then karger (contract_random_edge graph)
  else graph
;;

let print_results graph =
  let vs =
    VertexSet.fold
      (fun v acc ->
        match !v with
        | Single _ -> 1 :: acc
        | Super vs -> List.length vs :: acc)
      graph.vertices
      []
  in
  let a = List.nth_exn vs 0 in
  let b = List.nth_exn vs 1 in
  let _ =
    Printf.printf
      "\nBest cut so far: %d (A=%d, B=%d, A*B=%d)%!\n"
      (List.length graph.edges)
      a
      b
      (a * b)
  in
  ()
;;

let rec run_karger count best_cut required edges =
  if best_cut = required
  then (
    let _ =
      Printf.printf "COMPLETED (found cut of required = %d) in %d runs\n" required count
    in
    ())
  else (
    let _ = Printf.printf "\rRun %d...%!" count in
    let cut_graph = karger edges in
    let cut_length = List.length cut_graph.edges in
    let best_cut' = if cut_length < best_cut then cut_length else best_cut in
    let _ =
      if best_cut' < best_cut
      then (
        let _ = print_results cut_graph in
        ())
      else ()
    in
    run_karger (count + 1) best_cut' required edges)
;;

(* Each line of our input dictates a set of vertices (n >= 2) and a set of edges (m >= 1). Given
   our graph memory model does not rely on storing vertices inside of edges by memory reference (simple
   string comparison instead), we can accumulate a set of vertices and edges as we go).
   While the edges are undirected, the input specifically only mentions a given edge ONCE
*)
let parse_graph lines =
  List.fold_left
    ~init:(VertexSet.empty, [])
    ~f:(fun (v_acc, m_acc) line ->
      let splits = String.split ~on:':' line |> List.nth_exn in
      let source = Single (splits 0) in
      let dests =
        String.strip (splits 1)
        |> String.split ~on:' '
        |> List.map ~f:(fun v -> ref (Single v))
      in
      let v_acc' = VertexSet.add_list v_acc (ref source :: dests) in
      let m_acc' = m_acc @ List.map ~f:(fun d -> ref source, d) dests in
      v_acc', m_acc')
    lines
;;

let () =
  let vertices, edges = parse_graph (Advent.Strings.read_lines "inputs/day25.txt") in
  let _ = Printf.printf "Starting (looking for minimum cut of size 3)\n" in
  run_karger 0 10 3 { vertices; edges }
;;
