open Core

module Djikstra (Node : CCMap.OrderedType) = struct
  (* A graph node 'Node' tupled with it's accumulated cost to get to that node [used to
   sort priority queue] *)
  module NodeWithCost = struct
    type t = int * Node.t

    let compare (a, _) (b, _) = compare a b
    let pos (_, (x, y, _, _)) = x, y
  end

  module PQ = CCHeap.Make_from_compare (NodeWithCost)
  module S = CCSet.Make (Node)
  module M = CCMap.Make (Node)

  let construct_path_to node tracks =
    let rec aux acc current =
      match M.get current tracks with
      | None -> acc
      | Some parent -> aux (parent :: acc) parent
    in
    aux [ node ] node
  ;;

  let find_shortest_path _grid seeds ~adj ~goal ~g =
    let rec aux open_nodes closed_nodes tracks =
      match PQ.take open_nodes with
      | None -> failwith "no path to target"
      | Some (_, node) when goal node -> fst node, construct_path_to (snd node) tracks
      | Some (open_nodes', (_, node)) when S.mem node closed_nodes ->
        aux open_nodes' closed_nodes tracks
      | Some (open_nodes', (current_g, current_node)) ->
        let closed_nodes' = S.add current_node closed_nodes in
        let neighbors =
          adj current_node |> List.filter ~f:(fun n -> not (S.mem n closed_nodes))
        in
        let open_nodes'', tracks' =
          List.fold_left
            ~init:(open_nodes', tracks)
            ~f:(fun (onodes, ts) neighbor ->
              let onodes' = PQ.add onodes (current_g + g neighbor, neighbor) in
              let ts' = M.add neighbor current_node ts in
              onodes', ts')
            neighbors
        in
        aux open_nodes'' closed_nodes' tracks'
    in
    let start = List.map ~f:(fun seed -> 0, seed) seeds in
    aux (PQ.of_list start) (S.of_list []) (M.of_list [])
  ;;
end
