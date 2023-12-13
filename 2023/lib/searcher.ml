type direction =
  | Forwards
  | Backwards

let find_match pattern line direction =
  let searcher, offset =
    match direction with
    | Forwards -> Str.search_forward, 0
    | Backwards -> Str.search_backward, String.length line
  in
  try
    let index = searcher pattern line offset in
    let substr = Str.matched_group 0 line in
    Some (index, substr)
  with
  | _ -> None
;;

(* let lattice_bfs ~equal width height start target = *)
(*   let rec aux seen queue = *)
(*     match queue with *)
(*     | [] -> failwith "No path" *)
(*     | (q, path) :: _ when equal q target -> List.rev (q :: path) *)
(*     | (q, _) :: qs when IntTupleSet.mem q seen -> aux seen qs *)
(*     | (q, path) :: qs -> *)
(*       let new_path = q :: path in *)
(*       let neighbors_list = *)
(*         neighbors width height q *)
(*         |> List.filter ~f:(fun npos -> not (IntTupleSet.mem npos seen)) *)
(*         |> List.map ~f:(fun npos -> npos, new_path) *)
(*       in *)
(*       let new_seen = IntTupleSet.add q seen in *)
(*       let new_queue = List.append qs neighbors_list in *)
(*       aux new_seen new_queue *)
(*   in *)
(*   aux (IntTupleSet.of_list []) [ start, [] ] *)
(* ;; *)
