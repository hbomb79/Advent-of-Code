open Core
module IntSet = CCSet.Make (Int)

module IntTuple = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with
    | 0 -> compare y1 y2
    | c -> c
  ;;
end

module IntTupleSet = CCSet.Make (IntTuple)

(* type point = int * int [@@deriving show { with_path = false }] *)

let read_char x y ch =
  match ch with
  | '#' -> Some (x, y)
  | _ -> None
;;

let read_line y line =
  List.filter_mapi ~f:(fun index ch -> read_char index y ch) (String.to_list line)
;;

let read_lines height lines =
  let rec aux empty_rows galaxys y =
    match y with
    | y when y = height -> empty_rows, galaxys
    | _ ->
      let l = List.nth_exn lines y in
      (match read_line y l with
       | [] -> aux (y :: empty_rows) galaxys (y + 1)
       | gs -> aux empty_rows (galaxys @ gs) (y + 1))
  in
  aux [] [] 0
;;

let read_map width height lines =
  let empty_rows, galaxies = read_lines height lines in
  let occupied_cols = List.map ~f:(fun (x, _) -> x) galaxies |> IntSet.of_list in
  let all_cols = List.init width ~f:(fun x -> x) |> IntSet.of_list in
  let empty_cols = IntSet.diff all_cols occupied_cols |> IntSet.to_list in
  empty_rows, empty_cols, galaxies
;;

let push_galaxies_apart empty_rows empty_cols galaxies =
  let rec aux acc list =
    match list with
    | [] -> acc
    | (gx, gy) :: gs ->
      let dy = List.count ~f:(fun r -> r < gy) empty_rows in
      let dx = List.count ~f:(fun r -> r < gx) empty_cols in
      let adjusted = gx + dx, gy + dy in
      aux (adjusted :: acc) gs
  in
  aux [] galaxies
;;

let neighbors width height (x, y) =
  let possible_moves = [ -1, 0; 1, 0; 0, -1; 0, 1 ] in
  List.filter_map
    ~f:(fun (dx, dy) ->
      let nx, ny = x + dx, y + dy in
      if nx >= 0 && nx < width && ny >= 0 && ny < height then Some (nx, ny) else None)
    possible_moves
;;

let point_equal (ax, ay) (bx, by) = compare ax bx = 0 && compare ay by = 0
(* let show_point_list ls = List.map ~f:show_point ls |> String.concat ~sep:", " *)

let bfs width height start target =
  let rec aux seen queue =
    match queue with
    | [] -> failwith "No path"
    | (q, path) :: _ when point_equal q target -> List.rev (q :: path)
    | (q, _) :: qs when IntTupleSet.mem q seen -> aux seen qs
    | (q, path) :: qs ->
      let new_path = q :: path in
      let neighbors_list =
        neighbors width height q
        |> List.filter ~f:(fun npos -> not (IntTupleSet.mem npos seen))
        |> List.map ~f:(fun npos -> npos, new_path)
      in
      let new_seen = IntTupleSet.add q seen in
      let new_queue = List.append qs neighbors_list in
      aux new_seen new_queue
  in
  aux (IntTupleSet.of_list []) [ start, [] ]
;;

let rec combinations k l =
  if k = 0
  then [ [] ]
  else (
    match l with
    | [] -> []
    | x :: xs ->
      List.map ~f:(fun tl -> x :: tl) (combinations (k - 1) xs) @ combinations k xs)
;;

let find_all_distances points = combinations 2 points

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day11.txt" in
  let height = List.length lines in
  let width = List.nth_exn lines 0 |> String.length in
  let empty_rows, empty_cols, galaxies = read_map width height lines in
  let adjusted_height = height + List.length empty_rows in
  let adjusted_width = width + List.length empty_cols in
  let _ =
    Printf.printf
      "Empty row indices: %s\n"
      (List.map ~f:(fun x -> string_of_int x) empty_rows |> String.concat ~sep:", ")
  in
  let _ =
    Printf.printf
      "Empty col indices: %s\n"
      (List.map ~f:(fun x -> string_of_int x) empty_cols |> String.concat ~sep:", ")
  in
  let _ =
    Printf.printf
      "Galaxy positions: %s\n"
      (List.map ~f:(fun (x, y) -> Printf.sprintf "[%d,%d]" x y) galaxies
       |> String.concat ~sep:", ")
  in
  let adjusted = push_galaxies_apart empty_rows empty_cols galaxies |> List.rev in
  let _ =
    Printf.printf
      "Adjusted Galaxy positions: %s\n"
      (List.map ~f:(fun (x, y) -> Printf.sprintf "[%d,%d]" x y) adjusted
       |> String.concat ~sep:", ")
  in
  let all_dists = find_all_distances adjusted in
  let distances =
    List.mapi
      ~f:(fun i x ->
        match x with
        | [ start; target ] ->
          let sol = List.length (bfs adjusted_width adjusted_height start target) - 1 in
          let _ =
            Printf.printf
              "[%d/%d] Found path for (%d,%d) -> (%d,%d) of length %d%!\n"
              i
              (List.length all_dists)
              (fst start)
              (snd start)
              (fst target)
              (snd target)
              sol
          in
          sol
        | _ -> failwith "unexpected non-pair")
      all_dists
    |> List.reduce_exn ~f:( + )
  in
  (* let start_node = List.nth_exn galaxies 0 in *)
  (* let target_node = List.nth_exn galaxies 4 in *)
  (* let path = bfs width height start_node target_node in *)
  (* let _ = *)
  (*   Printf.printf *)
  (*     "Distance between (%d,%d) and (%d,%d) is %d" *)
  (*     (fst start_node) *)
  (*     (snd start_node) *)
  (*     (fst target_node) *)
  (*     (snd target_node) *)
  (*     (List.length path) *)
  (* in *)
  let _ = Printf.printf "All path lengths: %d\n" distances in
  ()
;;
