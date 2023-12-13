open Core

(* type galaxy = int * int *)

let read_char x y ch =
  match ch with
  | '#' -> Some (x, y)
  | _ -> None
;;

let read_line y line =
  List.filter_mapi ~f:(fun index ch -> read_char index y ch) (String.to_list line)
;;

(* Recurses over the height of the map, leading each line and tracking
   any rows that had no galaxies found *)
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

module IntSet = CCSet.Make (Int)

let read_map lines =
  let height = List.length lines in
  let width = List.nth_exn lines 0 |> String.length in
  let empty_rows, galaxies = read_lines height lines in
  let occupied_cols = List.map ~f:(fun (x, _) -> x) galaxies |> IntSet.of_list in
  let all_cols = List.init width ~f:(fun x -> x) |> IntSet.of_list in
  let empty_cols = IntSet.diff all_cols occupied_cols |> IntSet.to_list in
  empty_rows, empty_cols, galaxies
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day11-test.txt" in
  let empty_rows, empty_cols, galaxies = read_map lines in
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
  ()
;;
