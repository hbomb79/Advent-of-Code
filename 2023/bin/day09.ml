open Core
open Advent.Lists
open Advent.Strings

let rec find_diffs acc numbers =
  if List.for_all numbers ~f:(equal 0)
  then acc @ [ numbers ]
  else find_diffs (acc @ [ numbers ]) (elt_diffs numbers)
;;

let rec extrapolate ~select ~op acc histories =
  match histories with
  | [] -> acc
  | h :: hs -> extrapolate ~select ~op (op acc (select h)) hs
;;

let read_histories =
  read_lines "./inputs/day09.txt"
  |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:int_of_string)
  |> List.map ~f:(find_diffs [])
  |> List.map ~f:List.rev
;;

(* Part One *)
let () =
  let histories = read_histories in
  let extrapolate_next = extrapolate ~select:List.last_exn ~op:( + ) 0 in
  let next = List.map ~f:extrapolate_next histories |> elt_sum in
  Printf.printf "Next: %d\n" next
;;

(* Part Two *)
let () =
  let histories = read_histories in
  let extrapolate_prev = extrapolate ~select:List.hd_exn ~op:(fun a b -> b - a) 0 in
  let prev = List.map ~f:extrapolate_prev histories |> elt_sum in
  Printf.printf "Prev: %d\n" prev
;;
