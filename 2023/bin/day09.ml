open Core

let list_sum = List.reduce_exn ~f:(fun x y -> x + y)
let calc_diffs = List.fold_left ~init:[] ~f:(fun acc (a, b) -> acc @ [ b - a ])

let rec find_diffs acc numbers =
  if List.for_all numbers ~f:(equal 0)
  then acc @ [ numbers ]
  else find_diffs (acc @ [ numbers ]) (Advent.paired numbers |> calc_diffs)
;;

let rec extrapolate ~select ~op acc histories =
  match histories with
  | [] -> acc
  | h :: hs -> extrapolate ~select ~op (op acc (select h)) hs
;;

let read_histories =
  Advent.read_lines "./inputs/day09.txt"
  |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:int_of_string)
  |> List.map ~f:(find_diffs [])
  |> List.map ~f:List.rev
;;

(* Part One *)
let () =
  let histories = read_histories in
  let extrapolate_next = extrapolate ~select:List.last_exn ~op:( + ) 0 in
  let next = List.map ~f:extrapolate_next histories |> list_sum in
  Printf.printf "Next: %d\n" next
;;

(* Part Two *)
let () =
  let histories = read_histories in
  let extrapolate_prev = extrapolate ~select:List.hd_exn ~op:(fun a b -> b - a) 0 in
  let prev = List.map ~f:extrapolate_prev histories |> list_sum in
  Printf.printf "Prev: %d\n" prev
;;
