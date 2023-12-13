open Core

let concat_ints_in_string str =
  Advent.extract_numbers str
  |> List.map ~f:string_of_int
  |> String.concat ~sep:""
  |> int_of_string
;;

let find_quadratic_eq_roots t d =
  let a = 1.0 in
  let b = -.float_of_int t in
  let c = float_of_int d in
  let discriminant = (b ** 2.0) -. (4.0 *. a *. c) in
  if Float.compare discriminant 0.0 < 0
  then []
  else (
    let sqrt_discriminant = sqrt discriminant in
    let x1 = (-.b +. sqrt_discriminant) /. (2.0 *. a) in
    let x2 = (-.b -. sqrt_discriminant) /. (2.0 *. a) in
    [ x1; x2 ])
;;

let ints_between f1 f2 =
  (Float.round_up f1 |> int_of_float) - (Float.round_down f2 |> int_of_float) - 1
;;

let ints_between_quadratic_eq_roots t d =
  match find_quadratic_eq_roots t d with
  | [ hi; lo ] -> ints_between hi lo
  | _ -> Advent.unreachable () |> List.reduce_exn ~f:( * )
;;

let () =
  let lines = Advent.read_lines "./inputs/day06.txt" in
  let durations = Advent.extract_numbers (List.nth_exn lines 0) in
  let distances = Advent.extract_numbers (List.nth_exn lines 1) in
  let winning_times_count =
    List.zip_exn durations distances
    |> List.map ~f:(fun (t, d) -> ints_between_quadratic_eq_roots t d)
    |> List.reduce_exn ~f:( * )
  in
  Printf.printf "Part One: %d\n" winning_times_count
;;

let () =
  let lines = Advent.read_lines "./inputs/day06.txt" in
  let duration = concat_ints_in_string (List.nth_exn lines 0) in
  let distance = concat_ints_in_string (List.nth_exn lines 1) in
  let winning_times_count = ints_between_quadratic_eq_roots duration distance in
  Printf.printf "Part Two: %d\n" winning_times_count
;;
