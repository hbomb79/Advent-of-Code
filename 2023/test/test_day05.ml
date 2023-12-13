open OUnit2
open Advent.Ranges

let completely_overlapping_ranges _ =
  let input = { start = 100; stop = 200 } in
  let source = { start = 100; stop = 200 } in
  let dest = { start = 400; stop = 500 } in
  let out = process_ranges input source dest in
  match out with
  | Some [ a ] -> assert_equal dest.start a.start
  | _ -> failwith "incorrect range output"
;;

let surrounded_ranges _ =
  let input = { start = 0; stop = 300 } in
  let source = { start = 100; stop = 200 } in
  let dest = { start = 90; stop = 190 } in
  let out = process_ranges input source dest in
  match out with
  | Some [ a; b; c ] ->
    let _ = assert_equal 0 a.start ~printer:string_of_int in
    let _ = assert_equal 99 a.stop ~printer:string_of_int in
    let _ = assert_equal 90 b.start ~printer:string_of_int in
    let _ = assert_equal 190 b.stop ~printer:string_of_int in
    let _ = assert_equal 201 c.start ~printer:string_of_int in
    assert_equal c.stop 300
  | _ -> failwith "incorrect range output"
;;

let overlap_spill_left _ =
  let input = { start = 0; stop = 199 } in
  let source = { start = 100; stop = 200 } in
  let dest = { start = 400; stop = 500 } in
  let out = process_ranges input source dest in
  match out with
  | Some [ a; b ] ->
    (* let _ = Printf.printf "a = %s   b = %s\n" (show_range a) (show_range b) in *)
    let _ = assert_equal 0 a.start ~printer:string_of_int in
    let _ = assert_equal 99 a.stop ~printer:string_of_int in
    let _ = assert_equal 400 b.start ~printer:string_of_int in
    assert_equal 499 b.stop ~printer:string_of_int
  | _ -> failwith "incorrect range output"
;;

let overlap_spill_right _ =
  let input = { start = 101; stop = 300 } in
  let source = { start = 100; stop = 200 } in
  let dest = { start = 0; stop = 100 } in
  let out = process_ranges input source dest in
  match out with
  | Some [ a; b ] ->
    (* let _ = Printf.printf "a = %s   b = %s\n" (show_range a) (show_range b) in *)
    let _ = assert_equal 1 a.start ~printer:string_of_int in
    let _ = assert_equal 100 a.stop ~printer:string_of_int in
    let _ = assert_equal 201 b.start ~printer:string_of_int in
    assert_equal 300 b.stop ~printer:string_of_int
  | _ -> failwith "incorrect range output"
;;

let input_is_contained_by_source_range _ =
  let input = { start = 140; stop = 160 } in
  let source = { start = 100; stop = 200 } in
  let dest = { start = 200; stop = 300 } in
  let out = process_ranges input source dest in
  match out with
  | Some [ a ] ->
    (* let _ = Printf.printf "a = %s   b = %s\n" (show_range a) (show_range b) in *)
    let _ = assert_equal 240 a.start ~printer:string_of_int in
    assert_equal 260 a.stop ~printer:string_of_int
  | _ -> failwith "incorrect range output"
;;

(* Name the test cases and group them together *)
let suite =
  "suite"
  >::: [ "completely overlapping ranges" >:: completely_overlapping_ranges
       ; "input range overlaps and surrounds input range" >:: surrounded_ranges
       ; "input range overlaps and spills over on the left" >:: overlap_spill_left
       ; "input range overlaps and spills over on the right" >:: overlap_spill_right
       ; "input range is contained with no spill" >:: input_is_contained_by_source_range
       ]
;;

let () = run_test_tt_main suite
