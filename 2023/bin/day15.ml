open Core
open Advent.Lists

let hash_str str =
  List.fold_left
    ~f:(fun acc ch -> (acc + int_of_char ch) * 17 mod 256)
    ~init:0
    (String.to_list str)
;;

module Instruction = struct
  type t =
    | Insert of int * string * int
    | Remove of int * string

  let read str =
    if String.contains str '-'
    then (
      let label = String.take_while ~f:(fun ch -> compare_char ch '-' <> 0) str in
      Remove (hash_str label, label))
    else (
      let p = String.split str ~on:'=' in
      let label = List.nth_exn p 0 in
      let num = List.nth_exn p 1 |> int_of_string in
      Insert (hash_str label, label, num))
  ;;
end

let insert_lens boxes box_num lens focal =
  let box = List.nth_exn boxes box_num in
  let lens_idx = List.findi box ~f:(fun _ (label, _) -> equal_string label lens) in
  match lens_idx with
  | None -> replace_map boxes box_num ~f:(fun _ box -> box @ [ lens, focal ])
  | Some (idx, _) ->
    replace_map boxes box_num ~f:(fun _ lenses -> replace lenses idx (lens, focal))
;;

let remove_lens boxes box_num lens =
  replace_map boxes box_num ~f:(fun _ lenses ->
    List.filter ~f:(fun (label, _) -> not (equal_string label lens)) lenses)
;;

let rec apply_instructions boxes instructions =
  match instructions with
  | [] -> boxes
  | x :: xs ->
    let applied =
      match x with
      | Instruction.Insert (box, lens_label, lens_focal) ->
        insert_lens boxes box lens_label lens_focal
      | Instruction.Remove (box, lens_label) -> remove_lens boxes box lens_label
    in
    apply_instructions applied xs
;;

let box_focal_power box_num lenses =
  List.foldi
    ~init:0
    ~f:(fun sloti acc (_, focal) -> acc + ((box_num + 1) * (sloti + 1) * focal))
    lenses
;;

let () =
  let sections =
    Advent.Strings.read_lines "./inputs/day15.txt" |> List.hd_exn |> String.split ~on:','
  in
  let _ = Printf.printf "Part one: %d\n" (List.map ~f:hash_str sections |> elt_sum) in
  let instructions = List.map ~f:Instruction.read sections in
  let applied = apply_instructions (List.init 256 ~f:(fun _ -> [])) instructions in
  let bxpw =
    List.foldi
      ~init:0
      ~f:(fun boxi acc lenses -> acc + box_focal_power boxi lenses)
      applied
  in
  let _ = Printf.printf "Part two: %d\n" bxpw in
  ()
;;
