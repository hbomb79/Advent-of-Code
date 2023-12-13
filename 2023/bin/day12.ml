open Core
open Advent.Memo
open Advent.Strings

type record =
  { str : string
  ; groups : int list
  }

(* Module used to capture the state of the permutations we're testing *)
module PermState = struct
  type t = int * int * int [@@deriving hash, compare, sexp_of]
end

let valid_perms_memo record =
  (* As this function is passed through the memo_rec wrapper, it accepts a 'self' (which is the wrapper),
     rather than being truly recursive in it's own right *)
  let nth_group = List.nth_exn record.groups in
  let max_group_idx = List.length record.groups - 1 in
  let aux self (idx, group_idx, current_group_length) =
    if group_idx = max_group_idx && current_group_length = nth_group max_group_idx
    then if String.contains ~pos:idx record.str '#' then 0 else 1
    else if idx = String.length record.str
    then 0
    else (
      match String.get record.str idx with
      | '?' when current_group_length = 0 ->
        (* FORK: We COULD start a group here, recursively explore both . and # *)
        let perms_with_hash = self (idx + 1, group_idx, current_group_length + 1) in
        let perms_with_dot = self (idx + 1, group_idx, 0) in
        perms_with_dot + perms_with_hash
      | '?' when current_group_length < nth_group group_idx ->
        (* REC: We're currently inside of a group and it's not the right length to stop, keep going *)
        self (idx + 1, group_idx, current_group_length + 1)
      | '?' ->
        (* REC: Inside a group which has no more space in it, MUST be a . in order for it to be valid *)
        self (idx + 1, group_idx + 1, 0)
      | '#' when current_group_length = nth_group group_idx ->
        (* BASE: Broken spring, but the group we're building has no more space. This permutation is illegal *)
        0
      | '#' ->
        (* REC: Broken spring and our current group has space. Expand it. *)
        self (idx + 1, group_idx, current_group_length + 1)
      | '.' when current_group_length = nth_group group_idx ->
        (* REC: Pre-determined working-spring, and our current group is of the right size to be completed! *)
        self (idx + 1, group_idx + 1, 0)
      | '.' when current_group_length = 0 ->
        (* REC: Working spring, and we're not currently inside a broken group. Advance *)
        self (idx + 1, group_idx, 0)
      | _ ->
        (* BASE: Working spring, but our current group is incomplete. This permutation must be illegal *)
        0)
  in
  memo_rec (module PermState) aux (0, 0, 0)
;;

let read_record line =
  let splits = String.split line ~on:' ' in
  { str = List.nth_exn splits 0; groups = extract_numbers (List.nth_exn splits 1) }
;;

let explode sep list = List.init 5 ~f:(fun _ -> list) |> String.concat ~sep

let read_exploded_record line =
  let splits = String.split line ~on:' ' in
  let springs = List.nth_exn splits 0 |> explode "?" in
  let groups = List.nth_exn splits 1 |> explode "," in
  { str = springs; groups = extract_numbers groups }
;;

let _ =
  let lines = read_lines "./inputs/day12.txt" in
  let sum_perms rs = List.map ~f:valid_perms_memo rs |> List.reduce_exn ~f:( + ) in
  let p1 = List.map ~f:read_record lines |> sum_perms in
  let p2 = List.map ~f:read_exploded_record lines |> sum_perms in
  let _ = Printf.printf "Part One: %d\n" p1 in
  let _ = Printf.printf "Part Two: %d\n" p2 in
  ()
;;
