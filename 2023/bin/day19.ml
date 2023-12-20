open Core
open Advent
module M = CCMap.Make (String)

module Part = struct
  type t =
    { x : int
    ; m : int
    ; a : int
    ; s : int
    }
  [@@deriving show]

  let read line =
    match Advent.Strings.extract_numbers line with
    | [ x; m; a; s ] -> { x; m; a; s }
    | _ -> failwith "invalid part"
  ;;

  let value { x; m; a; s } = x + m + a + s
end

module RangePart = struct
  type t =
    { x : Ranges.range
    ; m : Ranges.range
    ; a : Ranges.range
    ; s : Ranges.range
    }
  [@@deriving show]

  let value { x; m; a; s } =
    Ranges.value x * Ranges.value m * Ranges.value a * Ranges.value s
  ;;

  let zero = { x = Ranges.zero; m = Ranges.zero; a = Ranges.zero; s = Ranges.zero }
end

let extract_rule_stuff str =
  (* Must be some expression of form `[xmas][<>]n:dest` *)
  let splits = String.split ~on:':' str |> List.nth_exn in
  let expr = splits 0 |> String.split_on_chars ~on:[ '<'; '>' ] |> List.nth_exn in
  expr 0, expr 1 |> int_of_string, splits 1
;;

module RangeRule = struct
  type result =
    | Accepted of RangePart.t * RangePart.t
    | Rejected of RangePart.t * RangePart.t
    | Move of string * RangePart.t * RangePart.t

  type t = RangePart.t -> result

  let build_action range_op field (part : RangePart.t) =
    let prange =
      match field with
      | "x" -> part.x
      | "m" -> part.m
      | "a" -> part.a
      | "s" -> part.s
      | _ -> failwith "unreachable"
    in
    let inside, outside = range_op prange in
    match field with
    | "x" -> { part with x = inside }, { part with x = outside }
    | "m" -> { part with m = inside }, { part with m = outside }
    | "a" -> { part with a = inside }, { part with a = outside }
    | "s" -> { part with s = inside }, { part with s = outside }
    | _ -> failwith "unreachable"
  ;;

  let parse_dest dest =
    match dest with
    | "A" -> fun (a, b) -> Accepted (a, b)
    | "R" -> fun (a, b) -> Rejected (a, b)
    | other -> fun (a, b) -> Move (other, a, b)
  ;;

  let read str =
    if String.contains str ':'
    then (
      let lhs, rhs, dest = extract_rule_stuff str in
      let op = if String.contains str '>' then Ranges.range_gt else Ranges.range_lt in
      let action = build_action (op rhs) lhs in
      fun rp -> action rp |> parse_dest dest)
    else fun rp -> (parse_dest str) (rp, RangePart.zero)
  ;;
end

module Rule = struct
  type result =
    | Accepted
    | Rejected
    | Move of string
    | Skip

  type t = Part.t -> result

  let parse_dest str =
    match str with
    | "R" -> Rejected
    | "A" -> Accepted
    | label -> Move label
  ;;

  let build_action lhs (op : int -> int -> bool) rhs result =
    let field_selector (part : Part.t) =
      match lhs with
      | "x" -> part.x
      | "m" -> part.m
      | "a" -> part.a
      | "s" -> part.s
      | _ -> failwith "illegal condition lhs"
    in
    fun (part : Part.t) ->
      let v = field_selector part in
      if op v rhs then result else Skip
  ;;

  let read str =
    if String.contains str ':'
    then (
      let lhs, rhs, dest = extract_rule_stuff str in
      let op = if String.contains str '>' then ( > ) else ( < ) in
      build_action lhs op rhs (parse_dest dest))
    else (
      let result = parse_dest str in
      fun _ -> result)
  ;;
end

let read_workflow ~f line =
  let splits = String.split ~on:'{' line in
  let label = List.nth_exn splits 0 in
  let rules =
    String.drop_suffix (List.nth_exn splits 1) 1 |> String.split ~on:',' |> List.map ~f
  in
  label, rules
;;

module Workflow = struct
  type t =
    { label : string
    ; rules : Rule.t list
    }

  let from_line line =
    let label, rules = read_workflow ~f:Rule.read line in
    { label; rules }
  ;;

  let apply workflow part =
    let rec aux rules =
      match rules with
      | [] -> failwith "workflow invalid for part"
      | r :: rs ->
        (match r part with
         | Rule.Skip -> aux rs
         | other -> other)
    in
    aux workflow.rules
  ;;
end

module RangeWorkflow = struct
  type t =
    { label : string
    ; rules : RangeRule.t list
    }

  let from_line line =
    let label, rules = read_workflow ~f:RangeRule.read line in
    { label; rules }
  ;;
end

let is_part_accepted part workflows =
  let rec aux current_workflow =
    match M.get current_workflow workflows with
    | Some wflw ->
      (match Workflow.apply wflw part with
       | Rule.Skip -> failwith "unreachable"
       | Rule.Move dest -> aux dest
       | Rule.Accepted -> true
       | Rule.Rejected -> false)
    | None -> failwith "workflow not found"
  in
  aux "in"
;;

let total_accepted_ranges range (workflow_map : RangeWorkflow.t M.t) =
  let rec aux acc queue =
    match queue with
    | [] -> acc
    | (_, []) :: qs -> aux acc qs
    | (p, r :: rs) :: qs ->
      (match r p with
       | RangeRule.Accepted (accepted, carry_on) ->
         aux (acc + RangePart.value accepted) ((carry_on, rs) :: qs)
       | RangeRule.Rejected (_, carry_on) -> aux acc ((carry_on, rs) :: qs)
       | RangeRule.Move (new_workflow, to_move, to_stay) ->
         let workflow = M.get new_workflow workflow_map |> Option.value_exn in
         let new_queue = (to_stay, rs) :: (to_move, workflow.rules) :: qs in
         aux acc new_queue)
  in
  let start_workflow = M.get "in" workflow_map |> Option.value_exn in
  aux 0 [ range, start_workflow.rules ]
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day19.txt" in
  let sections = Advent.Lists.partition_lines lines [] [] in
  let workflows = List.nth_exn sections 0 |> List.map ~f:Workflow.from_line in
  let workflow_map = M.of_list (List.map ~f:(fun w -> w.label, w) workflows) in
  let parts = List.nth_exn sections 1 |> List.map ~f:Part.read in
  let accepted_parts =
    List.filter ~f:(fun part -> is_part_accepted part workflow_map) parts
  in
  let total = List.map ~f:Part.value accepted_parts |> Advent.Lists.elt_sum in
  let _ = Printf.printf "Part One: %d\n" total in
  ()
;;

let _ =
  let open Ranges in
  let lines = Advent.Strings.read_lines "./inputs/day19.txt" in
  let sections = Advent.Lists.partition_lines lines [] [] in
  let workflows = List.nth_exn sections 0 |> List.map ~f:RangeWorkflow.from_line in
  let workflow_map = M.of_list (List.map ~f:(fun w -> w.label, w) workflows) in
  let initial_range = { start = 1; stop = 4001 } in
  let total =
    total_accepted_ranges
      { x = initial_range; m = initial_range; a = initial_range; s = initial_range }
      workflow_map
  in
  let _ = Printf.printf "Part Two: %d\n" total in
  ()
;;
