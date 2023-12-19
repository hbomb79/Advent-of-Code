open Core

type result =
  | Accepted
  | Rejected
  | Move of string
  | Skip

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

module Rule = struct
  type t = Part.t -> result

  let parse_condition_result str =
    match str with
    | "R" -> Rejected
    | "A" -> Accepted
    | label -> Move label
  ;;

  let parse_condition lhs (op : int -> int -> bool) rhs result =
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

  let parse_condition_rule rule op =
    let splits = String.split ~on:':' rule in
    let cond = List.nth_exn splits 0 in
    let result = List.nth_exn splits 1 |> parse_condition_result in
    let parts = String.split_on_chars cond ~on:[ '<'; '>' ] in
    let lhs = List.nth_exn parts 0 in
    let rhs = List.nth_exn parts 1 in
    parse_condition lhs op (int_of_string rhs) result
  ;;

  let read str =
    let gt = String.contains str '>' in
    let lt = String.contains str '<' in
    if gt
    then parse_condition_rule str ( > )
    else if lt
    then parse_condition_rule str ( < )
    else (
      let result = parse_condition_result str in
      fun _ -> result)
  ;;
end

module Workflow = struct
  type t =
    { label : string
    ; rules : Rule.t list
    }

  let from_line line =
    let splits = String.split ~on:'{' line in
    let label = List.nth_exn splits 0 in
    let rules =
      String.drop_suffix (List.nth_exn splits 1) 1
      |> String.split ~on:','
      |> List.map ~f:Rule.read
    in
    { label; rules }
  ;;

  let apply workflow part =
    let rec aux rules =
      match rules with
      | [] -> failwith "workflow invalid for part"
      | r :: rs ->
        (match r part with
         | Skip -> aux rs
         | other -> other)
    in
    aux workflow.rules
  ;;
end

module M = CCMap.Make (String)

let apply_workflows part workflows =
  let rec aux current_workflow =
    match M.get current_workflow workflows with
    | None -> failwith (Printf.sprintf "workflow '%s' not found" current_workflow)
    | Some wflw ->
      (match Workflow.apply wflw part with
       | Skip -> failwith "unreachable"
       | Move dest -> aux dest
       | Accepted -> true
       | Rejected -> false)
  in
  aux "in"
;;

let _ =
  let lines = Advent.Strings.read_lines "./inputs/day19.txt" in
  let sections = Advent.Lists.partition_lines lines [] [] in
  let workflows = List.nth_exn sections 0 |> List.map ~f:Workflow.from_line in
  let workflow_map = M.of_list (List.map ~f:(fun w -> w.label, w) workflows) in
  let parts = List.nth_exn sections 1 |> List.map ~f:Part.read in
  let accepted_parts =
    List.filter ~f:(fun part -> apply_workflows part workflow_map) parts
  in
  let total = List.map ~f:Part.value accepted_parts |> Advent.Lists.elt_sum in
  let _ = Printf.printf "Part One: %d\n" total in
  ()
;;
