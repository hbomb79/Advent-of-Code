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
end

module RangeRule = struct
  type result =
    (* fst = range which was accepted, snd = range part that was not adjusted by this rule *)
    | Accepted of RangePart.t * RangePart.t
      (* fst = range that was rejected by this rule, snd = range that was not affected by this rule *)
    | Rejected of RangePart.t * RangePart.t
      (* fst = new workflow, snd = range which should be handled by new workflow, third = range which was no impacted *)
    | Move of string * RangePart.t * RangePart.t

  type t = RangePart.t -> result

  (* Given a range and a n value, returns back the part of the range which DID fall within this range, and the part that didn't.

     NB: Zero value ranges are the expected output of this function if the range doesn't contain the value given
  *)
  let range_gt n range =
    let open Ranges in
    if range.stop <= n
    then (* This range does not contain 'n' at all *)
      { start = 0; stop = 0 }, range
    else if range.start <= n
    then (* Split the range *)
      { range with start = n + 1 }, { range with stop = n + 1 }
    else (* Entire range sits ahead of 'n' *)
      range, { start = 0; stop = 0 }
  ;;

  (* Same as range_gt, but swapped around for where the range is < n *)
  let range_lt n range =
    let open Ranges in
    if range.stop < n
    then (* Entire range sits before 'n' *)
      range, { start = 0; stop = 0 }
    else if range.start < n
    then (* Split the range *)
      { range with stop = n }, { range with start = n }
    else (* Entire range sits ahead of 'n' *)
      { start = 0; stop = 0 }, range
  ;;

  (* returns a function which will take a ranged-point, select the specific
     field/range we're interested in, and pass it through the range function,
     before returning back two ranges (fst is what DID get accepted by the range, and the second
     is what fell outside the range *)
  let apply_range_to_part_fun range_fun field (part : RangePart.t) =
    let prange =
      match field with
      | "x" -> part.x
      | "m" -> part.m
      | "a" -> part.a
      | "s" -> part.s
      | _ -> failwith "unreachable"
    in
    let inside, outside = range_fun prange in
    match field with
    | "x" -> { part with x = inside }, { part with x = outside }
    | "m" -> { part with m = inside }, { part with m = outside }
    | "a" -> { part with a = inside }, { part with a = outside }
    | "s" -> { part with s = inside }, { part with s = outside }
    | _ -> failwith "unreachable"
  ;;

  let get_cond_fun str =
    let target = List.nth (String.split ~on:':' str) 1 |> Option.value ~default:str in
    match target with
    | "A" -> fun (a, b) -> Accepted (a, b)
    | "R" -> fun (a, b) -> Rejected (a, b)
    | other -> fun (a, b) -> Move (other, a, b)
  ;;

  let get_range_fun str =
    let open Advent.Ranges in
    let gt = String.contains str '>' in
    let lt = String.contains str '<' in
    if gt
    then (
      let cond = List.nth_exn (String.split ~on:'>' str) 1 in
      let field = List.nth_exn (String.split ~on:'>' str) 0 in
      let condn = List.nth_exn (String.split ~on:':' cond) 0 in
      range_gt (int_of_string condn), Some field)
    else if lt
    then (
      let cond = List.nth_exn (String.split ~on:'<' str) 1 in
      let field = List.nth_exn (String.split ~on:'<' str) 0 in
      let condn = List.nth_exn (String.split ~on:':' cond) 0 in
      range_lt (int_of_string condn), Some field)
    else (fun r -> r, { start = 0; stop = 0 }), None
  ;;

  let read str =
    let open Ranges in
    let rfun, field = get_range_fun str in
    (* let field = List.nth (String.split ~on:':' str) 1 in *)
    let cond_fun = get_cond_fun str in
    match field with
    | Some f ->
      (* let _ = Printf.printf "\t- Applies to field %s\n" f in *)
      fun p -> apply_range_to_part_fun rfun f p |> cond_fun
    | None ->
      let empty_range = { start = 0; stop = 0 } in
      (* let _ = Printf.printf "\t- Applies to field ALL\n" in *)
      fun p ->
        cond_fun
          (p, { x = empty_range; m = empty_range; a = empty_range; s = empty_range })
  ;;
end

module RangeWorkflow = struct
  type t =
    { label : string
    ; rules : RangeRule.t list
    }

  let from_line line =
    let splits = String.split ~on:'{' line in
    let label = List.nth_exn splits 0 in
    let rules =
      String.drop_suffix (List.nth_exn splits 1) 1
      |> String.split ~on:','
      |> List.map ~f:RangeRule.read
    in
    { label; rules }
  ;;
end

let apply_workflows' range workflow_map =
  let rec aux acc queue =
    match queue with
    | [] -> acc
    | (p, []) :: qs when RangePart.value p = 0 ->
      (* This point has no more rules to apply *)
      aux acc qs
    | (_, []) :: _ ->
      failwith "A non-zero range reached the end of it's workflow's rules..."
    | (p, r :: rs) :: qs ->
      (match r p with
       | RangeRule.Accepted (accepted, carry_on) ->
         (* Count the size of the accepted range, and recurse again with
            the 'carry_on' points tupled with the next rule from the workflow *)
         aux (acc + RangePart.value accepted) ((carry_on, rs) :: qs)
       | RangeRule.Rejected (rejected, carry_on) ->
         let _ = Printf.printf "Rejected range of size %d\n" (RangePart.value rejected) in
         aux acc ((carry_on, rs) :: qs)
       | RangeRule.Move (new_workflow, to_move, to_stay) ->
         (* The 'to_move' range is being moved to a new workflow for processing, fetch that workflows
            rules now and add this range to the queue with the rules from the aforementioned workflow *)
         let (workflow : RangeWorkflow.t) =
           M.get new_workflow workflow_map |> Option.value_exn
         in
         let new_queue = (to_stay, rs) :: (to_move, workflow.rules) :: qs in
         aux acc new_queue)
  in
  let start_workflow = M.get "in" workflow_map |> Option.value_exn in
  aux 0 [ range, start_workflow.rules ]
;;

module Rule = struct
  type result =
    | Accepted
    | Rejected
    | Move of string
    | Skip

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
         | Rule.Skip -> aux rs
         | other -> other)
    in
    aux workflow.rules
  ;;
end

let apply_workflows part workflows =
  let rec aux current_workflow =
    match M.get current_workflow workflows with
    | None -> failwith (Printf.sprintf "workflow '%s' not found" current_workflow)
    | Some wflw ->
      (match Workflow.apply wflw part with
       | Rule.Skip -> failwith "unreachable"
       | Rule.Move dest -> aux dest
       | Rule.Accepted -> true
       | Rule.Rejected -> false)
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

let _ =
  let open Ranges in
  let lines = Advent.Strings.read_lines "./inputs/day19.txt" in
  let sections = Advent.Lists.partition_lines lines [] [] in
  let workflows = List.nth_exn sections 0 |> List.map ~f:RangeWorkflow.from_line in
  let workflow_map = M.of_list (List.map ~f:(fun w -> w.label, w) workflows) in
  let initial_range = { start = 1; stop = 4001 } in
  let total =
    apply_workflows'
      { x = initial_range; m = initial_range; a = initial_range; s = initial_range }
      workflow_map
  in
  let _ = Printf.printf "Part Two: %d\n" total in
  ()
;;
