open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let string_tail str n = String.sub str ~pos:n ~len:(String.length str - n)

(*
   Given a list of strings, this function will return (string list) list, splitting the original
   list based on blank lines
*)
let rec partition_lines lines building acc =
  match lines with
  | [] when List.length building = 0 -> acc
  | [] -> acc @ [ building ]
  | l :: ls when String.length l = 0 -> partition_lines ls [] (acc @ [ building ])
  | l :: ls -> partition_lines ls (building @ [ l ]) acc
;;

let paired lst =
  let rec aux acc remaining =
    match remaining with
    | [] | _ :: [] -> List.rev acc
    | hd :: tl :: rest -> aux ((hd, tl) :: acc) (tl :: rest)
  in
  aux [] lst
;;

let rec drop n l =
  match l with
  | [] -> []
  | _ when n = 0 -> l
  | _ :: xs -> drop (n - 1) xs
;;

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / gcd m n
;;

let unreachable () = failwith "unreachable"

module Searcher = struct
  type direction =
    | Forwards
    | Backwards

  let find_match pattern line direction =
    let searcher, offset =
      match direction with
      | Forwards -> Str.search_forward, 0
      | Backwards -> Str.search_backward, String.length line
    in
    try
      let index = searcher pattern line offset in
      let substr = Str.matched_group 0 line in
      Some (index, substr)
    with
    | _ -> None
  ;;
end

module CyclicIterator = struct
  type 'a cyclic_iterator =
    { mutable index : int
    ; elements : 'a list
    }

  let create_cyclic_iterator elements =
    if List.length elements = 0 then failwith "Empty list is not allowed";
    { index = 0; elements }
  ;;

  let next_value iterator =
    let value = List.nth iterator.elements iterator.index in
    iterator.index <- (iterator.index + 1) mod List.length iterator.elements;
    value
  ;;

  let create_infinite_iterator elements =
    let iterator = create_cyclic_iterator elements in
    fun () ->
      match next_value iterator with
      | Some v -> v
      | _ -> failwith ""
  ;;
end

module Ranges = struct
  (*
     Represents the numerical range for some category, used to determine if a specific
     identifier lands within the range, and for determining the offset to apply to
     the destination range (see mapping)
  *)
  type range =
    { start : int
    ; stop : int
    }
  [@@deriving show { with_path = false }]

  (* Given three ranges, 'identifier', 'source' and 'destination', this function
     will return a list of tuples which will:
     - Contain the source tuple if there was no overlap with the given destination range
     - Contains the destination range if the source range is contained entirely within it
     - Contain two ranges if the source range had a partial intersection with the destination range
     - Contain three ranges if the source range overlapped the destination range completely
  *)
  let process_ranges input_range source_range dest_range =
    if input_range.start > source_range.stop || input_range.stop < source_range.start
       (* Input not overlapping *)
    then [ input_range ]
    else if input_range.start >= source_range.start
            && input_range.stop <= source_range.stop
            (* Input within but not exceeding source *)
    then (
      let offset = input_range.start - source_range.start in
      let length = input_range.stop - input_range.start in
      [ { start = dest_range.start + offset; stop = dest_range.start + offset + length } ])
    else if input_range.start < source_range.start
            && input_range.stop <= source_range.stop
            (* Partial overlap w/ spill to left *)
    then (
      let overlap_amount = input_range.stop - source_range.start in
      [ { input_range with stop = source_range.start - 1 }
      ; { dest_range with stop = dest_range.start + overlap_amount }
      ])
    else if input_range.stop > source_range.stop
            && input_range.start >= source_range.start
            (* Partial overlap w/ spill to right *)
    then (
      let overlap_amount = source_range.stop - input_range.start in
      [ { dest_range with start = dest_range.stop - overlap_amount }
      ; { input_range with start = source_range.stop + 1 }
      ])
    else
      (* This bad boy overlaps on both sides of the source range *)
      [ { input_range with stop = source_range.start - 1 }
      ; dest_range
      ; { input_range with start = source_range.stop + 1 }
      ]
  ;;
end
