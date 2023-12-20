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

let process_ranges input_range source_range dest_range =
  if input_range.start > input_range.stop
  then failwith "range cannot have a stop index greater than it's start"
  else if input_range.start > source_range.stop || input_range.stop < source_range.start
          (* Input not overlapping *)
  then None
  else if input_range.start >= source_range.start && input_range.stop <= source_range.stop
          (* Input within but not exceeding source *)
  then (
    let offset = input_range.start - source_range.start in
    let length = input_range.stop - input_range.start in
    Some
      [ { start = dest_range.start + offset; stop = dest_range.start + offset + length } ])
  else if input_range.start < source_range.start && input_range.stop <= source_range.stop
          (* Partial overlap w/ spill to left *)
  then (
    let overlap_amount = input_range.stop - source_range.start in
    Some
      [ { input_range with stop = source_range.start - 1 }
      ; { dest_range with stop = dest_range.start + overlap_amount }
      ])
  else if input_range.stop > source_range.stop && input_range.start >= source_range.start
          (* Partial overlap w/ spill to right *)
  then (
    let overlap_amount = source_range.stop - input_range.start in
    Some
      [ { dest_range with start = dest_range.stop - overlap_amount }
      ; { input_range with start = source_range.stop + 1 }
      ])
  else
    (* This bad boy overlaps on both sides of the source range *)
    Some
      [ { input_range with stop = source_range.start - 1 }
      ; dest_range
      ; { input_range with start = source_range.stop + 1 }
      ]
;;

let value range = range.stop - range.start
