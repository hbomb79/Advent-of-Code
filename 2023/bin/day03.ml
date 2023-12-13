type cell_type =
  | Symbol of char
  | PartialPart of string
  | Empty

let string_of_char ch = String.make 1 ch

let parse_char ch =
  match ch with
  | '.' -> Empty
  | '0' .. '9' -> PartialPart (string_of_char ch)
  | s -> Symbol s
;;

type cell_bounds =
  { cell : cell_type
  ; x : int
  ; y : int
  ; width : int
  ; height : int
  }

let is_coordinate_inside_bounds coordinate bounds =
  let x_within_bounds =
    coordinate |> fst >= bounds.x && coordinate |> fst < bounds.x + bounds.width
  in
  let y_within_bounds =
    coordinate |> snd >= bounds.y && coordinate |> snd < bounds.y + bounds.height
  in
  x_within_bounds && y_within_bounds
;;

(*
   Iterates through the line provided until the idx reaches the end of the parts list.
   As it iterates, any numbers detected will be consumed and 'packed' in to a 'part', which
   consists of the part number and it's start/end index in the row (tracked by packing_idx as it
   seeks through the line).
*)
let rec extract_line ~row idx parts packing acc =
  match parts, packing with
  | [], None -> acc
  | [], Some part -> acc @ [ part ]
  | PartialPart hd :: tl, None ->
    let part = { cell = PartialPart hd; x = idx; y = row; width = 1; height = 1 } in
    extract_line ~row (idx + 1) tl (Some part) acc
  | hd :: tl, None ->
    let next = { cell = hd; x = idx; y = row; width = 1; height = 1 } in
    extract_line ~row (idx + 1) tl None (acc @ [ next ])
  | PartialPart hdStr :: tl, Some ({ cell = PartialPart str; _ } as part) ->
    let widened =
      { part with width = part.width + 1; cell = PartialPart (str ^ hdStr) }
    in
    extract_line ~row (idx + 1) tl (Some widened) acc
  | hd :: tl, Some part ->
    let next = { cell = hd; x = idx; y = row; width = 1; height = 1 } in
    extract_line ~row (idx + 1) tl None (acc @ [ part; next ])
;;

let parse_line ~row line =
  let cells = String.to_seq line |> List.of_seq |> List.map parse_char in
  extract_line ~row 0 cells None []
;;

module TupleSet = CCSet.Make (struct
    type t = int * int

    let compare = compare
  end)

(* Given a cell bounding box, this function extends it in each direction *)
let widen_part part =
  { part with
    width = part.width + 2
  ; x = part.x - 1
  ; y = part.y - 1
  ; height = part.height + 2
  }
;;

(*
   Extracts symbols and parts from the given cell_bounds list, returning a list of cells and a list
   of co-ordinates in a tuple (parts and symbol positions respectively)
*)
let extract_parts_and_symbols only_gears cells =
  List.fold_left
    (fun ((partAcc, symAcc) as acc) cell ->
      match cell with
      | { cell = Symbol _; x; y; _ } when only_gears = false -> partAcc, symAcc @ [ x, y ]
      | { cell = Symbol '*'; x; y; _ } when only_gears -> partAcc, symAcc @ [ x, y ]
      | { cell = PartialPart _; _ } as part -> partAcc @ [ widen_part part ], symAcc
      | _ -> acc)
    ([], [])
    cells
;;

let base =
  let lines = Advent.Strings.read_lines "./inputs/day03.txt" in
  List.mapi (fun i v -> parse_line ~row:i v) lines
  |> List.flatten
  |> extract_parts_and_symbols false
;;

(* Part One *)
let () =
  let parts, symbols = base in
  let symbol_position_set = TupleSet.of_list symbols in
  let valid =
    List.filter
      (fun part ->
        TupleSet.exists
          (fun pos -> is_coordinate_inside_bounds pos part)
          symbol_position_set)
      parts
  in
  let sum =
    List.fold_left
      (fun acc p ->
        match p with
        | { cell = PartialPart num; _ } -> acc + int_of_string num
        | _ -> raise (Failure "Invalid cell found in valid parts"))
      0
      valid
  in
  let _ = Printf.printf "%d is the sum of valid parts\n" sum in
  ()
;;

(* Part Two *)
let () =
  let parts, symbols = base in
  let summed_gear_ratios =
    List.fold_left
      (fun acc sym ->
        let matches = List.filter (is_coordinate_inside_bounds sym) parts in
        match List.length matches with
        | 2 ->
          acc
          + List.fold_left
              (fun acc cell ->
                match cell with
                | { cell = PartialPart num; _ } -> acc * int_of_string num
                | _ -> acc)
              1
              matches
        | _ -> acc)
      0
      symbols
  in
  let _ = Printf.printf "%d is the sum of gear rations\n" summed_gear_ratios in
  ()
;;
