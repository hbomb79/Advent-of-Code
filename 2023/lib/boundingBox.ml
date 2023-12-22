open Core

type t =
  { x1 : int
  ; y1 : int
  ; z1 : int
  ; x2 : int
  ; y2 : int
  ; z2 : int
  }
[@@deriving show { with_path = false }]

let from_string str =
  let splits = Strings.extract_numbers str |> List.nth_exn in
  { x1 = splits 0
  ; y1 = splits 1
  ; z1 = splits 2
  ; x2 = splits 3
  ; y2 = splits 4
  ; z2 = splits 5
  }
;;

let does_intersect a b =
  a.x1 <= b.x2
  && a.x2 >= b.x1
  && a.y1 <= b.y2
  && a.y2 >= b.y1
  && a.z1 <= b.z2
  && a.z2 >= b.z1
;;

let compare_z a b = compare_int a.z1 b.z1
let shift_z b n = { b with z1 = b.z1 + n; z2 = b.z2 + n }
