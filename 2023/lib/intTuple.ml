type t = int * int

let compare (x1, y1) (x2, y2) =
  match compare x1 x2 with
  | 0 -> compare y1 y2
  | c -> c
;;
