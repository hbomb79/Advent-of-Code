open Core

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
