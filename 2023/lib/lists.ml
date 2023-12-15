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

let rec combinations k l =
  if k = 0
  then [ [] ]
  else (
    match l with
    | [] -> []
    | x :: xs ->
      List.map ~f:(fun tl -> x :: tl) (combinations (k - 1) xs) @ combinations k xs)
;;

(* Returns a list composed of the numerical difference between each adajcent element
   in the list. e.g. an input of [1,1,1] -> [0,0] where as [1,5;10] -> [4;5;5] *)
let elt_diffs list =
  List.fold_left ~init:[] ~f:(fun acc (a, b) -> acc @ [ b - a ]) (paired list)
;;

let elt_sum l =
  match List.reduce ~f:(fun x y -> x + y) l with
  | Some i -> i
  | None -> 0
;;

let all_same ~compare list =
  let fst = List.hd_exn list in
  if List.for_all ~f:(compare fst) list then Some fst else None
;;

let replace list idx v = List.mapi list ~f:(fun li l -> if equal_int li idx then v else l)

let replace_map list idx ~f =
  List.mapi list ~f:(fun li l -> if equal_int li idx then f li l else l)
;;
