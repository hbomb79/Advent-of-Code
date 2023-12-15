open Core

let extract_numbers str =
  let number_regex = Str.regexp "\\b\\([0-9]+\\)\\b" in
  let rec aux acc start_pos =
    try
      let _ = Str.search_forward number_regex str start_pos in
      let number = int_of_string (Str.matched_group 1 str) in
      aux (number :: acc) (Str.match_end ())
    with
    | _ -> List.rev acc
  in
  aux [] 0
;;

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let string_tail str n = String.sub str ~pos:n ~len:(String.length str - n)
let make_from_ints ints = (List.map ~f:string_of_int) ints |> String.concat ~sep:", "

(* Reads the given list of lines by passing each character through ~f, which must
   return something of type 'a option. If the any 'None' response is simply filtered out, where
   as 'Some' responses are tupled together with their (x,y) position in the 'grid' of lines *)
let read_option_chars_as_grid lines ~f =
  let height = List.length lines in
  let read_line y line ~f =
    List.filter_mapi
      ~f:(fun x ch -> f ch |> Option.map ~f:(fun r -> (x, y), r))
      (String.to_list line)
  in
  let rec aux points y =
    match y with
    | y when y = height -> points
    | _ ->
      let l = List.nth_exn lines y in
      aux (points @ read_line y l ~f) (y + 1)
  in
  aux [] 0
;;

let read_chars_as_grid lines ~f =
  let height = List.length lines in
  let read_line y line ~f =
    List.mapi
      ~f:(fun x ch ->
        let r = f ch in
        (x, y), r)
      (String.to_list line)
  in
  let rec aux points y =
    match y with
    | y when y = height -> points
    | _ ->
      let l = List.nth_exn lines y in
      aux (points @ read_line y l ~f) (y + 1)
  in
  aux [] 0
;;
