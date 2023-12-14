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
