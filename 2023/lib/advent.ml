open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let string_tail str n = String.sub str ~pos:n ~len:(String.length str - n)

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
