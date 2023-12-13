open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let string_tail str n = String.sub str ~pos:n ~len:(String.length str - n)

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
