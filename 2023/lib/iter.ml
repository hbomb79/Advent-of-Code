open Core

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
