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
