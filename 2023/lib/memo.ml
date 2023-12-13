open Core

(* Generic memoization wrapper around a given function, reutnring the existing
   mapping for a given input if known, else calculating it and storing it *)
let memo_rec k f =
  let h = Hashtbl.create k in
  let rec g x =
    match Hashtbl.find h x with
    | Some result -> result
    | None ->
      let result = f g x in
      Hashtbl.set h ~key:x ~data:result;
      result
  in
  g
;;
