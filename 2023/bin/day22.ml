open Core

module Brick = struct
  open Advent.BoundingBox

  type t = int * Advent.BoundingBox.t

  let box b = snd b
  let id b = fst b
  let equal a b = equal_int (id a) (id b)
  let compare a b = compare_int (id a) (id b)
  let read id str = id, Advent.BoundingBox.from_string str
  let compare_z a b = Advent.BoundingBox.compare_z (box a) (box b)
  let move_down b = id b, Advent.BoundingBox.shift_z (box b) (-1)

  let shift_above b =
    let inner = box b in
    id b, { inner with z1 = inner.z2 + 1; z2 = inner.z2 + 1 }
  ;;

  let shift_below b =
    let inner = box b in
    id b, { inner with z1 = inner.z1 - 1; z2 = inner.z1 - 1 }
  ;;

  let filter_intersections boxes (_, b) =
    List.filter ~f:(fun (_, cand) -> does_intersect b cand) boxes
  ;;

  let any_intersections boxes (_, b) =
    List.find ~f:(fun (_, cand) -> does_intersect b cand) boxes |> is_some
  ;;
end

open Brick
module S = CCSet.Make (Brick)

let remove_box boxes box = List.filter ~f:(fun b -> not (Brick.equal b box)) boxes

let apply_gravity boxes =
  let rec aux queue displaced settled =
    match queue with
    | [] -> displaced, settled
    | q :: qs ->
      let lowered = move_down q in
      (match (box lowered).z1 > 0 && not (any_intersections settled lowered) with
       | true -> aux (lowered :: qs) (S.add q displaced) settled
       | false -> aux qs displaced (q :: settled))
  in
  aux (List.sort ~compare:compare_z boxes) (S.of_list []) []
;;

let can_remove boxes box =
  let boxes' = remove_box boxes box in
  let intersecting = filter_intersections boxes' (shift_above box) in
  List.for_all ~f:(fun b -> shift_below b |> any_intersections boxes') intersecting
;;

let how_many_fell_after_removing_this boxes box =
  remove_box boxes box |> apply_gravity |> fst |> S.to_list |> List.length
;;

let _ =
  let _, blocks =
    Advent.Strings.read_lines "./inputs/day22.txt" |> List.mapi ~f:read |> apply_gravity
  in
  let _ =
    List.filter ~f:(fun b -> can_remove blocks b) blocks
    |> List.length
    |> Printf.printf "Part One: %d\n"
  in
  let _ =
    List.map ~f:(fun b -> how_many_fell_after_removing_this blocks b) blocks
    |> Advent.Lists.elt_sum
    |> Printf.printf "Part two: %d\n"
  in
  ()
;;
