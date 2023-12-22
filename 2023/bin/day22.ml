open Core

module BoundingBox = struct
  type t =
    { label : int
    ; x1 : int
    ; y1 : int
    ; z1 : int
    ; x2 : int
    ; y2 : int
    ; z2 : int
    }
  [@@deriving show { with_path = false }]

  let equal a b = equal_int a.label b.label
  let compare a b = compare_int a.label b.label

  let read i str =
    let splits = Advent.Strings.extract_numbers str |> List.nth_exn in
    let x1 = splits 0 in
    let y1 = splits 1 in
    let z1 = splits 2 in
    { label = i; x1; y1; z1; x2 = splits 3; y2 = splits 4; z2 = splits 5 }
  ;;

  let intersects a b =
    a.x1 <= b.x2
    && a.x2 >= b.x1
    && a.y1 <= b.y2
    && a.y2 >= b.y1
    && a.z1 <= b.z2
    && a.z2 >= b.z1
  ;;

  let compare_z a b = compare_int a.z1 b.z1
  let move_down b = { b with z1 = b.z1 - 1; z2 = b.z2 - 1 }
  let intersections boxes b = List.filter ~f:(fun cand -> intersects b cand) boxes

  let does_intersect boxes b =
    List.find ~f:(fun cand -> intersects b cand) boxes |> is_some
  ;;
end

module S = CCSet.Make (BoundingBox)

let apply_gravity boxes =
  let rec aux queue dropped settled =
    match queue with
    | [] -> dropped, settled
    | q :: qs ->
      let lowered = BoundingBox.move_down q in
      let ok = lowered.z1 > 0 && not (BoundingBox.does_intersect settled lowered) in
      if ok
      then
        (* This box (q) lowered is OK... re-add to the queue *)
        aux (lowered :: qs) (S.add q dropped) settled
      else (* This box could not be lowered any more *)
        aux qs dropped (q :: settled)
  in
  aux (List.sort ~compare:BoundingBox.compare_z boxes) (S.of_list []) []
;;

let can_remove boxes box =
  (* Are there any boxes ABOVE this point? If there are no boxes directly above it, then
     we know it's not 'supporting' anything, and can be safely disintegrated *)
  let open BoundingBox in
  let other_boxes = List.filter ~f:(fun b -> not (BoundingBox.equal b box)) boxes in
  let above_box =
    { label = -1
    ; x1 = box.x1
    ; x2 = box.x2
    ; y1 = box.y1
    ; y2 = box.y2
    ; z1 = box.z2 + 1
    ; z2 = box.z2 + 1
    }
  in
  let any_above = BoundingBox.intersections other_boxes above_box in
  if List.is_empty any_above
  then true
  else
    (* Hm, there does seem to be 1 or more boxes above this, check if there any boxes BELOW each box (except
       for us of course)? If this is true for ALL, then good news, we can disintegrate *)
    List.for_all
      ~f:(fun above ->
        let below_box =
          { label = -1
          ; x1 = above.x1
          ; x2 = above.x2
          ; y1 = above.y1
          ; y2 = above.y2
          ; z1 = above.z1 - 1
          ; z2 = above.z1 - 1
          }
        in
        BoundingBox.does_intersect other_boxes below_box)
      any_above
;;

let how_many_fell_after_removing_this boxes box =
  let boxes' = List.filter ~f:(fun b -> not (BoundingBox.equal b box)) boxes in
  let fell, _ = apply_gravity boxes' in
  S.to_list fell |> List.length
;;

let _ =
  let _, blocks =
    Advent.Strings.read_lines "./inputs/day22.txt"
    |> List.mapi ~f:BoundingBox.read
    |> apply_gravity
  in
  let can_remove = List.filter ~f:(fun b -> can_remove blocks b) blocks |> List.length in
  let _ = Printf.printf "Part One: %d\n" can_remove in
  let will_fall =
    List.map ~f:(fun b -> how_many_fell_after_removing_this blocks b) blocks
    |> Advent.Lists.elt_sum
  in
  let _ = Printf.printf "Part two: %d\n" will_fall in
  ()
;;
