open Core
module M = CCMap.Make (String)

type pulse =
  | Lo
  | Hi
[@@deriving variants, show { with_path = false }]

type module_type =
  | Broadcaster of string list
  | Toggle of string list * bool
  | Conjunction of string list * pulse M.t

(*
   Accepts a list of strings and returns back a map name -> module_type. The memory of
   the returned modules will be initialized, such that all Toggle junctions start with memory
   of false, and all conjunction modules start with EMPTY memory (it should be assumed to be false
   for any given module name that does not exist, as this is the intentional starting state).
*)
let read_modules lines =
  (* First, read in all module names/outputs in to a list which we can use to create a map *)
  let mods =
    List.map
      ~f:(fun line ->
        let split = Str.split (Str.regexp " -> ") line |> List.nth_exn in
        let label = split 0 in
        let t = String.sub label ~pos:0 ~len:1 in
        let name = String.drop_prefix label 1 in
        let outputs = split 1 |> Str.split (Str.regexp ", ") in
        match t with
        | "%" -> name, Toggle (outputs, false)
        | "&" -> name, Conjunction (outputs, M.of_list [])
        | _ when equal_string label "broadcaster" -> "broadcaster", Broadcaster outputs
        | _ -> failwith "unknown line")
      lines
    |> M.of_list
  in
  (* Now iterate through each module, and find Conjunction modules, update the module
     to contain a memory entry for all modules pointed at it *)
  M.fold
    (fun key value acc ->
      match value with
      | Conjunction (outputs, memory) ->
        let inputs =
          M.filter
            (fun k v ->
              let outputs =
                match v with
                | Broadcaster outputs -> outputs
                | Toggle (outputs, _) -> outputs
                | Conjunction (outputs, _) when not (equal_string key k) -> outputs
                | _ -> []
              in
              List.mem outputs key ~equal:equal_string)
            acc
        in
        let input_names = M.to_list inputs |> List.map ~f:(fun (k, _) -> k, Lo) in
        let memory' = M.add_list memory input_names in
        M.add key (Conjunction (outputs, memory')) acc
      | _ -> acc)
    mods
    mods
;;

(* let output_to modules targets pulse = *)

let fire_pulse (modules : module_type M.t) =
  let rec aux lo_acc hi_acc mods queue =
    match queue with
    | [] -> lo_acc, hi_acc, mods
    | (origin, target, pulse) :: qs ->
      let lo_acc', hi_acc' =
        match pulse with
        | Lo -> lo_acc + 1, hi_acc
        | Hi -> lo_acc, hi_acc + 1
      in
      (match M.get target mods with
       | Some (Toggle (outputs, memory)) when is_lo pulse ->
         (* Toggle has received a LO pulse, update it's memory (flip it) and queue a pulse
            with the new Hi/Lo state as it's payload to all the outputs *)
         let memory' = not memory in
         let mods' = M.add target (Toggle (outputs, memory')) mods in
         let outgoing = if memory' then Hi else Lo in
         let pulses_to_send =
           List.map ~f:(fun output -> target, output, outgoing) outputs
         in
         aux lo_acc' hi_acc' mods' (qs @ pulses_to_send)
       | Some (Conjunction (outputs, memory)) ->
         (* Conjunction module has received a pulse. Update it's memory to contain the new
            last known state of one of it's inputs (origin). Also queue a message to all output nodes
            with a payload of Lo if all it's memory values are TRUE, otherwise Hi *)
         let memory' = M.add origin pulse memory in
         let mods' = M.add target (Conjunction (outputs, memory')) mods in
         let outgoing =
           if M.for_all (fun _ pulse -> is_hi pulse) memory' then Lo else Hi
         in
         let pulses_to_send =
           List.map ~f:(fun output -> target, output, outgoing) outputs
         in
         aux lo_acc' hi_acc' mods' (qs @ pulses_to_send)
       | _ -> aux lo_acc' hi_acc' mods qs)
  in
  match M.find "broadcaster" modules with
  | Broadcaster outputs ->
    aux 0 0 modules (List.map ~f:(fun out -> "broadcaster", out, Lo) outputs)
  | _ -> failwith "illegal"
;;

let do_this_shit_thousand_times modules =
  let rec aux n lo_acc hi_acc mods_acc =
    match n with
    | 0 -> lo_acc, hi_acc
    | _ ->
      let lo, hi, mods = fire_pulse mods_acc in
      aux (n - 1) (lo_acc + lo) (hi_acc + hi) mods
  in
  aux
    1000
    1000 (* initial 1000 lo_acc to represent the 1000 lo pulses from the button *)
    0
    modules
;;

let _ =
  let mods = Advent.Strings.read_lines "./inputs/day20.txt" |> read_modules in
  let lo, hi = do_this_shit_thousand_times mods in
  let _ = Printf.printf "Part One: %d (%d * %d)\n" (lo * hi) lo hi in
  ()
;;

(* 828731520 too high *)
