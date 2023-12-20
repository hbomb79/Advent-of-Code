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

let broadcaster = "broadcaster"

let module_outputs m =
  match m with
  | Broadcaster outputs -> outputs
  | Toggle (outputs, _) -> outputs
  | Conjunction (outputs, _) -> outputs
;;

let filter_modules modules ~f =
  M.filter (fun _ v -> List.find (module_outputs v) ~f |> is_some) modules |> M.to_list
;;

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
        | _ when equal_string label broadcaster -> broadcaster, Broadcaster outputs
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
        let input_names =
          M.filter (fun _k v -> List.mem (module_outputs v) key ~equal:equal_string) acc
          |> M.to_list
          |> List.map ~f:(fun (k, _) -> k, Lo)
        in
        let memory' = M.add_list memory input_names in
        M.add key (Conjunction (outputs, memory')) acc
      | _ -> acc)
    mods
    mods
;;

let sim_pulse modules (origin, target, pulse) =
  match M.get target modules with
  | Some (Toggle (outputs, memory)) when is_lo pulse ->
    (* Toggle has received a LO pulse, update it's memory (flip it) and queue a pulse
       with the new Hi/Lo state as it's payload to all the outputs *)
    let memory' = not memory in
    let mods' = M.add target (Toggle (outputs, memory')) modules in
    let outgoing = if memory' then Hi else Lo in
    let pulses_to_send = List.map ~f:(fun output -> target, output, outgoing) outputs in
    mods', pulses_to_send
  | Some (Conjunction (outputs, memory)) ->
    (* Conjunction module has received a pulse. Update it's memory to contain the new
       last known state of one of it's inputs (origin). Also queue a message to all output nodes
       with a payload of Lo if all it's memory values are TRUE, otherwise Hi *)
    let memory' = M.add origin pulse memory in
    let mods' = M.add target (Conjunction (outputs, memory')) modules in
    let outgoing = if M.for_all (fun _ pulse -> is_hi pulse) memory' then Lo else Hi in
    let pulses_to_send = List.map ~f:(fun output -> target, output, outgoing) outputs in
    mods', pulses_to_send
  | _ -> modules, []
;;

let get_broadcast_messages modules =
  match M.find broadcaster modules with
  | Broadcaster outputs -> List.map ~f:(fun out -> broadcaster, out, Lo) outputs
  | _ -> failwith "illegal broadcaster module_type"
;;

let press_button modules =
  let rec aux lo_acc hi_acc mods queue =
    match queue with
    | [] -> lo_acc, hi_acc, mods
    | (origin, target, pulse) :: qs ->
      let lo_acc', hi_acc' =
        match pulse with
        | Lo -> lo_acc + 1, hi_acc
        | Hi -> lo_acc, hi_acc + 1
      in
      let mods', new_pulses = sim_pulse mods (origin, target, pulse) in
      aux lo_acc' hi_acc' mods' (qs @ new_pulses)
  in
  aux 0 0 modules (get_broadcast_messages modules)
;;

let find_cycles modules feeder all_seen all_intervals count =
  let rec aux mods queue seen intervals =
    match queue with
    | [] -> seen, intervals, mods
    | ((origin, target, pulse) as p) :: qs ->
      let seen', intervals' =
        if equal_string target feeder && is_hi pulse
        then (
          (* Increase the counter for how many times we've seen this particular module (origin) fire Hi *)
          let s = M.add origin (M.find origin seen + 1) seen in
          let i =
            if M.mem origin intervals then intervals else M.add origin count intervals
          in
          s, i)
        else seen, intervals
      in
      let mods', new_pulses = sim_pulse mods p in
      aux mods' (qs @ new_pulses) seen' intervals'
  in
  aux modules (get_broadcast_messages modules) all_seen all_intervals
;;

let find_interval_cycle modules =
  (* Find the node pointing to 'rx' (only one in my input), and all the modules which are pointing to 'target'...
     The 'feeders' are all Conjunction modules (in my input) and so if we can find the frequency
     at which these individually emit HI, and then find the lcm of the frequencies for all the feeders,
     we know how many cycles we'd need for these to all emit Hi to the 'target' (and therefore,
     cause a Lo pulse to be emitted by 'target' to 'rx')
  *)
  let target = filter_modules modules ~f:(equal_string "rx") |> List.hd_exn |> fst in
  let feeders = filter_modules modules ~f:(equal_string target) in
  let required_seen = List.map ~f:(fun (k, _) -> k, 0) feeders in
  let rec aux count mods_acc seen intervals =
    match find_cycles mods_acc target seen intervals count with
    | seen', intervals', _ when M.for_all (fun _ v -> v > 1) seen' -> M.to_list intervals'
    | seen', intervals', mods' -> aux (count + 1) mods' seen' intervals'
  in
  aux 1 modules (M.of_list required_seen) (M.of_list [])
;;

let repeat_sim modules n =
  let rec aux n lo_acc hi_acc mods_acc =
    match n with
    | 0 -> lo_acc * hi_acc
    | _ ->
      let lo, hi, mods = press_button mods_acc in
      aux (n - 1) (lo_acc + lo) (hi_acc + hi) mods
  in
  aux n n 0 modules
;;

let _ =
  let mods = Advent.Strings.read_lines "./inputs/day20.txt" |> read_modules in
  let _ = Printf.printf "Part One: %d\n" (repeat_sim mods 1000) in
  let intervals = find_interval_cycle mods |> List.map ~f:(fun (_, v) -> v) in
  let freq =
    List.fold_left
      ~f:(fun acc n -> Advent.Math.lcm acc n)
      ~init:(List.nth_exn intervals 0)
      (Core.List.drop intervals 1)
  in
  let _ = Printf.printf "Part Two: %d\n" freq in
  ()
;;
