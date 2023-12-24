open Core

let float_gt a b = compare_float a b > 0
let float_gte a b = compare_float a b >= 0
let float_lt a b = compare_float a b < 0
let float_lte a b = compare_float a b <= 0

module Hailstone = struct
  (* Tuple of position (x,y,z) and velocity [constant] (x,y,z) per ns *)
  type t = (float * float * float) * (float * float * float)
  [@@deriving show, compare, equal]

  let get_x ((x', _, _), _) = x'
  let get_y ((_, y', _), _) = y'
  let get_z ((_, _, z'), _) = z'
  let get_dx (_, (dx', _, _)) = dx'
  let get_dy (_, (_, dy', _)) = dy'
  let get_dz (_, (_, _, dz')) = dz'

  (* Linear equation standard form ax + by = c helpers *)
  let get_a (_, (_, vy, _)) = vy
  let get_b (_, (vx, _, _)) = vx *. -1.
  let get_c ((sx, sy, _), (vx, vy, _)) = (vy *. sx) -. (vx *. sy)

  let get_intersection a b =
    let a1, b1, c1 = get_a a, get_b a, get_c a in
    let a2, b2, c2 = get_a b, get_b b, get_c b in
    let d = (a1 *. b2) -. (a2 *. b1) in
    let x = ((c1 *. b2) -. (c2 *. b1)) /. d in
    let y = ((c2 *. a1) -. (c1 *. a2)) /. d in
    x, y
  ;;

  let get_future_intersection a b =
    let x, y = get_intersection a b in
    if float_gte ((x -. get_x a) *. get_dx a) 0.
       && float_gte ((y -. get_y a) *. get_dy a) 0.
       && float_gte ((x -. get_x b) *. get_dx b) 0.
       && float_gte ((y -. get_y b) *. get_dy b) 0.
    then Some (x, y)
    else None
  ;;

  let is_parallel a b =
    let a1 = get_a a in
    let b1 = get_b a in
    let a2 = get_a b in
    let b2 = get_b b in
    equal_float (a1 *. b2) (b1 *. a2)
  ;;

  let from_string str =
    match Advent.Strings.extract_numbers str |> List.map ~f:float_of_int with
    | [ px; py; pz; vx; vy; vz ] -> (px, py, pz), (vx, vy, vz)
    | _ -> failwith "invalid string"
  ;;
end

let _ =
  let load_test = false in
  let path, mi, ma =
    if load_test
    then "inputs/day24-test.txt", 7.0, 27.0
    else "inputs/day24.txt", 200000000000000.0, 400000000000000.0
  in
  let hailstones = Advent.Strings.read_lines path |> List.map ~f:Hailstone.from_string in
  let perms = Advent.Lists.combinations 2 hailstones in
  let count =
    List.fold_left
      ~init:0
      ~f:(fun acc combination ->
        match combination with
        | [ a; b ] ->
          if not (Hailstone.is_parallel a b)
          then
            let open Hailstone in
            match get_future_intersection a b with
            | Some (x, y)
              when float_lt x mi || float_gt x ma || float_lt y mi || float_gt y ma -> acc
            | Some _ -> acc + 1
            | None -> acc
          else acc
        | _ -> failwith "invalid combination")
      perms
  in
  let _ = Printf.printf "Part One: %d\n" count in
  (* For part two we'll use Z3, which is a theorem solver which will massively simplify
     this task... Hopefully.

     https://github.com/Z3prover/z3
  *)
  let mk_fresh_int_var ctx name =
    Z3.Expr.mk_const_s ctx name (Z3.Arithmetic.Integer.mk_sort ctx)
  in
  let ctx = Z3.mk_context [] in
  let x = mk_fresh_int_var ctx "x" in
  let y = mk_fresh_int_var ctx "y" in
  let z = mk_fresh_int_var ctx "z" in
  let vx = mk_fresh_int_var ctx "vx" in
  let vy = mk_fresh_int_var ctx "vy" in
  let vz = mk_fresh_int_var ctx "vz" in
  let solver = Z3.Solver.mk_solver ctx None in
  List.iteri
    ~f:(fun i a ->
      let (ax, ay, az), (vax, vay, vaz) = a in
      let t = mk_fresh_int_var ctx (Printf.sprintf "t_%d" i) in
      Z3.Solver.add
        solver
        [ Z3.Arithmetic.mk_ge ctx t (Z3.Arithmetic.Integer.mk_const_s ctx "0") ];
      Z3.Solver.add
        solver
        [ Z3.Boolean.mk_eq
            ctx
            (Z3.Arithmetic.mk_add ctx [ x; Z3.Arithmetic.mk_mul ctx [ vx; t ] ])
            (Z3.Arithmetic.mk_add
               ctx
               [ Z3.Arithmetic.Integer.mk_numeral_i ctx (int_of_float ax)
               ; Z3.Arithmetic.mk_mul
                   ctx
                   (* I have no idea why only this one condition can be a const int, where as the others need to be numerals... *)
                   [ Z3.Arithmetic.Integer.mk_const_s
                       ctx
                       (vax |> int_of_float |> string_of_int)
                   ; t
                   ]
               ])
        ];
      Z3.Solver.add
        solver
        [ Z3.Boolean.mk_eq
            ctx
            (Z3.Arithmetic.mk_add ctx [ y; Z3.Arithmetic.mk_mul ctx [ vy; t ] ])
            (Z3.Arithmetic.mk_add
               ctx
               [ Z3.Arithmetic.Integer.mk_numeral_i ctx (int_of_float ay)
               ; Z3.Arithmetic.mk_mul
                   ctx
                   [ Z3.Arithmetic.Integer.mk_numeral_i ctx (vay |> int_of_float); t ]
               ])
        ];
      Z3.Solver.add
        solver
        [ Z3.Boolean.mk_eq
            ctx
            (Z3.Arithmetic.mk_add ctx [ z; Z3.Arithmetic.mk_mul ctx [ vz; t ] ])
            (Z3.Arithmetic.mk_add
               ctx
               [ Z3.Arithmetic.Integer.mk_numeral_i ctx (int_of_float az)
               ; Z3.Arithmetic.mk_mul
                   ctx
                   [ Z3.Arithmetic.Integer.mk_numeral_i ctx (int_of_float vaz); t ]
               ])
        ])
    hailstones;
  match Z3.Solver.check solver [] with
  | Z3.Solver.UNSATISFIABLE -> failwith "not satisfiable"
  | Z3.Solver.UNKNOWN -> failwith "unknown"
  | Z3.Solver.SATISFIABLE ->
    let model = Z3.Solver.get_model solver |> Option.value_exn in
    let get_int_value (var : Z3.Expr.expr) : int =
      Z3.Expr.to_string (Z3.Model.eval model var true |> Option.value_exn)
      |> int_of_string
    in
    let x_value = get_int_value x in
    let y_value = get_int_value y in
    let z_value = get_int_value z in
    Printf.printf
      "Part two: %d [x = %d, y = %d, z = %d]\n"
      (x_value + y_value + z_value)
      x_value
      y_value
      z_value
;;
(* Z3.del_context ctx () *)
