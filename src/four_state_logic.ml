open! Core
open Hardcaml

type t =
  { bits : Bits.t
  ; mask : Bits.t
  }
[@@deriving equal ~localize, compare ~localize, fields ~getters, sexp_of]

let is_pure t =
  let d = Bits.to_constant t.mask |> Constant.Raw.to_bytes in
  Bytes.fold ~init:true ~f:(fun acc b -> acc && Char.to_int b = 0) d
;;

let of_bits t = { bits = t; mask = Bits.zero (Bits.width t) }

let to_bits_exn t =
  if not (is_pure t)
  then raise_s [%message "cannot convert to bits - value has error bits set" (t : t)];
  t.bits
;;

module Gates : Comb.Gates with type t = t = struct
  type nonrec t = t [@@deriving equal ~localize]

  let empty = { bits = Bits.empty; mask = Bits.empty }
  let is_empty { bits; _ } = Bits.is_empty bits
  let width { bits; _ } = Bits.width bits
  let of_constant c = of_bits (Bits.of_constant c)
  let to_constant t = Bits.to_constant (to_bits_exn t)
  let vdd = of_constant (Constant.of_int ~width:1 1)
  let gnd = of_constant (Constant.of_int ~width:1 0)

  let concat_msb l =
    { bits = Bits.concat_msb (List.map l ~f:bits)
    ; mask = Bits.concat_msb (List.map l ~f:mask)
    }
  ;;

  let select t ~high ~low =
    { bits = Bits.select t.bits ~high ~low; mask = Bits.select t.mask ~high ~low }
  ;;

  let ( -- ) ~(loc : [%call_pos]) t _name =
    ignore loc;
    t
  ;;

  let[@inline] op1 op a =
    let open Bits in
    { bits = op a.bits &: ~:(a.mask) |: (a.bits &: a.mask); mask = a.mask }
  ;;

  (* if mask=1, 1 is X and 0 is Z *)
  let[@inline] op2 op a b =
    let open Bits in
    let mask = a.mask &: b.mask |: (a.mask &: a.bits) |: (b.mask &: b.bits) in
    let bits =
      let bits_if_they_hold_value = ~:mask &: op a.bits b.bits in
      let bits_if_they_are_x = mask &: (a.bits |: b.bits) in
      bits_if_they_hold_value |: bits_if_they_are_x
    in
    { bits; mask }
  ;;

  let ( &: ) = op2 Bits.( &: )
  let ( |: ) = op2 Bits.( |: )
  let ( ^: ) = op2 Bits.( ^: )
  let ( ~: ) = op1 Bits.( ~: )

  let to_string t =
    List.map2_exn
      (Bits.to_string t.bits |> String.to_list)
      (Bits.to_string t.mask |> String.to_list)
      ~f:(fun bit mask ->
        match bit, mask with
        | a, '0' -> a
        | '0', '1' -> 'Z'
        | '1', '1' -> 'X'
        | _ -> failwith "unexpected value in Bits.to_string")
    |> String.of_char_list
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
end

module Slow_primitives = Comb.Make_primitives (Gates)

module T = Comb.Make (struct
    include Gates

    let[@inline] lift2 op op_fallback a b =
      if is_pure a && is_pure b
      then (
        let bits = op a.bits b.bits in
        { bits; mask = Bits.zero (Bits.width bits) })
      else op_fallback a b
    ;;

    let ( +: ) = lift2 Bits.( +: ) Slow_primitives.( +: )
    let ( -: ) = lift2 Bits.( -: ) Slow_primitives.( -: )
    let ( *: ) = lift2 Bits.( *: ) Slow_primitives.( *: )
    let ( *+ ) = lift2 Bits.( *+ ) Slow_primitives.( *+ )
    let ( <: ) = lift2 Bits.( <: ) Slow_primitives.( <: )
    let ( ==: ) = lift2 Bits.( ==: ) Slow_primitives.( ==: )

    let mux a l =
      if is_pure a
      then (
        match
          List.map l ~f:(fun v -> if is_pure v then Some v.bits else None) |> Option.all
        with
        | Some l_bits -> { bits = Bits.mux a.bits l_bits; mask = (List.hd_exn l).mask }
        | None -> Slow_primitives.mux a l)
      else Slow_primitives.mux a l
    ;;

    include Comb.Expert.Gen_cases_from_mux (struct
        type nonrec t = t

        let mux = mux
        let ( ==: ) = ( ==: )
      end)
  end)

include (
  T :
    module type of struct
      include T
    end
    with type t := t)

let ( = ) x y = compare x y = 0
let zero width = { bits = Bits.zero width; mask = Bits.zero width }
let high_impedance width = { bits = Bits.zero width; mask = Bits.one width }
let don't_care width = { bits = Bits.one width; mask = Bits.one width }

let const t =
  let bits_str, mask_str =
    String.to_list t
    |> List.map ~f:(function
      | '1' -> '1', '0'
      | '0' -> '0', '0'
      | 'X' -> '1', '1'
      | 'Z' -> '0', '1'
      | _ -> raise_s [%message "invalid Logic string" (t : string)])
    |> List.unzip
  in
  { bits = Bits.of_string (String.of_char_list bits_str)
  ; mask = Bits.of_string (String.of_char_list mask_str)
  }
;;

let of_string = const

(* Mapped based on how Xilinx maps VHDL states to Verilog states
   https://docs.amd.com/r/en-US/ug900-vivado-logic-simulation/VHDL-and-Verilog-Values-Mapping
*)
let of_string_9_state ?(strict = false) s =
  String.map s ~f:(function
    | '0' -> '0'
    | '1' -> '1'
    | 'X' -> 'X'
    | 'Z' -> 'Z'
    | 'L' when not strict -> '0'
    | 'H' when not strict -> '1'
    | ('U' | 'W' | '-') when not strict -> 'X'
    | 'U' | 'W' | '-' | 'L' | 'H' ->
      raise_s
        [%message
          "9-state logic string cannot be converted to four-state logic"
            (s : string)
            (strict : bool)]
    | _ -> raise_s [%message "invalid 9-state logic string" s])
  |> of_string
;;

(*=
   . | 0 1 X Z
   --+--------
   0 | 0 X X 0
   1 | X 1 X 1
   X | X X X X
   Z | 0 1 X Z
*)

let resolve2 x y =
  let open Bits in
  let a = x.mask in
  let b = x.bits in
  let c = y.mask in
  let d = y.bits in
  let a' = ~:a in
  let b' = ~:b in
  let c' = ~:c in
  let d' = ~:d in
  { bits = b |: d
  ; mask = c &: d |: (a &: c) |: (a &: b) |: (a' &: b' &: d) |: (b &: c' &: d')
  }
;;

let%test_unit "resolve2 works correctly" =
  let resolve_bit_slow a b =
    match a, b with
    | '0', '0' -> '0'
    | '1', '1' -> '1'
    | 'X', _ -> 'X'
    | _, 'X' -> 'X'
    | '0', '1' -> 'X'
    | '1', '0' -> 'X'
    | 'Z', b -> b
    | a, 'Z' -> a
    | _ -> failwith "unexpected values"
  in
  let resolve_slow a b =
    List.map2_exn
      (to_string a |> String.to_list)
      (to_string b |> String.to_list)
      ~f:resolve_bit_slow
    |> String.of_char_list
    |> const
  in
  let values = [ "0"; "1"; "Z"; "X" ] in
  List.iter values ~f:(fun a_str ->
    List.iter values ~f:(fun b_str ->
      let a = const a_str in
      let b = const b_str in
      [%test_result: t] (resolve2 a b) ~expect:(resolve_slow a b)))
;;

module For_simulator (Prop : sig
    val width : int
    val initial_value : t
    val resolution : [ `Unresolved | `Resolved ]
  end) =
struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t
  let ( = ) = equal

  let rec resolve_func ~last_value values =
    match values with
    | [] -> last_value
    | [ x ] -> x
    | x :: xs -> resolve2 x (resolve_func ~last_value xs)
  ;;

  let resolve_value =
    match Prop.resolution with
    | `Resolved -> `Func resolve_func
    | `Unresolved -> `Unresolved
  ;;

  let check_value_compatibility new_value =
    if width new_value <> Prop.width
    then
      raise_s
        [%message
          "attempting to assign value with wrong width" (new_value : t) (Prop.width : int)]
  ;;

  let initial_value = Prop.initial_value
end

let create_signal ?initial_value ?(resolution = `Unresolved) width =
  Event_driven_sim.Simulator.Signal.create
    (module For_simulator (struct
        let width = width
        let initial_value = initial_value |> Option.value ~default:(don't_care width)
        let resolution = resolution
      end))
;;

let is_twostate = false
