open! Core
include Hardcaml.Bits

let to_bits_exn = Fn.id
let of_bits = Fn.id

let of_string_9_state ?(strict = false) s =
  String.map s ~f:(function
    | '0' -> '0'
    | '1' -> '1'
    | 'L' when not strict -> '0'
    | 'H' when not strict -> '1'
    | 'U' | 'X' | 'Z' | 'W' | '-' | 'L' | 'H' ->
      raise_s
        [%message
          "9-state logic string cannot be converted to two-state logic"
            (s : string)
            (strict : bool)]
    | _ -> raise_s [%message "invalid 9-state logic string" s])
  |> of_bit_string
;;

let create_signal ?initial_value ?resolution width =
  ignore resolution;
  Event_driven_sim.Simulator.Signal.create
    (module struct
      type nonrec t = t [@@deriving sexp_of]

      let ( = ) = Hardcaml.Bits.equal
      let resolve_value = `Unresolved

      let check_value_compatibility new_value =
        if Hardcaml.Bits.width new_value <> width
        then
          raise_s
            [%message
              "attempting to assign value with wrong width" (new_value : t) (width : int)]
      ;;

      let initial_value = initial_value |> Option.value ~default:(zero width)
    end)
;;

let is_twostate = true
