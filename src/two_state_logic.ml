open! Core
include Hardcaml.Bits

let of_bits = Fn.id

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
