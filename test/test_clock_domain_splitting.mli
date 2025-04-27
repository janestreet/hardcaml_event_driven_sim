open! Core
open Hardcaml

module Make_test (Input : Interface.S) (Output : Interface.S) : sig
  val test
    :  ?show:[ `All | `Named_and_interfaces | `Named ]
    -> ?show_kind:bool
    -> (Signal.t Input.t -> Signal.t Output.t)
    -> unit
end
