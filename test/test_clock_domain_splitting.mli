open! Core
open Hardcaml

module Make_test : functor (Input : Interface.S) (Output : Interface.S) -> sig
  val test
    :  ?show:[ `All | `Named_and_interfaces | `Named ]
    -> ?show_kind:bool
    -> (Reg_spec.signal Input.t -> Reg_spec.signal Output.t)
    -> unit
end
