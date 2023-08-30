open! Core
open Event_driven_sim.Simulator

module type Logic_S = Logic.S

module Make
  (Logic : Logic_S)
  (Input : Hardcaml.Interface.S)
  (Output : Hardcaml.Interface.S) : sig
  module Logic : Logic_S with type t = Logic.t
  module Ops : Ops.S with type comb := Logic.t

  type t =
    { processes : Process.t list
    ; input : Logic.t Port.t Input.t
    ; output : Logic.t Port.t Output.t
    }

  (** Returns a process that drives a given signal as a clock with a given time between
      transitions. *)
  val create_clock
    :  Logic.t Event_driven_sim.Simulator.Signal.t
    -> time:int
    -> Event_driven_sim.Simulator.Process.t

  val create
    :  name:string
    -> (Hardcaml.Signal.t Input.t -> Hardcaml.Signal.t Output.t)
    -> t
end
