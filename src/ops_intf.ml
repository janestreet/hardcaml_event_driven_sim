open! Core

module type S = sig
  type t
  type comb

  val processes : t -> Event_driven_sim.Simulator.Process.t list
  val to_sim_signal : t -> Hardcaml.Signal.t -> comb Event_driven_sim.Simulator.Signal.t

  (** Compiles Hardcaml circuit into a Event_driven_sim process list.
      Returns the list and a mapping from Hardcaml signals into Event_driven_sim signals.

      For every signal, [delay] should return the simulation time it takes to compute value
      for a given signal. Default is to have no delay.
  *)
  val circuit_to_processes
    :  ?delay:(Hardcaml.Signal.t -> int)
    -> ?external_insts:
         (Hardcaml.Signal.t
          -> inputs:comb Event_driven_sim.Simulator.Signal.t list
          -> comb Event_driven_sim.Simulator.Signal.t)
    -> Hardcaml.Circuit.t
    -> t
end

module type Ops = sig
  module type S = S

  module Make (Comb : Logic.S) : S with type comb := Comb.t
end
