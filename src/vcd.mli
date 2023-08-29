open Core

module Make (Logic : Logic.S) : sig
  type t

  val create : Out_channel.t -> Logic.t Port.t list -> t
  val processes : t -> Event_driven_sim.Simulator.Process.t list
  val attach_to_simulator : t -> Event_driven_sim.Simulator.t -> unit
end
