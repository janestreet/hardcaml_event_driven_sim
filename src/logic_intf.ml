open! Core

module type S = sig
  include Hardcaml.Comb.S

  val to_bits_exn : t -> Hardcaml.Bits.t
  val of_bits : Hardcaml.Bits.t -> t
  val compare : t -> t -> int

  val create_signal
    :  ?initial_value:t
    -> ?resolution:[ `Unresolved | `Resolved ]
    -> int
    -> t Event_driven_sim.Simulator.Signal.t
end

module type Logic = sig
  module type S = S
end
