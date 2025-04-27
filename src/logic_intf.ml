open! Core

module type S = sig
  include Hardcaml.Comb.S

  val to_bits_exn : t -> Hardcaml.Bits.t
  val of_bits : Hardcaml.Bits.t -> t
  val compare : t -> t -> int

  (** Convert from a VHDL-style 9-state bit string. If strict, will only allow those bit
      values which map exactly to a bit value in the current logic scheme. *)
  val of_string_9_state : ?strict:bool -> string -> t

  val create_signal
    :  ?initial_value:t
    -> ?resolution:[ `Unresolved | `Resolved ]
    -> int
    -> t Event_driven_sim.Simulator.Signal.t

  (** For RTL generation, where there are different types depending on two-state vs
      not-two-state. *)
  val is_twostate : bool
end

module type Logic = sig
  module type S = S
end
