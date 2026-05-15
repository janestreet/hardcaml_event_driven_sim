open! Base

module M (Logic : Logic.S) = struct
  module type S = sig
    type t =
      { processes : Event_driven_sim.Simulator.Process.t list
      ; waveform : Hardcaml.Wave_data.t
      }

    val create : Logic.t Port.t list -> t
  end
end

module type Waveterm = sig
  module M = M
  module Make (Logic : Logic.S) : M(Logic).S
end
