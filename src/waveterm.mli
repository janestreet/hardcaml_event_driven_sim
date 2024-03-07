open! Base
module Sim = Event_driven_sim.Simulator
module Events : Hardcaml_waveterm_kernel.Expert.Data.S
include module type of Hardcaml_waveterm.Expert.Make (Events)

module Make (Logic : Logic.S) : sig
  type t =
    { processes : Sim.Process.t list
    ; waveform : Waveform.t
    }

  val create : Logic.t Port.t list -> t
end
