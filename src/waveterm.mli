open! Base
module Sim = Event_driven_sim.Simulator
module Time = Int
module Data : Hardcaml_waveterm_event_store.Event_store.Data with type t = Hardcaml.Bits.t

module Events : sig
  module E : Hardcaml_waveterm_event_store.Event_store.M(Time)(Data).S
  include Hardcaml_waveterm_kernel.Expert.Data.Readable
end

include module type of Hardcaml_waveterm_kernel.Make (Events)

module Make (Logic : Logic.S) : sig
  type t =
    { processes : Sim.Process.t list
    ; waveform : Waveform.t
    }

  val create : Logic.t Port.t list -> t
end
