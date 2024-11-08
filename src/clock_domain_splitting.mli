open! Core
open Hardcaml

module Clock_spec : sig
  type t =
    { clock : Signal.t
    ; edge : Edge.t
    }
end

module Clock_domain : sig
  (** A [t] represents the clock domain of a signal. A [Floating] signal is driven by
      signals from multiple clock domains. A [Clocked clock] signal is only driven by
      signals coming from circuit elements clocked to [clock]. *)
  type t =
    | Clocked of Clock_spec.t
    | Floating
  [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
end

val group_by_clock_domain : Circuit.t -> Signal_graph.t Clock_domain.Map.t
