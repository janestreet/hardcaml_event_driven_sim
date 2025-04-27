open! Core
open Hardcaml

module Reset_spec : sig
  type t =
    { signal : Signal.t
    ; edge : Edge.t
    }
end

module Clock_spec : sig
  type t =
    { clock : Signal.t
    ; edge : Edge.t
    ; reset : Reset_spec.t option
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

module Copied_circuit : sig
  (** In the new circuit, most old signals map to a single new signal. There is one
      instances when this is not the case:
      - If an old signal is the output of a clock domain, it will have 2 new signals: one
        a copied version of the old signal and one that is a wire driven by that signal. *)
  module New_signal : sig
    type t =
      | Internal of Signal.Type.Uid.t
      | Output of
          { output_wire : Signal.Type.Uid.t
          ; output_driver : Signal.Type.Uid.t
          }
  end

  type t =
    { circuit : Circuit.t
    ; new_signals_by_original_uids : New_signal.t Map.M(Signal.Type.Uid).t
    (** Maps a uid from the original circuit to the signal(s) that it corresponds to in
        the new circuit *)
    }
end

(** [group_by_clock_domain circuit] breaks the original circuit in its clock domains and
    constructs a circuit for each clock domain.

    NOTE: some signals in [circuit] may map to signals in multiple clock domains. The
    returned circuits have non-overlapping uid spaces. *)
val group_by_clock_domain
  :  Signal_graph.t
  -> extra_outputs:Signal.t list
  -> Copied_circuit.t Clock_domain.Map.t

module Original_signal_kind : sig
  module Output : sig
    type t =
      { new_output_wire : Signal.Type.Uid.t
      ; new_output_driver : Signal.Type.Uid.t
      }
  end

  module Boundary : sig
    type t =
      { new_output : Output.t
      ; new_output_domain : Clock_domain.t
      ; new_inputs : Signal.Type.Uid.t Clock_domain.Map.t
      (** If empty, then this is a circuit output. *)
      }
  end

  module Circuit_input : sig
    type t =
      | Input of
          { new_uid : Signal.Type.Uid.t
          ; new_domain : Clock_domain.t
          }
      | Boundary of Boundary.t
  end

  (** Represents the possible kinds of signals that a signal from the original circuit can
      be. *)
  type t =
    | Circuit_input of Circuit_input.t
    | Internal of { new_uids : Signal.Type.Uid.t Clock_domain.Map.t }
    | Boundary of Boundary.t
  [@@deriving sexp_of]
end

(** Returns a map from the original circuit uids to their signal kind. *)
val classify_original_uids
  :  original_signal_graph:Signal_graph.t
  -> clock_domains:Copied_circuit.t Clock_domain.Map.t
  -> Original_signal_kind.t Map.M(Signal.Type.Uid).t

module For_testing : sig
  module Clock_domain : sig
    module Floating_reason : sig
      type t =
        | Input
        | Input_and_one_clock_domain
        | Input_and_multiple_clock_domains
        | Multiple_clock_domains
    end

    type t =
      | Any
      | Clocked of Clock_spec.t
      | Floating of Floating_reason.t
    [@@deriving sexp_of]

    include Comparable.S_plain with type t := t
  end

  module Stats : sig
    type t = Clock_domain.t Map.M(Signal.Type.Uid).t [@@deriving sexp_of]

    val create : Signal_graph.t -> t
    val clock_domain_size : t -> int Clock_domain.Map.t

    module Summary : sig
      type t =
        { num_total_nodes : int
        ; num_nodes_not_any : int
        ; num_clocked_nodes : int
        ; percent_of_clocked_non_any_nodes : Percent.t
        }
      [@@deriving sexp_of]

      val to_string : t -> string
    end

    val to_stat_summary : t -> Summary.t
    val to_stat_summary_string : t -> string
  end
end
