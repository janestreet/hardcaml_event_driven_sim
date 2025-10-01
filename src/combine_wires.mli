open! Core
open! Hardcaml

module Copied_signals : sig
  type t =
    { new_signals : Signal.t list
    ; old_signal_to_new_signal : Signal.t Map.M(Signal.Type.Uid).t
    (** A map from the original signal's uids to the new [Signal.t]s. Some old signals may
        map to the same new signal *)
    }
end

(** [combine outputs] creates a new circuit with all wires compressed down to only the
    necessary wires. It returns the outputs of this new circuit and a map from the old
    signals to the new signals.

    This function will NOT preserve the names of the signals in the original circuit.
    Every signal in the returned circuit will have a fresh name unique to the returned
    circuit. *)
val combine : Signal.t list -> Copied_signals.t
