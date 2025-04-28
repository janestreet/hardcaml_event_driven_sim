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

val combine : Signal.t list -> Copied_signals.t
