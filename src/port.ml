type 'logic t =
  { signal : 'logic Event_driven_sim.Simulator.Signal.t
  ; base_signal : Hardcaml.Signal.t
  ; mangled_names : string list
  }
