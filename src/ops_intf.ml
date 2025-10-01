open! Core

module type S = sig
  type t
  type comb

  val processes : t -> Event_driven_sim.Simulator.Process.t list

  (** Find the simulation signal associated with the given Hardcaml signal. Raises if the
      signal does not exist within the circuit. *)
  val find_sim_signal : t -> Hardcaml.Signal.t -> comb Event_driven_sim.Simulator.Signal.t

  (** Construct a fake simulation signal for the given Hardcaml signal. *)
  val fake_sim_signal : t -> Hardcaml.Signal.t -> comb Event_driven_sim.Simulator.Signal.t

  (** Returns the underlying memory for a signal used in the simulator. *)
  val lookup_memory_exn
    :  t
    -> Hardcaml.Signal.Type.Uid.t
    -> comb Hardcaml.Private.Simulation_memory.t

  (** Compiles Hardcaml circuit into a Event_driven_sim process list. Returns the list and
      a mapping from Hardcaml signals into Event_driven_sim signals.

      For every signal, [delay] should return the simulation time it takes to compute
      value for a given signal. Default is to have no delay. *)
  val circuit_to_processes
    :  ?delay:(Hardcaml.Signal.t -> int)
    -> ?external_insts:
         (Hardcaml.Signal.t
          -> inputs:comb Event_driven_sim.Simulator.Signal.t list
          -> comb Event_driven_sim.Simulator.Signal.t)
    -> Hardcaml.Circuit.t
    -> combine_wires:bool
    -> t

  (** Splits the circuit into clock domains and uses cyclesim to simulate all non-floating
      clock domains. *)
  val circuit_to_hybrid_processes
    :  ?delay:(Hardcaml.Signal.t -> int)
    -> ?external_insts:
         (Hardcaml.Signal.t
          -> inputs:comb Event_driven_sim.Simulator.Signal.t list
          -> comb Event_driven_sim.Simulator.Signal.t)
    -> Hardcaml.Circuit.t
    -> internally_traced_signals:Hardcaml.Signal.t list
    -> combine_wires:bool
    -> combinational_ops_database:Hardcaml.Combinational_ops_database.t
    -> random_initializer:Hardcaml.Cyclesim.Config.Random_initializer.t option
    -> config_options:Hybrid_sim_options.t
    -> t
end

module type Ops = sig
  module type S = S

  module Make (Comb : Logic.S) : S with type comb := Comb.t
end
