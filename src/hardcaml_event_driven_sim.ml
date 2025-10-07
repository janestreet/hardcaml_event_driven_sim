open! Core
module Config = With_interface.Config
module Four_state_logic = Four_state_logic
module Logic = Logic
module Mini_async = Event_driven_sim.Mini_async
module Ops = Ops
module Port = Port
module Sim_mode = With_interface.Sim_mode
module Simulator = Event_driven_sim.Simulator
module Two_state_logic = Two_state_logic
module Vcd = Vcd
module Waveterm = Waveterm
module With_interface = With_interface.Make

module Private = struct
  module Clock_domain_splitting = Clock_domain_splitting
  module Combine_wires = Combine_wires
end

module type S = sig
  module Config = Config
  module Logic : Logic.S
  module Mini_async = Mini_async
  module Ops : Ops.S with type comb := Logic.t
  module Port = Port
  module Sim_mode = Sim_mode
  module Simulator = Simulator
  module Vcd : module type of Vcd.Make (Logic)
  module Waveterm = Waveterm
  module With_interface : module type of With_interface (Logic)
end

module Make (Logic : Logic.S) = struct
  module Config = Config
  module Logic = Logic
  module Mini_async = Mini_async
  module Ops = Ops.Make (Logic)
  module Port = Port
  module Sim_mode = Sim_mode
  module Simulator = Simulator
  module Vcd = Vcd.Make (Logic)
  module Waveterm = Waveterm
  module With_interface = With_interface (Logic)
end

module Two_state_simulator : S with type Logic.t = Two_state_logic.t =
  Make (Two_state_logic)

module Four_state_simulator : S with type Logic.t = Four_state_logic.t =
  Make (Four_state_logic)
