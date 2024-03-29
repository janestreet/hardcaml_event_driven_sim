open! Core
module Config = With_interface.Config
module Logic = Logic
module Four_state_logic = Four_state_logic
module Two_state_logic = Two_state_logic
module Vcd = Vcd
module Ops = Ops
module Port = Port
module Waveterm = Waveterm
module With_interface = With_interface.Make

module Make (Logic : Logic.S) = struct
  module Config = Config
  module Event_simulator = Event_driven_sim.Simulator
  module Logic = Logic
  module Ops = Ops.Make (Logic)
  module Vcd = Vcd.Make (Logic)
  module With_interface = With_interface (Logic)
end
