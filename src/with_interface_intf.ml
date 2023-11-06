open! Core
open Event_driven_sim.Simulator

module type Logic_S = Logic.S

module type Config = sig
  type t = { is_internal_port : (Hardcaml.Signal.t -> bool) option }

  val default : t
  val trace_all : t
end

module Config = struct
  type t = { is_internal_port : (Hardcaml.Signal.t -> bool) option }

  let default = { is_internal_port = None }

  let trace_all =
    { is_internal_port = Some (fun s -> not (List.is_empty (Hardcaml.Signal.names s))) }
  ;;
end

module M (Logic : Logic_S) (Input : Hardcaml.Interface.S) (Output : Hardcaml.Interface.S) =
struct
  module type S = sig
    module Input : Hardcaml.Interface.S with type 'a t = 'a Input.t
    module Output : Hardcaml.Interface.S with type 'a t = 'a Output.t
    module Logic : Logic_S with type t = Logic.t
    module Ops : Ops.S with type comb := Logic.t

    type t =
      { processes : Process.t list
      ; input : Logic.t Port.t Input.t
      ; output : Logic.t Port.t Output.t
      ; internal : Logic.t Port.t list
      }

    (** Returns a process that drives a given signal as a clock with a given time between
        transitions. *)
    val create_clock
      :  ?phase:int
           (** the offset of the first rising edge of the clock relative to the start of the
          simulation. It must be within the range [0, 2 * time). The default value is
          [time], so that all clocks start on the falling edge. *)
      -> time:int
      -> Logic.t Event_driven_sim.Simulator.Signal.t
      -> Event_driven_sim.Simulator.Process.t

    val create : ?config:Config.t -> Hardcaml.Interface.Create_fn(Input)(Output).t -> t

    type testbench_processes =
      Logic.t Port.t Input.t
      -> Logic.t Port.t Output.t
      -> Event_driven_sim.Simulator.Process.t list

    type testbench =
      { ports_and_processes : t
      ; simulator : Event_driven_sim.Simulator.t
      }

    (** Create an event driven simulation with the provided processes. *)
    val with_processes
      :  ?config:Config.t
      -> Hardcaml.Interface.Create_fn(Input)(Output).t
      -> testbench_processes
      -> testbench

    (** Create an event driven simulation and VCD trace file and attach it together. *)
    val with_vcd
      :  ?config:Config.t
      -> vcd:Out_channel.t
      -> Hardcaml.Interface.Create_fn(Input)(Output).t
      -> testbench_processes
      -> testbench

    (** Like [with_vcd] except the vcd is generated only if a file name is provided and
        the environment variable [EXPECT_TEST_WAVEFORM] exists. *)
    val expect
      :  ?config:Config.t
      -> ?vcd:string
      -> Hardcaml.Interface.Create_fn(Input)(Output).t
      -> testbench_processes
      -> testbench
  end
end

module type With_interface = sig
  module M = M

  module type Logic_S = Logic.S

  module Config = Config

  module Make
    (Logic : Logic_S)
    (Input : Hardcaml.Interface.S)
    (Output : Hardcaml.Interface.S) : M(Logic)(Input)(Output).S
end
