open! Core
open Event_driven_sim

module type Logic_S = Logic.S

module M = With_interface_intf.M
module Config = With_interface_intf.Config

module Make
    (Logic : Logic_S)
    (Input : Hardcaml.Interface.S)
    (Output : Hardcaml.Interface.S) =
struct
  module Input = Input
  module Output = Output
  module Logic = Logic
  module Ops = Ops.Make (Logic)
  module Vcd = Vcd.Make (Logic)
  module Waveterm = Waveterm.Make (Logic)

  type t =
    { processes : Simulator.Process.t list
    ; input : Logic.t Port.t Input.t
    ; output : Logic.t Port.t Output.t
    ; internal : Logic.t Port.t list
    }

  let create_clock ?initial_delay ~time signal =
    let open Simulator in
    let initial_delay = Option.value initial_delay ~default:time in
    let initial_iteration = ref true in
    let toggle ~delay = (signal <--- Logic.( ~: ) !!signal) ~delay in
    Simulator.Process.create [ !&signal ] (fun () ->
      match !initial_iteration with
      | true ->
        initial_iteration := false;
        toggle ~delay:initial_delay
      | false -> toggle ~delay:time)
  ;;

  let traced = Hardcaml.Cyclesim.Private.Traced_nodes.create

  let make_circuit f =
    let input_pre_signals =
      Input.zip Input.port_widths Input.port_names
      |> Input.map ~f:(fun (width, name) -> Hardcaml.Signal.input name width)
    in
    let output_pre = f input_pre_signals in
    let output_pre_named =
      Output.map2 Output.port_names output_pre ~f:Hardcaml.Signal.output
    in
    Hardcaml.Circuit.create_exn ~name:"simulator" (Output.to_list output_pre_named)
  ;;

  let make_circuit_and_io ~is_internal_port f =
    let circuit = make_circuit f in
    let traced = traced circuit ~is_internal_port in
    circuit, traced
  ;;

  let create ?(config = Config.default) f =
    let circuit, traced =
      make_circuit_and_io ~is_internal_port:config.is_internal_port f
    in
    let ops = Ops.circuit_to_processes circuit in
    let port base_signal mangled_names =
      { Port.signal = Ops.find_sim_signal ops base_signal; base_signal; mangled_names }
    in
    let fake_port width mangled_names =
      let base_signal = Hardcaml.Signal.wire width in
      { Port.signal = Ops.fake_sim_signal ops base_signal; base_signal; mangled_names }
    in
    let find_port (traced : Hardcaml.Cyclesim.Traced.io_port list) name =
      List.find traced ~f:(fun t -> String.equal t.name name)
    in
    { processes = Ops.processes ops
    ; input =
        Input.map2 Input.port_names Input.port_widths ~f:(fun name width ->
          match find_port traced.input_ports name with
          | None -> fake_port width [ name ]
          | Some { signal; name } -> port signal [ name ])
    ; output =
        Output.map Output.port_names ~f:(fun name ->
          match find_port traced.output_ports name with
          | None -> raise_s [%message "Missing output port"]
          | Some { signal; name } -> port signal [ name ])
    ; internal =
        List.map traced.internal_signals ~f:(fun { signal; mangled_names } ->
          port signal mangled_names)
    }
  ;;

  type testbench_processes =
    Logic.t Port.t Input.t
    -> Logic.t Port.t Output.t
    -> Event_driven_sim.Simulator.Process.t list

  type testbench =
    { ports_and_processes : t
    ; simulator : Event_driven_sim.Simulator.t
    }

  let with_processes ?config f testbench =
    let ({ processes; input; output; internal = _ } as ports_and_processes) =
      create ?config f
    in
    let testbench_processes = testbench input output in
    let simulator = Event_driven_sim.Simulator.create (processes @ testbench_processes) in
    { ports_and_processes; simulator }
  ;;

  let with_vcd ?config ~vcd f testbench =
    let ({ processes; input; output; internal } as ports_and_processes) =
      create ?config f
    in
    let vcd = Vcd.create vcd (Input.to_list input @ Output.to_list output @ internal) in
    let testbench_processes = testbench input output in
    let simulator =
      Event_driven_sim.Simulator.create
        (processes @ Vcd.processes vcd @ testbench_processes)
    in
    Vcd.attach_to_simulator vcd simulator;
    { ports_and_processes; simulator }
  ;;

  let with_waveterm ?config f testbench =
    let ({ processes; input; output; internal } as ports_and_processes) =
      create ?config f
    in
    let { Waveterm.processes = waveterm_processes; waveform } =
      Waveterm.create (Input.to_list input @ Output.to_list output @ internal)
    in
    let testbench_processes = testbench input output in
    let simulator =
      Event_driven_sim.Simulator.create
        (processes @ waveterm_processes @ testbench_processes)
    in
    waveform, { ports_and_processes; simulator }
  ;;

  let expect ?config ?vcd f testbench =
    match Sys.getenv "EXPECT_TEST_WAVEFORM", vcd with
    | Some _, Some name ->
      with_vcd ?config ~vcd:(Out_channel.create [%string "%{name}.vcd"]) f testbench
    | Some _, None | None, Some _ | None, None -> with_processes ?config f testbench
  ;;
end
