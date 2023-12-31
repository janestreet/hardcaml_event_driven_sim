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

  type t =
    { processes : Simulator.Process.t list
    ; input : Logic.t Port.t Input.t
    ; output : Logic.t Port.t Output.t
    ; internal : Logic.t Port.t list
    }

  let create_clock ?phase ~time signal =
    let open Simulator in
    let phase = Option.value phase ~default:time in
    if phase < 0 || phase >= 2 * time
    then raise_s [%message "phase must be within [0, 2*time)" (phase : int) (time : int)];
    let initial_iteration = ref true in
    let toggle ~delay = (signal <--- Logic.( ~: ) !!signal) ~delay in
    Simulator.Process.create [ !&signal ] (fun () ->
      match !initial_iteration with
      | true ->
        initial_iteration := false;
        if phase = 0
        then
          raise_s
            [%message
              "We don't currently deal with the case where the first rising edge is \
               exactly at time 0. This requires some thought as to what the correct \
               behavior is - for example, take a look at [test/basic_memory.ml] in this \
               folder. In the current model, where clocks start out low, we align all of \
               our top level inputs to occur on the falling edge, so the change always \
               occurs at the next rising edge. In a model where the clock is 1 at time \
               0, it is less clear what is the correct behavior if you drive the input \
               at time 0."]
        else if phase <= time
        then (
          if Logic.is_vdd !!signal
          then
            raise_s [%message "clock initial value is vdd; should be gnd" (phase : int)];
          toggle ~delay:phase)
        else (
          if Logic.is_gnd !!signal
          then
            raise_s [%message "clock initial value is gnd; should be vdd" (phase : int)];
          toggle ~delay:(phase - time))
      | false -> toggle ~delay:time)
  ;;

  let internal_ports circuit ~is_internal_port =
    let open Hardcaml in
    match is_internal_port with
    | None -> []
    | Some f ->
      Signal_graph.filter (Circuit.signal_graph circuit) ~f:(fun s ->
        (not (Circuit.is_input circuit s))
        && (not (Circuit.is_output circuit s))
        && (not (Signal.is_empty s))
        && f s)
      |> List.map ~f:(fun signal -> `connected signal)
  ;;

  let make_circuit_and_io ~is_internal_port f =
    let input_pre_signals =
      Input.zip Input.port_widths Input.port_names
      |> Input.map ~f:(fun (width, name) -> Hardcaml.Signal.input name width)
    in
    let output_pre = f input_pre_signals in
    let output_pre_named =
      Output.map2 Output.port_names output_pre ~f:Hardcaml.Signal.output
    in
    let circuit =
      Hardcaml.Circuit.create_exn ~name:"simulator" (Output.to_list output_pre_named)
    in
    let signal_name signal =
      match Hardcaml.Signal.names signal with
      | [ name ] -> name
      | _ -> raise_s [%message "expected exactly one name" (signal : Hardcaml.Signal.t)]
    in
    let output_signals =
      List.map (Hardcaml.Circuit.outputs circuit) ~f:(fun output ->
        signal_name output, `connected output)
      |> Output.Unsafe_assoc_by_port_name.of_alist
    in
    let input_signals =
      let real_inputs =
        List.map (Hardcaml.Circuit.inputs circuit) ~f:(fun input ->
          signal_name input, input)
        |> String.Map.of_alist_exn
      in
      Input.(
        map port_names_and_widths ~f:(fun (name, width) ->
          match Map.find real_inputs name with
          | Some v -> name, `connected v
          | None -> name, `unconnected (Hardcaml.Signal.wire width)))
      |> Input.to_list
      |> Input.Unsafe_assoc_by_port_name.of_alist
    in
    let internal_ports = internal_ports circuit ~is_internal_port in
    circuit, input_signals, output_signals, internal_ports
  ;;

  let create ?(config = Config.default) f =
    let circuit, input_signals, output_signals, internal_signals =
      make_circuit_and_io ~is_internal_port:config.is_internal_port f
    in
    let ops = Ops.circuit_to_processes circuit in
    let port base_signal =
      match base_signal with
      | `unconnected base_signal ->
        (* An unconnected port (which can come from unconnected inputs) get replaced with
           a fake simulation signal. *)
        { Port.signal = Ops.fake_sim_signal ops base_signal; base_signal }
      | `connected base_signal ->
        { Port.signal = Ops.find_sim_signal ops base_signal; base_signal }
    in
    { processes = Ops.processes ops
    ; input = Input.map input_signals ~f:port
    ; output = Output.map output_signals ~f:port
    ; internal = List.map internal_signals ~f:port
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

  let expect ?config ?vcd f testbench =
    match Sys.getenv "EXPECT_TEST_WAVEFORM", vcd with
    | Some _, Some name ->
      with_vcd ?config ~vcd:(Out_channel.create [%string "%{name}.vcd"]) f testbench
    | Some _, None | None, Some _ | None, None -> with_processes ?config f testbench
  ;;
end
