open! Core
open Event_driven_sim

module type Logic_S = Logic.S

module Make
    (Logic : Logic_S)
    (Input : Hardcaml.Interface.S)
    (Output : Hardcaml.Interface.S) =
struct
  module Logic = Logic
  module Ops = Ops.Make (Logic)

  type t =
    { processes : Simulator.Process.t list
    ; input : Logic.t Port.t Input.t
    ; output : Logic.t Port.t Output.t
    }

  let create_clock signal ~time =
    let open Simulator in
    Simulator.Process.create [ !&signal ] (fun () ->
      (signal <--- Logic.( ~: ) !!signal) ~delay:time)
  ;;

  let make_circuit_and_io ~name f =
    let input_pre_signals =
      Input.zip Input.port_widths Input.port_names
      |> Input.map ~f:(fun (width, name) -> Hardcaml.Signal.input name width)
    in
    let output_pre = f input_pre_signals in
    let output_pre_named =
      Output.map2 Output.port_names output_pre ~f:Hardcaml.Signal.output
    in
    let circuit = Hardcaml.Circuit.create_exn ~name (Output.to_list output_pre_named) in
    let signal_name signal =
      match Hardcaml.Signal.names signal with
      | [ name ] -> name
      | _ -> raise_s [%message "expected exactly one name" (signal : Hardcaml.Signal.t)]
    in
    let output_signals =
      List.map (Hardcaml.Circuit.outputs circuit) ~f:(fun output ->
        signal_name output, output)
      |> Output.Unsafe_assoc_by_port_name.of_alist
    in
    let input_signals =
      List.map (Hardcaml.Circuit.inputs circuit) ~f:(fun input ->
        signal_name input, input)
      |> Input.Unsafe_assoc_by_port_name.of_alist
    in
    circuit, input_signals, output_signals
  ;;

  let create ~name f =
    let circuit, input_signals, output_signals = make_circuit_and_io ~name f in
    let ops = Ops.circuit_to_processes circuit in
    let port base_signal =
      { Port.signal = Ops.to_sim_signal ops base_signal; base_signal }
    in
    { processes = Ops.processes ops
    ; input = Input.map input_signals ~f:port
    ; output = Output.map output_signals ~f:port
    }
  ;;
end
