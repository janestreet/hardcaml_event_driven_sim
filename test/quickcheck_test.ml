(** Test Hardcaml_event_driven_sim against Cyclesim on randomly generated circuits. *)

open Core
open Hardcaml
open Hardcaml_event_driven_sim.Four_state_simulator

let cycle_count = 15

let gen_circuit_and_inputs =
  let open Quickcheck.Let_syntax in
  let%bind circuit = Hardcaml_test.Generator.gen_circuit ~allow_inputs:true ~depth:4 in
  let%map inputs =
    Quickcheck.Generator.list_with_length
      cycle_count
      (Hardcaml_test.Generator.gen_input_data circuit)
  in
  circuit, inputs
;;

let cyclesim_eval inputs circuit =
  let sim = Cyclesim.create circuit in
  Cyclesim.reset sim;
  let results = ref [] in
  let out_port = Cyclesim.out_port sim "out" in
  List.iter inputs ~f:(fun input_values ->
    List.iter input_values ~f:(fun (name, value) -> Cyclesim.in_port sim name := value);
    Cyclesim.cycle sim;
    results := !out_port :: !results);
  !results |> List.rev
;;

let find_by_name name =
  List.find ~f:(fun signal -> List.mem (Signal.names signal) name ~equal:String.equal)
;;

let event_driven_sim_eval inputs circuit =
  let open Simulator in
  let open Simulator.Async in
  let ops = Ops.circuit_to_processes circuit ~combine_wires:true in
  let find_sim_signal = Ops.find_sim_signal ops in
  let sim_clock_opt =
    find_by_name "clock" (Circuit.inputs circuit) |> Option.map ~f:find_sim_signal
  in
  let sim_reset_opt =
    find_by_name "reset" (Circuit.inputs circuit) |> Option.map ~f:find_sim_signal
  in
  let sim_out = find_sim_signal (List.hd_exn (Circuit.outputs circuit)) in
  let sim_clock =
    match sim_clock_opt with
    | Some sim_clock -> sim_clock
    | None -> Logic.create_signal ~initial_value:(Logic.zero 1) 1
  in
  let clock_driver =
    Process.create [ !&sim_clock ] (fun () ->
      (sim_clock <--- Logic.( ~: ) !!sim_clock) ~delay:500)
  in
  let reset_driver =
    match sim_reset_opt with
    | Some sim_reset ->
      create_process (fun () ->
        let%bind () = delay 50 in
        sim_reset <-- Logic.of_string "1";
        let%bind () = delay 50 in
        sim_reset <-- Logic.of_string "0";
        forever (fun () -> delay 100000))
    | None -> create_process (fun () -> delay 100000)
  in
  let remaining_inputs = ref inputs in
  let set_inputs values =
    List.iter values ~f:(fun (name, value) ->
      let sim_input =
        find_by_name name (Circuit.inputs circuit) |> Option.value_exn |> find_sim_signal
      in
      (* we need to set inputs after all activity from previous cycle finishes, but before
         rising edge of the next one *)
      (sim_input <--- Logic.of_bits value) ~delay:200)
  in
  let input_driver =
    Process.create [ !&sim_clock ] (fun () ->
      if not (Logic.to_bool !!sim_clock)
      then (
        match !remaining_inputs with
        | inputs :: rest ->
          set_inputs inputs;
          remaining_inputs := rest
        | [] -> ()))
  in
  let sim = create ([ clock_driver; reset_driver; input_driver ] @ Ops.processes ops) in
  let results = ref [] in
  for i = 1 to cycle_count do
    run sim ~time_limit:((i * 1000) + 10);
    let value = Signal.read sim_out in
    let bits = Logic.to_bits_exn value in
    results := bits :: !results
  done;
  !results |> List.rev
;;

let%test_unit "cyclesim == event_based_sim" =
  Quickcheck.test gen_circuit_and_inputs ~f:(fun (circuit, inputs) ->
    let cyclesim_out = cyclesim_eval inputs circuit in
    let event_driven_sim_out = event_driven_sim_eval inputs circuit in
    printf
      !"sim: expected=%{Sexp} got=%{Sexp}\n"
      ([%sexp_of: Bits.t list] cyclesim_out)
      ([%sexp_of: Bits.t list] event_driven_sim_out);
    if not ([%equal: Bits.t list] event_driven_sim_out cyclesim_out)
    then (
      Rtl.print Verilog circuit;
      raise_s
        [%message
          "invalid result"
            (event_driven_sim_out : Bits.t list)
            (cyclesim_out : Bits.t list)]))
;;
