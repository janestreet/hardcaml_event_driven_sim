open! Core
open Core

module Test (Logic : Hardcaml_event_driven_sim.Logic.S) = struct
  module Sim = Hardcaml_event_driven_sim.Make (Logic)

  module M = Hardcaml_networking.Async_fifo.Make (struct
    let width = 4
    let log2_depth = 3
  end)

  module I = M.I
  module O = M.O

  let%expect_test "full/valid state" =
    let module Sim_interface = Sim.With_interface (I) (O) in
    let open Sim.Event_simulator in
    let open Async in
    let open Logic in
    let fifo = M.create (Hardcaml.Scope.create ~flatten_design:true ()) in
    let { Sim_interface.processes; input; output } =
      Sim_interface.create ~name:"fifo" fifo
    in
    let input = I.map input ~f:(fun v -> v.signal) in
    let output = O.map output ~f:(fun v -> v.signal) in
    let sim =
      Event_driven_sim.Simulator.create
        (processes
         @ [ Debug.print_signal "out" output.O.data_out
           ; Debug.print_signal "in" input.I.data_in
           ; Debug.print_signal "full" output.O.full
           ; Debug.print_signal "valid" output.O.valid
           ; Process.create [] (fun () -> input.I.read_enable <-- of_string "1")
           ; Async.create_process (fun () ->
               input.I.write_enable <-- of_string "1";
               forever (fun () ->
                 let%bind () = delay 30 in
                 input.I.clock_write <-- of_string "1";
                 let%map () = delay 30 in
                 if is_gnd !!(output.O.full)
                 then (
                   input.I.write_enable <-- of_string "1";
                   input.I.data_in <-- !!(input.I.data_in) +:. 1)
                 else input.I.write_enable <-- of_string "0";
                 input.I.clock_write <-- of_string "0"))
           ; Async.create_process (fun () ->
               let%bind () = delay 400 in
               forever (fun () ->
                 input.I.clock_read <-- of_string "1";
                 let%bind () = delay 5 in
                 input.I.clock_read <-- of_string "0";
                 delay 5))
           ])
    in
    run ~time_limit:700 sim;
    [%expect
      {|
    t=0 valid=1
    t=0 full=1
    t=0 valid=0
    t=0 full=0
    t=60 in=0001
    t=120 in=0010
    t=180 in=0011
    t=240 in=0100
    t=300 in=0101
    t=360 in=0110
    t=390 full=1
    t=410 valid=1
    t=420 out=0001
    t=430 out=0010
    t=440 out=0011
    t=450 out=0100
    t=460 out=0101
    t=470 out=0110
    t=480 out=0000
    t=480 valid=0
    t=510 full=0
    t=540 in=0111
    t=580 out=0111
    t=590 valid=1
    t=600 in=1000
    t=600 out=0000
    t=600 valid=0
    t=640 out=1000
    t=650 valid=1
    t=660 in=1001
    t=660 out=0001
    t=660 valid=0 |}]
  ;;

  let reader_writer_test ~read_clock_time ~write_clock_time =
    let module Sim = Event_driven_sim.Simulator in
    let module Sim_interface = Hardcaml_event_driven_sim.With_interface (Logic) (I) (O) in
    let open Sim in
    let open Logic in
    let fifo = M.create (Hardcaml.Scope.create ~flatten_design:true ()) in
    let { Sim_interface.processes; input; output } =
      Sim_interface.create ~name:"fifo" fifo
    in
    let expected_now = ref (of_string "0001") in
    let input = I.map input ~f:(fun v -> v.signal) in
    let output = O.map output ~f:(fun v -> v.signal) in
    let sim =
      Sim.create
        (processes
         @ [ Sim.Process.create [] (fun () -> input.I.read_enable <-- of_string "1")
           ; Sim_interface.create_clock input.I.clock_read ~time:read_clock_time
           ; Sim_interface.create_clock input.I.clock_write ~time:write_clock_time
           ; Sim.Process.create [ !&(input.I.clock_write) ] (fun () ->
               if Logic.compare !!(input.I.data_in) (of_string "1111") = 0
               then input.I.write_enable <-- of_string "0"
               else (
                 input.I.write_enable <-- of_string "1";
                 if is_gnd !!(input.I.clock_write)
                 then
                   if is_gnd !!(output.O.full)
                   then input.I.data_in <-- !!(input.I.data_in) +:. 1))
           ; Sim.Process.create [ !&(input.I.clock_read) ] (fun () ->
               if is_gnd !!(input.I.clock_read)
               then
                 if is_vdd !!(output.O.valid)
                 then (
                   let current_value = !!(output.O.data_out) in
                   if not (Logic.compare current_value !expected_now = 0)
                   then
                     printf
                       !"invalid value: %{Logic} %{Logic}\n"
                       current_value
                       !expected_now
                   else expected_now := !expected_now +:. 1))
           ])
    in
    Sim.run ~time_limit:100000 sim;
    printf !"finish: %{Logic}\n" !expected_now
  ;;

  let%expect_test "reader/writer case" =
    reader_writer_test ~read_clock_time:150 ~write_clock_time:320;
    [%expect {| finish: 0000 |}];
    reader_writer_test ~read_clock_time:300 ~write_clock_time:150;
    [%expect {| finish: 0000 |}];
    reader_writer_test ~read_clock_time:300 ~write_clock_time:160;
    [%expect {| finish: 0000 |}];
    reader_writer_test ~read_clock_time:300 ~write_clock_time:290;
    [%expect {| finish: 0000 |}];
    reader_writer_test ~read_clock_time:300 ~write_clock_time:1000;
    [%expect {| finish: 0000 |}]
  ;;
end

module _ = Test (Hardcaml_event_driven_sim.Four_state_logic)
module _ = Test (Hardcaml_event_driven_sim.Two_state_logic)
