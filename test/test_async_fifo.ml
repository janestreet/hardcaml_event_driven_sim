open! Core
open Core

module Test (Simulator : Hardcaml_event_driven_sim.S) = struct
  open Simulator

  module M = Hardcaml.Async_fifo.Make (struct
      let width = 4
      let log2_depth = 3
      let optimize_for_same_clock_rate_and_always_reading = false
    end)

  module I = M.I
  module O = M.O
  module Sim_interface = With_interface (I) (O)

  let%expect_test "full/valid state" =
    let open Simulator in
    let open Async in
    let open Logic in
    let fifo = M.create ~scope:(Hardcaml.Scope.create ~flatten_design:true ()) in
    let { Sim_interface.processes; input; output; internal = _; memories = _ } =
      Sim_interface.create fifo
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
                 if not (to_bool !!(output.O.full))
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
      t=660 valid=0
      |}]
  ;;

  let reader_writer_test
    ?(print_waves = false)
    ?use_negedge_sync_chain
    ()
    ~read_clock_time
    ~write_clock_time
    =
    let module Sim = Event_driven_sim.Simulator in
    let module Sim_interface = Hardcaml_event_driven_sim.With_interface (Logic) (I) (O) in
    let open Sim in
    let open Logic in
    let fifo =
      M.create
        ?use_negedge_sync_chain
        ~scope:(Hardcaml.Scope.create ~flatten_design:true ())
    in
    let expected_now = ref (of_string "0001") in
    let waves, { Sim_interface.simulator = sim; _ } =
      Sim_interface.with_waveterm
        ~config:Hardcaml_event_driven_sim.Config.trace_all
        fifo
        (fun input output ->
           let input = I.map input ~f:(fun v -> v.signal) in
           let output = O.map output ~f:(fun v -> v.signal) in
           [ Sim.Process.create [] (fun () -> input.I.read_enable <-- of_string "1")
           ; Sim_interface.create_clock input.I.clock_read ~time:read_clock_time
           ; Sim_interface.create_clock input.I.clock_write ~time:write_clock_time
           ; Sim.Process.create [ !&(input.I.clock_write) ] (fun () ->
               if Logic.compare !!(input.I.data_in) (of_string "1111") = 0
               then input.I.write_enable <-- of_string "0"
               else (
                 input.I.write_enable <-- of_string "1";
                 if not (to_bool !!(input.I.clock_write))
                 then
                   if not (to_bool !!(output.O.full))
                   then input.I.data_in <-- !!(input.I.data_in) +:. 1))
           ; Sim.Process.create [ !&(input.I.clock_read) ] (fun () ->
               if not (to_bool !!(input.I.clock_read))
               then
                 if to_bool !!(output.O.valid)
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
    printf !"finish: %{Logic}\n" !expected_now;
    if print_waves
    then
      let open Hardcaml_waveterm_kernel in
      let display_rules =
        Display_rule.
          [ port_name_is "clock_read" ~wave_format:Bit
          ; port_name_is "clock_write" ~wave_format:Bit
          ; port_name_is ~alignment:Right "data_in" ~wave_format:Hex
          ; port_name_is ~alignment:Right "read_enable" ~wave_format:Bit
          ; port_name_is ~alignment:Right "write_enable" ~wave_format:Bit
          ; port_name_is ~alignment:Right "almost_empty" ~wave_format:Bit
          ; port_name_is ~alignment:Right "data_out" ~wave_format:Hex
          ; port_name_is ~alignment:Right "full" ~wave_format:Bit
          ; port_name_is ~alignment:Right "valid" ~wave_format:Bit
          ; port_name_is ~alignment:Right "raddr_rd" ~wave_format:Hex
          ; port_name_is ~alignment:Right "raddr_wd_ff_0" ~wave_format:Hex
          ; port_name_is ~alignment:Right "raddr_wd" ~wave_format:Hex
          ; port_name_is ~alignment:Right "waddr_wd" ~wave_format:Hex
          ; port_name_is ~alignment:Right "waddr_rd_ff_0" ~wave_format:Hex
          ; port_name_is ~alignment:Right "waddr_rd" ~wave_format:Hex
          ]
      in
      Hardcaml_event_driven_sim.Waveterm.Waveform.expect
        waves
        ~display_rules
        ~wave_width:(-40)
        ~display_width:130
  ;;

  let%expect_test "reader/writer case" =
    reader_writer_test () ~read_clock_time:150 ~write_clock_time:320;
    [%expect {| finish: 0000 |}];
    reader_writer_test () ~read_clock_time:300 ~write_clock_time:150;
    [%expect {| finish: 0000 |}];
    reader_writer_test () ~read_clock_time:300 ~write_clock_time:160;
    [%expect {| finish: 0000 |}];
    reader_writer_test () ~read_clock_time:300 ~write_clock_time:290;
    [%expect {| finish: 0000 |}];
    reader_writer_test () ~read_clock_time:300 ~write_clock_time:1000;
    [%expect {| finish: 0000 |}]
  ;;

  let%expect_test "negedge waveform" =
    reader_writer_test
      ~print_waves:true
      ~use_negedge_sync_chain:true
      ()
      ~read_clock_time:300
      ~write_clock_time:150;
    [%expect
      {|
      finish: 0000
      ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
      │clock_read        ││       ╥───────┐      ╥───────┐      ╥───────┐      ╥───────┐      ╥───────┐      ╥───────┐      ╥───────┐  │
      │                  ││───────╨       └──────╨       └──────╨       └──────╨       └──────╨       └──────╨       └──────╨       └──│
      │clock_write       ││   ╥───╥   ╥───┐  ╥───╥   ╥───┐  ╥───╥   ╥───┐  ╥───╥   ╥───┐  ╥───╥   ╥───┐  ╥───╥   ╥───┐  ╥───╥   ╥───┐  │
      │                  ││───╨   ╨───╨   └──╨   ╨───╨   └──╨   ╨───╨   └──╨   ╨───╨   └──╨   ╨───╨   └──╨   ╨───╨   └──╨   ╨───╨   └──│
      │                  ││───────╥───────┬──────╥───────┬──────╥───────┬──────╥──────────────╥──────────────╥──────────────╥──────────│
      │data_in           ││ 1     ║2      │3     ║4      │5     ║6      │7     ║8             ║9             ║A             ║B         │
      │                  ││───────╨───────┴──────╨───────┴──────╨───────┴──────╨──────────────╨──────────────╨──────────────╨──────────│
      │read_enable       ││────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │                  ││                                                                                                            │
      │write_enable      ││────────────────────────────────────────────────────────────────────────────────────────────────────────────│
      │                  ││                                                                                                            │
      │almost_empty      ││─────────────────────────────────────╥                                                                      │
      │                  ││                                     ╨──────────────────────────────────────────────────────────────────────│
      │                  ││───────╥─────────────────────────────╥──────────────╥──────────────╥──────────────╥──────────────╥──────────│
      │data_out          ││ 0     ║1                            ║2             ║3             ║4             ║5             ║6         │
      │                  ││───────╨─────────────────────────────╨──────────────╨──────────────╨──────────────╨──────────────╨──────────│
      │full              ││                                                        ╥──────╥       ╥──────╥       ╥──────╥       ╥──────│
      │                  ││────────────────────────────────────────────────────────╨      ╨───────╨      ╨───────╨      ╨───────╨      │
      │valid             ││                      ╥─────────────────────────────────────────────────────────────────────────────────────│
      │                  ││──────────────────────╨                                                                                     │
      │                  ││─────────────────────────────────────╥──────────────╥──────────────╥──────────────╥──────────────╥──────────│
      │raddr_rd          ││ 0                                   ║1             ║3             ║2             ║6             ║7         │
      │                  ││─────────────────────────────────────╨──────────────╨──────────────╨──────────────╨──────────────╨──────────│
      │                  ││─────────────────────────────────────────────┬──────────────┬──────────────┬──────────────┬──────────────┬──│
      │raddr_wd_ff_0     ││ 0                                           │1             │3             │2             │6             │7 │
      │                  ││─────────────────────────────────────────────┴──────────────┴──────────────┴──────────────┴──────────────┴──│
      │                  ││────────────────────────────────────────────────╥──────────────╥──────────────╥──────────────╥──────────────│
      │raddr_wd          ││ 0                                              ║1             ║3             ║2             ║6             │
      │                  ││────────────────────────────────────────────────╨──────────────╨──────────────╨──────────────╨──────────────│
      │                  ││───╥───────╥──────╥───────╥──────╥───────╥──────╥───────╥──────────────╥──────────────╥──────────────╥──────│
      │waddr_wd          ││ 0 ║1      ║3     ║2      ║6     ║7      ║5     ║4      ║0             ║1             ║3             ║2     │
      │                  ││───╨───────╨──────╨───────╨──────╨───────╨──────╨───────╨──────────────╨──────────────╨──────────────╨──────│
      │                  ││───────────────┬──────────────┬──────────────┬──────────────┬──────────────┬──────────────┬──────────────┬──│
      │waddr_rd_ff_0     ││ 0             │3             │6             │5             │0             │1             │3             │2 │
      │                  ││───────────────┴──────────────┴──────────────┴──────────────┴──────────────┴──────────────┴──────────────┴──│
      │                  ││──────────────────────╥──────────────╥──────────────╥──────────────╥──────────────╥──────────────╥──────────│
      │waddr_rd          ││ 0                    ║3             ║6             ║5             ║0             ║1             ║3         │
      │                  ││──────────────────────╨──────────────╨──────────────╨──────────────╨──────────────╨──────────────╨──────────│
      └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
      f3e6b921494d4e1a2828df4675d528e3
      |}]
  ;;
end

module%test With_four_state_logic = Test (Hardcaml_event_driven_sim.Four_state_simulator)
module%test With_two_state_logic = Test (Hardcaml_event_driven_sim.Two_state_simulator)
