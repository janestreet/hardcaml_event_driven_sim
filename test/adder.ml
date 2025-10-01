open Hardcaml.Signal

module Test (Simulator : Hardcaml_event_driven_sim.S) = struct
  open Simulator

  let bits = 4

  module I = struct
    type 'a t =
      { a : 'a [@bits bits]
      ; b : 'a [@bits bits]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { c : 'a [@bits bits] } [@@deriving hardcaml]
  end

  let f i = { O.c = i.I.a +: i.I.b }

  let%expect_test "adder" =
    let open Logic in
    let open Simulator in
    let module Sim_interface = With_interface (I) (O) in
    let { Sim_interface.processes; input; output; internal = _; memories = _ } =
      Sim_interface.create f
    in
    let input = I.map input ~f:(fun v -> v.signal) in
    let output = O.map output ~f:(fun v -> v.signal) in
    let sim =
      create
        (processes
         @ [ Debug.print_signal "c" output.O.c
           ; Process.create [] (fun () -> input.I.b <-- of_string "1000")
           ; Process.create [ !&(input.a) ] (fun () ->
               (input.I.a <--- !!(input.I.a) +:. 1) ~delay:10)
           ])
    in
    run ~time_limit:100 sim;
    [%expect
      {|
      t=0 c=1000
      t=10 c=1001
      t=20 c=1010
      t=30 c=1011
      t=40 c=1100
      t=50 c=1101
      t=60 c=1110
      t=70 c=1111
      t=80 c=0000
      t=90 c=0001
      |}]
  ;;

  let%expect_test "waveform" =
    let open Logic in
    let open Simulator in
    let module Sim_interface = With_interface (I) (O) in
    let waves, { Sim_interface.ports_and_processes = _; simulator } =
      Sim_interface.with_waveterm f (fun input _ ->
        let input = I.map input ~f:(fun v -> v.signal) in
        [ Process.create [] (fun () -> input.I.b <-- of_string "1000")
        ; Process.create [ !&(input.a) ] (fun () ->
            (input.I.a <--- !!(input.I.a) +:. 1) ~delay:10)
        ])
    in
    run ~time_limit:100 simulator;
    Core.print_s [%message (waves : Waveterm.Waveform.t)];
    Waveterm.Waveform.expect waves ~wave_width:(-3);
    [%expect
      {|
      (waves
       ((waves
         ((Data (name a)
           (data
            ((t
              ((data
                (0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 "" "" "" "" ""
                 ""))
               (time (0 10 20 30 40 50 60 70 80 90 0 0 0 0 0 0)) (length 10)))
             (width 4) (max_time 90)))
           (wave_format ((current (Bit_or Hex)) (default (Bit_or Hex))))
           (text_alignment Left)
           (style ((style ((bold false) (fg White) (bg Black))))))
          (Data (name b)
           (data
            ((t ((data (1000 "")) (time (0 0)) (length 1))) (width 4)
             (max_time 90)))
           (wave_format ((current (Bit_or Hex)) (default (Bit_or Hex))))
           (text_alignment Left)
           (style ((style ((bold false) (fg White) (bg Black))))))
          (Data (name c)
           (data
            ((t
              ((data
                (1000 1001 1010 1011 1100 1101 1110 1111 0000 0001 "" "" "" "" ""
                 ""))
               (time (0 10 20 30 40 50 60 70 80 90 0 0 0 0 0 0)) (length 10)))
             (width 4) (max_time 90)))
           (wave_format ((current (Bit_or Hex)) (default (Bit_or Hex))))
           (text_alignment Left)
           (style ((style ((bold false) (fg White) (bg Black))))))))
        (ports
         (((type_ Internal) (port_name a) (width 4))
          ((type_ Internal) (port_name b) (width 4))
          ((type_ Internal) (port_name c) (width 4))))))
      ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
      │               ││───╥──╥───┬──╥──╥───┬──╥──╥───┬                    │
      │a              ││ 0 ║1 ║2  │3 ║4 ║5  │6 ║7 ║8  │                    │
      │               ││───╨──╨───┴──╨──╨───┴──╨──╨───┴                    │
      │               ││───────────────────────────────                    │
      │b              ││ 8                                                 │
      │               ││───────────────────────────────                    │
      │               ││───╥──╥───┬──╥──╥───┬──╥──╥───┬                    │
      │c              ││ 8 ║9 ║A  │B ║C ║D  │E ║F ║0  │                    │
      │               ││───╨──╨───┴──╨──╨───┴──╨──╨───┴                    │
      └───────────────┘└───────────────────────────────────────────────────┘
      deacc79608dafd064470bfe1e9ae2338
      |}]
  ;;
end

module%test Four_state = Test (Hardcaml_event_driven_sim.Four_state_simulator)
module%test Two_state = Test (Hardcaml_event_driven_sim.Two_state_simulator)
