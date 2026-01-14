(* based on example from hardcaml-tutorials *)
open Core
open Hardcaml.Signal

module Test (Simulator : Hardcaml_event_driven_sim.S) = struct
  open Simulator

  module I = struct
    type 'a t =
      { incr : 'a [@bits 1]
      ; amount : 'a [@bits 4]
      ; clock : 'a [@bits 1]
      }
    [@@deriving hardcaml ~rtlprefix:"i$"]
  end

  module O = struct
    type 'a t = { total : 'a [@bits 8] } [@@deriving hardcaml ~rtlprefix:"o$"]
  end

  (* Increment [total] by [amount] whenever [incr] is high. Whenever [total] overflows,
     set it to 0. All values are unsigned. *)
  let f i =
    let reg_spec = Reg_spec.create () ~clock:i.I.clock in
    let total =
      reg_fb reg_spec ~enable:i.I.incr ~width:8 ~f:(fun d ->
        (* Artificially induce the same effect as a module hierarchy for testing that it
           is parsed properly. More in-depth testing of the VCD module is done in the main
           [test_vcd.ml] in the Hardcaml tests, this check is primarily to verify that the
           event-sim VCD module is calling into the correct version of the Hardcaml VCD
           functions. *)
        let d = Unsigned.(d +: i.I.amount) -- "deep$module$hierarchy$NEXT" in
        mux2 (msb d) (zero 8) (lsbs d))
    in
    { O.total }
  ;;

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
         @ [ Debug.print_signal "total" output.O.total
           ; Debug.print_signal "clock" input.I.clock
           ; Sim_interface.create_clock input.I.clock ~time:10
           ; Process.create [] (fun () ->
               input.I.amount <-- of_string "0001";
               input.I.incr <-- of_string "1")
           ])
    in
    run ~time_limit:150 sim;
    [%expect
      {|
      t=10 clock=1
      t=10 total=00000001
      t=20 clock=0
      t=30 clock=1
      t=30 total=00000010
      t=40 clock=0
      t=50 clock=1
      t=50 total=00000011
      t=60 clock=0
      t=70 clock=1
      t=70 total=00000100
      t=80 clock=0
      t=90 clock=1
      t=90 total=00000101
      t=100 clock=0
      t=110 clock=1
      t=110 total=00000110
      t=120 clock=0
      t=130 clock=1
      t=130 total=00000111
      t=140 clock=0
      |}]
  ;;
end

module%test Two_state = Test (Hardcaml_event_driven_sim.Two_state_simulator)
module Four_state = Test (Hardcaml_event_driven_sim.Four_state_simulator)

let%expect_test "adder - vcd" =
  let open Four_state in
  let open Hardcaml_event_driven_sim.Four_state_simulator in
  let open Logic in
  let open Simulator in
  let module Sim_interface = With_interface (I) (O) in
  let { Sim_interface.simulator = sim; _ } =
    Sim_interface.with_vcd
      ~config:Config.trace_all
      ~vcd:Out_channel.stdout
      f
      (fun input _output ->
         [ Sim_interface.create_clock input.I.clock.signal ~time:10
         ; Process.create [] (fun () ->
             input.I.amount.signal <-- of_string "0001";
             input.I.incr.signal <-- of_string "1")
         ])
  in
  run ~time_limit:150 sim;
  [%expect.output] |> Hardcaml_test.Test_vcd_hierarchy.add_indentation |> print_endline;
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml-evsim
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module traced $end
      $var wire 1 & gnd $end
      $scope module -inputs $end
        $var wire 4 " amount $end
        $var wire 1 # clock $end
        $var wire 1 ! incr $end
      $upscope $end
      $scope module -outputs $end
        $var wire 8 $ total $end
      $upscope $end
      $scope module deep $end
        $scope module module $end
          $scope module hierarchy $end
            $var wire 9 % NEXT $end
          $upscope $end
        $upscope $end
      $upscope $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x&
    bxxxx "
    x#
    x!
    bxxxxxxxx $
    bxxxxxxxxx %
    $end
    #0
    0&
    b00000000 $
    1!
    b0001 "
    0#
    b000000001 %
    #10
    b00000001 $
    1#
    b000000010 %
    #20
    0#
    #30
    b00000010 $
    1#
    b000000011 %
    #40
    0#
    #50
    b00000011 $
    1#
    b000000100 %
    #60
    0#
    #70
    b00000100 $
    1#
    b000000101 %
    #80
    0#
    #90
    b00000101 $
    1#
    b000000110 %
    #100
    0#
    #110
    b00000110 $
    1#
    b000000111 %
    #120
    0#
    #130
    b00000111 $
    1#
    b000001000 %
    #140
    0#
    |}]
;;
