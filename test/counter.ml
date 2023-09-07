(* based on example from hardcaml-tutorials *)
open Core
open Hardcaml.Signal

module Test (Logic : Hardcaml_event_driven_sim.Logic.S) = struct
  module Sim = Hardcaml_event_driven_sim.Make (Logic)

  module I = struct
    type 'a t =
      { incr : 'a [@bits 1]
      ; amount : 'a [@bits 4]
      ; clock : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { total : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
  end

  (* Increment [total] by [amount] whenever [incr] is high.  Whenever [total] overflows, set
     it to 0.  All values are unsigned. *)
  let f i =
    let reg_spec = Hardcaml.Reg_spec.create () ~clock:i.I.clock in
    let total =
      reg_fb reg_spec ~enable:i.I.incr ~width:8 ~f:(fun d ->
        let d = Uop.(d +: i.I.amount) -- "NEXT" in
        mux2 (msb d) (zero 8) (lsbs d))
    in
    { O.total }
  ;;

  let%expect_test "adder" =
    let open Logic in
    let open Sim.Event_simulator in
    let module Sim_interface = Sim.With_interface (I) (O) in
    let { Sim_interface.processes; input; output; internal = _ } =
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
    t=140 clock=0 |}]
  ;;
end

module _ = Test (Hardcaml_event_driven_sim.Two_state_logic)
module Four_state = Test (Hardcaml_event_driven_sim.Four_state_logic)

let%expect_test "adder - vcd" =
  let open Hardcaml_event_driven_sim.Four_state_logic in
  let open Four_state in
  let open Four_state.Sim.Event_simulator in
  let module Sim_interface = Sim.With_interface (I) (O) in
  let sim =
    Sim_interface.with_vcd
      ~config:Sim.Config.trace_all
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
  [%expect
    {|
    $date
      ...
    $end
    $version
      Hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module traced $end
    $var wire 1 ! incr $end
    $var wire 4 " amount $end
    $var wire 1 # clock $end
    $var wire 8 $ total $end
    $var wire 1 % gnd $end
    $var wire 9 & NEXT $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    bxxxx "
    x#
    bxxxxxxxx $
    x%
    bxxxxxxxxx &
    $end
    #0
    b0001 "
    1!
    b000000001 &
    #10
    b000000010 &
    1#
    b00000001 $
    #20
    0#
    #30
    b000000011 &
    1#
    b00000010 $
    #40
    0#
    #50
    b000000100 &
    1#
    b00000011 $
    #60
    0#
    #70
    b000000101 &
    1#
    b00000100 $
    #80
    0#
    #90
    b000000110 &
    1#
    b00000101 $
    #100
    0#
    #110
    b000000111 &
    1#
    b00000110 $
    #120
    0#
    #130
    b000001000 &
    1#
    b00000111 $
    #140
    0# |}]
;;
