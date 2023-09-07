open! Core
open Hardcaml.Signal
module Sim = Hardcaml_event_driven_sim.Make (Hardcaml_event_driven_sim.Four_state_logic)

let%expect_test "simple comb" =
  let open Sim.Event_simulator in
  let circuit =
    let a = of_string "01" +: of_string "10" in
    Hardcaml.Circuit.create_exn ~name:"const" [ output "a" a ]
  in
  let ops = Sim.Ops.circuit_to_processes circuit in
  let sim = Sim.Event_simulator.create (Sim.Ops.processes ops) in
  let outputs = Hardcaml.Circuit.outputs circuit in
  let a = List.nth_exn outputs 0 in
  Sim.Event_simulator.stabilise sim;
  print_s ([%sexp_of: Sim.Logic.t] !!(Sim.Ops.find_sim_signal ops a));
  [%expect {| 11 |}]
;;

let%expect_test "simple time" =
  let input = input "input" 2 in
  let circuit =
    let a = of_string "01" +: input in
    Hardcaml.Circuit.create_exn ~name:"timed" [ output "a" a ]
  in
  let ops = Sim.Ops.circuit_to_processes circuit in
  let inputs = Hardcaml.Circuit.inputs circuit in
  let sig_input = Sim.Ops.find_sim_signal ops (List.nth_exn inputs 0) in
  let outputs = Hardcaml.Circuit.outputs circuit in
  let sig_a = Sim.Ops.find_sim_signal ops (List.nth_exn outputs 0) in
  let open Sim.Event_simulator in
  let sim =
    create
      (Sim.Ops.processes ops
       @ [ Process.create [ !&sig_input ] (fun () ->
             (sig_input <--- Sim.Logic.( +:. ) !!sig_input 1) ~delay:20)
         ; Debug.print_signal "a" sig_a
         ])
  in
  Sim.Event_simulator.run ~time_limit:201 sim;
  [%expect
    {|
    t=0 a=01
    t=20 a=10
    t=40 a=11
    t=60 a=00
    t=80 a=01
    t=100 a=10
    t=120 a=11
    t=140 a=00
    t=160 a=01
    t=180 a=10
    t=200 a=11 |}]
;;

let%expect_test "signals optimized out" =
  let open struct
    module I = struct
      type 'a t =
        { a : 'a [@bits 2]
        ; b : 'a [@bits 2]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { c : 'a [@bits 2] } [@@deriving sexp_of, hardcaml]
    end

    let f i = { O.c = i.I.a +: of_string "01" }
  end in
  let open Sim.Logic in
  let open Sim.Event_simulator in
  let module Sim_interface = Sim.With_interface (I) (O) in
  let { Sim_interface.processes; input; output; internal = _ } = Sim_interface.create f in
  let input = I.map input ~f:(fun v -> v.signal) in
  let output = O.map output ~f:(fun v -> v.signal) in
  let sim =
    create
      (processes
       @ [ Debug.print_signal "c" output.O.c
         ; Process.create [] (fun () -> input.I.b <-- of_string "10")
         ; Process.create [ !&(input.a) ] (fun () ->
             (input.I.a <--- !!(input.I.a) +:. 1) ~delay:10)
         ])
  in
  run ~time_limit:100 sim;
  [%expect
    {|
    t=0 c=01
    t=10 c=10
    t=20 c=11
    t=30 c=00
    t=40 c=01
    t=50 c=10
    t=60 c=11
    t=70 c=00
    t=80 c=01
    t=90 c=10 |}]
;;
