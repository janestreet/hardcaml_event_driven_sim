open Hardcaml.Signal

module Test (Logic : Hardcaml_event_driven_sim.Logic.S) = struct
  module Sim = Hardcaml_event_driven_sim.Make (Logic)

  let bits = 4

  module I = struct
    type 'a t =
      { a : 'a [@bits bits]
      ; b : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { c : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let f i = { O.c = i.I.a +: i.I.b }

  let%expect_test "adder" =
    let open Logic in
    let open Sim.Event_simulator in
    let module Sim_interface = Sim.With_interface (I) (O) in
    let { Sim_interface.processes; input; output } =
      Sim_interface.create ~name:"adder" f
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
    t=90 c=0001 |}]
  ;;
end

module _ = Test (Hardcaml_event_driven_sim.Four_state_logic)
module _ = Test (Hardcaml_event_driven_sim.Two_state_logic)
