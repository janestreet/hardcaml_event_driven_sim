open Core
module Logic = Hardcaml_event_driven_sim.Four_state_logic
module Sim = Hardcaml_event_driven_sim.Make (Logic)

module I = struct
  type 'a t =
    { read_address : 'a [@bits 4]
    ; write_address : 'a [@bits 4]
    ; write_enable : 'a [@bits 1]
    ; write_clock : 'a [@bits 1]
    ; write_data : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

let size = 15

module O = struct
  type 'a t = { read_data : 'a [@bits 8] } [@@deriving sexp_of, hardcaml]
end

let f i =
  let read_data_array =
    Hardcaml.Signal.multiport_memory
      ~read_addresses:[| i.I.read_address |]
      ~write_ports:
        [| { write_address = i.I.write_address
           ; write_enable = i.I.write_enable
           ; write_clock = i.I.write_clock
           ; write_data = i.I.write_data
           }
        |]
      size
  in
  { O.read_data = read_data_array.(0) }
;;

let%expect_test "basic_memory" =
  let module Sim_interface = Sim.With_interface (I) (O) in
  let open Sim.Event_simulator in
  let open Hardcaml_event_driven_sim.Four_state_logic in
  let { Sim_interface.processes; input; output } = Sim_interface.create ~name:"adder" f in
  let input = I.map input ~f:(fun v -> v.signal) in
  let output = O.map output ~f:(fun v -> v.signal) in
  let sim =
    create
      (processes
       @ [ Debug.print_signal "read_data" output.O.read_data
         ; Sim_interface.create_clock input.I.write_clock ~time:10
         ; Async.create_process (fun () ->
             input.I.write_enable <-- of_string "1";
             input.I.write_address <-- of_string "0000";
             input.I.write_data <-- of_string "11000001";
             Async.delay 30)
         ])
  in
  run ~time_limit:300 sim;
  [%expect {|
    t=10 read_data=11000001 |}]
;;
