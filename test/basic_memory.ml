open Core
open Hardcaml_event_driven_sim.Four_state_simulator

module I = struct
  type 'a t =
    { read_address : 'a [@bits 4]
    ; write_address : 'a [@bits 4]
    ; write_enable : 'a [@bits 1]
    ; write_clock : 'a [@bits 1]
    ; write_data : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

let size = 15

module O = struct
  type 'a t = { read_data : 'a [@bits 8] } [@@deriving hardcaml]
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
      ~initialize_to:
        (Array.init size ~f:(fun i -> Hardcaml.Bits.of_int_trunc ~width:8 (i + 1)))
      size
  in
  { O.read_data = read_data_array.(0) }
;;

let%expect_test "basic_memory" =
  let module Sim_interface = With_interface (I) (O) in
  let open Simulator in
  let open Logic in
  let { Sim_interface.processes; input; output; internal = _; memories = _ } =
    Sim_interface.create f
  in
  let input = I.map input ~f:(fun v -> v.signal) in
  let output = O.map output ~f:(fun v -> v.signal) in
  let sim =
    create
      (processes
       @ [ Debug.print_signal "read_data" output.O.read_data
         ; Sim_interface.create_clock input.I.write_clock ~time:10
         ; Async.create_process (fun () ->
             input.I.write_enable <-- of_string "1";
             input.I.write_address <-- of_string "1001";
             input.I.write_data <-- of_string "11000001";
             Async.delay 30)
         ; Process.create
             [ !&(input.I.write_clock) ]
             (let address = ref 0 in
              fun () ->
                input.I.read_address <-- of_int_trunc ~width:4 !address;
                address := min (size - 1) (!address + 1))
         ])
  in
  run ~time_limit:300 sim;
  [%expect
    {|
    t=0 read_data=00000001
    t=10 read_data=00000010
    t=20 read_data=00000011
    t=30 read_data=00000100
    t=40 read_data=00000101
    t=50 read_data=00000110
    t=60 read_data=00000111
    t=70 read_data=00001000
    t=80 read_data=00001001
    t=90 read_data=11000001
    t=100 read_data=00001011
    t=110 read_data=00001100
    t=120 read_data=00001101
    t=130 read_data=00001110
    t=140 read_data=00001111
    |}]
;;
