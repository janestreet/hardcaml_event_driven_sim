open! Core
open Hardcaml
open Hardcaml_event_driven_sim.Two_state_simulator

module type Test = sig
  val width_ : int
end

module Make_test (Test : Test) = struct
  include Test

  let memory_name = "mem"

  module Input = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module Output = struct
    type 'a t = { value : 'a [@bits width_] } [@@deriving hardcaml]
  end

  module Sim_interface = With_interface (Input) (Output)

  let circuit ({ clock } : _ Input.t) =
    let open Signal in
    let incr_by = sll (one width_) ~by:(width_ - 2) -:. 3 in
    let memory_output = wire width_ in
    let log_memory_size = 3 in
    let address = of_int_trunc ~width:log_memory_size 5 in
    let read_out =
      let size = 1 lsl log_memory_size in
      (multiport_memory
         size
         ~initialize_to:(Array.init size ~f:(Fn.const (Bits.one width_)))
         ~write_ports:
           [| { write_clock = clock
              ; write_address = address
              ; write_enable = vdd
              ; write_data = memory_output +: incr_by
              }
           |]
         ~read_addresses:[| address |]).(0)
    in
    (match read_out with
     | Mem_read_port { memory; _ } ->
       let (_ : t) = memory -- memory_name in
       ()
     | _ -> raise_s [%message "unexpected"]);
    memory_output <-- read_out;
    { Output.value = memory_output }
  ;;

  let run_test ~sim_mode =
    let { Sim_interface.ports_and_processes = { memories; _ }; simulator } =
      Sim_interface.with_processes
        ~config:{ Config.trace_all with sim_mode; combine_wires = true }
        circuit
        (fun { clock } { value = _ } ->
           [ Sim_interface.create_clock ~time:1 clock.signal ])
    in
    for _ = 1 to 4 do
      for _ = 1 to 6 do
        Event_driven_sim.Simulator.step simulator
      done;
      let memory = Map.find_exn memories memory_name |> List.hd_exn in
      let value = Private.Simulation_memory.get memory 5 in
      print_s [%message (value : Logic.t)]
    done
  ;;
end

let test_verilator =
  Sim_mode.Hybrid
    { cyclesim_create =
        (fun ~config ~clock_names circuit ->
          Hardcaml_verilator.create ~config ~clock_names circuit)
    }
;;

let sim_modes_to_test : Sim_mode.t list =
  [ Evsim; Sim_mode.default_hybrid; test_verilator ]
;;

let%expect_test "increment memory value small" =
  let open Make_test (struct
      let width_ = 4
    end) in
  List.iter sim_modes_to_test ~f:(fun sim_mode ->
    run_test ~sim_mode;
    [%expect
      {|
      (value 0100)
      (value 0111)
      (value 1010)
      (value 1101)
      |}])
;;

let%expect_test "increment memory value medium" =
  let open Make_test (struct
      let width_ = 12
    end) in
  List.iter sim_modes_to_test ~f:(fun sim_mode ->
    run_test ~sim_mode;
    [%expect
      {|
      (value 101111111000)
      (value 011111101111)
      (value 001111100110)
      (value 111111011101)
      |}])
;;

let%expect_test "increment memory value large" =
  let open Make_test (struct
      let width_ = 25
    end) in
  List.iter sim_modes_to_test ~f:(fun sim_mode ->
    run_test ~sim_mode;
    [%expect
      {|
      (value 1011111111111111111111000)
      (value 0111111111111111111101111)
      (value 0011111111111111111100110)
      (value 1111111111111111111011101)
      |}])
;;

let%expect_test "increment memory value xl" =
  let open Make_test (struct
      let width_ = 81
    end) in
  List.iter sim_modes_to_test ~f:(fun sim_mode ->
    run_test ~sim_mode;
    [%expect
      {|
      (value
       101111111111111111111111111111111111111111111111111111111111111111111111111111000)
      (value
       011111111111111111111111111111111111111111111111111111111111111111111111111101111)
      (value
       001111111111111111111111111111111111111111111111111111111111111111111111111100110)
      (value
       111111111111111111111111111111111111111111111111111111111111111111111111111011101)
      |}])
;;
