open! Core
open Hardcaml

module Make_test (Input : Interface.S) (Output : Interface.S) : sig
  val show_domains : (Signal.t Input.t -> Signal.t Output.t) -> unit

  val test
    :  ?config:Hardcaml_event_driven_sim.Config.t
    -> (Signal.t Input.t -> Signal.t Output.t)
    -> handle_input:
         (Bits.t Hardcaml_event_driven_sim.Port.t Input.t
          -> create_clock:
               (?initial_delay:int
                -> time:int
                -> Bits.t Event_driven_sim.Simulator.Signal.t
                -> Event_driven_sim.Simulator.Process.t)
          -> Event_driven_sim.Simulator.Process.t Input.t)
    -> sim_mode:Hardcaml_event_driven_sim.Sim_mode.t
    -> unit
end = struct
  open Hardcaml_event_driven_sim.Two_state_simulator
  module Sim_interface = With_interface (Input) (Output)

  module Clock_domain_splitting_test =
    Test_clock_domain_splitting.Make_test (Input) (Output)

  let show_domains circuit = Clock_domain_splitting_test.test circuit

  let test ?(config = Config.trace_all) circuit ~handle_input ~sim_mode =
    let%tydi waves, { simulator; _ } =
      Sim_interface.with_waveterm
        ~config:{ config with sim_mode; combine_wires = true }
        circuit
        (fun input_ports _output_ports ->
           handle_input
             input_ports
             ~create_clock:(Sim_interface.create_clock ~here:[%here])
           |> Input.to_list)
    in
    for _ = 1 to 20 do
      Event_driven_sim.Simulator.step simulator
    done;
    Hardcaml_event_driven_sim.Waveterm.Waveform.expect
      waves
      ~wave_width:(-1)
      ~display_width:40
  ;;
end

let test_verilator =
  Hardcaml_event_driven_sim.Sim_mode.Hybrid
    { cyclesim_create =
        (fun ~config ~clock_names circuit ->
          Hardcaml_verilator.create ~config ~clock_names circuit)
    }
;;

let sim_modes_to_test = Hardcaml_event_driven_sim.Sim_mode.all @ [ test_verilator ]

module%test _ = struct
  module I = struct
    type 'a t =
      { clock_1 : 'a [@bits 1]
      ; clock_2 : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ clock_1; clock_2 } : _ I.t) =
    let open Signal in
    let zero = zero 1 -- "zero" in
    let reg_1 = reg (Reg_spec.create () ~clock:clock_1) zero -- "reg_1" in
    let reg_2 =
      reg_fb (Reg_spec.create () ~clock:clock_2) ~width:1 ~f:(fun r -> r +:. 1) -- "reg_2"
    in
    let xor = (reg_1 ^: reg_2) -- "xor" in
    { O.out = xor }
  ;;

  let%expect_test "simple multi clock domain ev-sim test" =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked ((clock (__1)) (edge Rising)))
      (2):            Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      reg_2:          Reg[id:3 bits:1 names:__2 deps:1,2]
      reg_2:          Wire[id:6 bits:1 names:__5 deps:3] -> 3 (output)

      (Clocked ((clock (__1)) (edge Rising)))
      (5):            Wire[id:8 bits:1 names:__1 deps:] -> () (input)
      reg_1:          Reg[id:9 bits:1 names:__2 deps:7,8]
      reg_1:          Wire[id:10 bits:1 names:__3 deps:9] -> 9 (output)
      zero:           Const[id:7 bits:1 names:__0 deps:] = 0

      Floating
      (2):            Wire[id:21 bits:1 names:__10 deps:12] -> 12 (output)
      (5):            Wire[id:20 bits:1 names:__9 deps:14] -> 14 (output)
      clock_1:        Wire[id:13 bits:1 names:__2 deps:] -> () (input)
      clock_2:        Wire[id:11 bits:1 names:__0 deps:] -> () (input)
      out:            Wire[id:15 bits:1 names:__4 deps:18] -> 18
      out:            Wire[id:19 bits:1 names:__8 deps:15] -> 15 (output)
      reg_1:          Wire[id:16 bits:1 names:__5 deps:] -> () (input)
      reg_2:          Wire[id:17 bits:1 names:__6 deps:] -> () (input)
      xor:            Op[id:18 bits:1 names:__7 deps:16,17] = xor
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test ~sim_mode circuit ~handle_input:(fun { clock_1; clock_2 } ~create_clock ->
        { clock_1 = create_clock ~time:3 clock_1.signal
        ; clock_2 = create_clock ~time:5 clock_2.signal
        });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock_1 ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │clock_2 ││     ┌────┐    ┌────┐    ┌──│
        │        ││─────┘    └────┘    └────┘  │
        │out     ││     ┌─────────┐         ┌──│
        │        ││─────┘         └─────────┘  │
        │reg_1   ││                            │
        │        ││────────────────────────────│
        │reg_2   ││     ┌─────────┐         ┌──│
        │        ││─────┘         └─────────┘  │
        │xor     ││     ┌─────────┐         ┌──│
        │        ││─────┘         └─────────┘  │
        │zero    ││                            │
        │        ││────────────────────────────│
        └────────┘└────────────────────────────┘
        76628347a5e67dc2316973545c176236
        |}])
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let zero = zero 1 -- "zero" in
    let memory_input = wire 2 -- "mem-in" in
    let memory =
      multiport_memory
        1
        ~write_ports:
          [| { write_clock = clock
             ; write_address = zero
             ; write_enable = vdd
             ; write_data = memory_input
             }
          |]
        ~read_addresses:[| zero |]
    in
    let read_out = memory.(0) -- "read-out" in
    memory_input <-- read_out +:. 1;
    (match read_out with
     | Mem_read_port { memory; _ } ->
       let (_ : t) = memory -- "mem" in
       ()
     | _ -> raise_s [%message "unexpected"]);
    { O.out = read_out }
  ;;

  let%expect_test "Able to handle memories fully inside a clock domain" =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked ((clock (__3)) (edge Rising)))
      (3):            Wire[id:4 bits:1 names:__3 deps:] -> () (input)
      mem-in:         Wire[id:1 bits:2 names:__0 deps:9] -> 9
      mem:            Multiport_mem[id:6 bits:2 names:__5 deps:4,3,1,5]
      out:            Wire[id:10 bits:2 names:__9 deps:2] -> 2 (output)
      out:            Wire[id:2 bits:2 names:__1 deps:7] -> 7
      read-out:       Mem_read_port[id:7 bits:2 names:__6 deps:3,6]
      vdd:            Const[id:5 bits:1 names:__4 deps:] = 1
      zero:           Const[id:3 bits:1 names:__2 deps:] = 0

      Floating
      (3):            Wire[id:13 bits:1 names:__2 deps:12] -> 12 (output)
      clock:          Wire[id:11 bits:1 names:__0 deps:] -> () (input)
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test ~sim_mode circuit ~handle_input:(fun { clock } ~create_clock ->
        { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││────────────────────────────│
        │mem     ││ 0                          │
        │        ││────────────────────────────│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │mem-in  ││ 1 │2    │3    │0    │1    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │read-out││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │vdd     ││────────────────────────────│
        │        ││                            │
        │zero    ││                            │
        │        ││────────────────────────────│
        └────────┘└────────────────────────────┘
        d1d77c5ac728cf6e646f63260ef128e8
        |}])
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out1 : 'a [@bits 2]
      ; out2 : 'a [@bits 2]
      }
    [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let r_signal =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "r-signal"
    in
    let reset_signal = r_signal ==:. 2 in
    let r_regular =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "r-regular"
    in
    let r_reset =
      reg_fb (Reg_spec.create () ~clock ~reset:reset_signal) ~width:2 ~f:(fun r ->
        r +:. 1)
      -- "r-reset"
    in
    { O.out1 = r_regular; out2 = r_reset }
  ;;

  let%expect_test "Registers with different resets are in different domains" =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked ((clock (__3)) (edge Rising)))
      (12):           Wire[id:14 bits:1 names:__13 deps:12] -> 12 (output)
      (5):            Wire[id:4 bits:1 names:__3 deps:] -> () (input)
      out1:           Wire[id:13 bits:2 names:__12 deps:3] -> 3 (output)
      out1:           Wire[id:3 bits:2 names:__2 deps:8] -> 8
      r-regular:      Reg[id:8 bits:2 names:__7 deps:2,4]
      r-signal:       Reg[id:5 bits:2 names:__4 deps:1,4]

      (Clocked
       ((clock (__2)) (edge Rising) (reset ((signal (__3)) (edge Rising)))))
      (12):           Wire[id:18 bits:1 names:__3 deps:] -> () (input)
      (5):            Wire[id:17 bits:1 names:__2 deps:] -> () (input)
      out2:           Wire[id:16 bits:2 names:__1 deps:20] -> 20
      out2:           Wire[id:23 bits:2 names:__8 deps:16] -> 16 (output)
      r-reset:        Reg[id:20 bits:2 names:__5 deps:15,17,18,19]

      Floating
      (5):            Wire[id:26 bits:1 names:__2 deps:25] -> 25 (output)
      clock:          Wire[id:24 bits:1 names:__0 deps:] -> () (input)
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test
        ~config:Hardcaml_event_driven_sim.Config.default
        ~sim_mode
        circuit
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out1    ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out2    ││ 0 │1    │0    │1    │2    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        └────────┘└────────────────────────────┘
        fd1e25e3fa3efb1b673a5e8d07415440
        |}])
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let counter =
      reg_fb (Reg_spec.create () ~clock ~reset:gnd) ~width:2 ~f:(fun r -> r +:. 1)
      -- "counter"
    in
    { O.out = counter }
  ;;

  let%expect_test "able to handle constant resets" =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked
       ((clock (__2)) (edge Rising) (reset ((signal (__3)) (edge Rising)))))
      (2):            Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      counter:        Reg[id:6 bits:2 names:__5 deps:1,3,4,5]
      gnd:            Wire[id:4 bits:1 names:__3 deps:] -> () (input)
      out:            Wire[id:2 bits:2 names:__1 deps:6] -> 6
      out:            Wire[id:9 bits:2 names:__8 deps:2] -> 2 (output)

      Floating
      (2):            Wire[id:14 bits:1 names:__4 deps:11] -> 11 (output)
      clock:          Wire[id:10 bits:1 names:__0 deps:] -> () (input)
      gnd:            Const[id:12 bits:1 names:__2 deps:] = 0
      gnd:            Wire[id:13 bits:1 names:__3 deps:12] -> 12 (output)
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test ~sim_mode circuit ~handle_input:(fun { clock } ~create_clock ->
        { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │counter ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │gnd     ││                            │
        │        ││────────────────────────────│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        └────────┘└────────────────────────────┘
        3b9942c422385ecd25990271ac4baa71
        |}])
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { unused : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let circuit ({ unused = _ } : _ I.t) =
    let open Signal in
    let zero = zero 1 in
    let counter =
      reg_fb (Reg_spec.create () ~clock:zero ~reset:gnd) ~width:2 ~f:(fun r -> r +:. 1)
      -- "counter"
    in
    { O.out = counter }
  ;;

  let%expect_test "constant register clock results in readable error" =
    let module Test = Make_test (I) (O) in
    Expect_test_helpers_base.require_does_raise ~here:[%here] (fun () ->
      Test.test
        ~sim_mode:Hardcaml_event_driven_sim.Sim_mode.default_hybrid
        circuit
        ~handle_input:(fun { unused } ~create_clock ->
          { unused = create_clock unused.signal ~time:1 }));
    [%expect {| ("Clock signal is a constant" (dep (__2))) |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { unused : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let circuit ({ unused = _ } : _ I.t) =
    let open Signal in
    let zero = zero 1 in
    let memory =
      multiport_memory
        1
        ~write_ports:
          [| { write_clock = zero
             ; write_address = zero
             ; write_enable = vdd
             ; write_data = zero
             }
          |]
        ~read_addresses:[| zero |]
    in
    let out = memory.(0) in
    { O.out }
  ;;

  let%expect_test "constant memory clock results in readable error" =
    let module Test = Make_test (I) (O) in
    Expect_test_helpers_base.require_does_raise ~here:[%here] (fun () ->
      Test.test
        ~sim_mode:Hardcaml_event_driven_sim.Sim_mode.default_hybrid
        circuit
        ~handle_input:(fun { unused } ~create_clock ->
          { unused = create_clock unused.signal ~time:1 }));
    [%expect {| ("Clock signal is a constant" (dep (__1))) |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 3] } [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let counter =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "counter"
    in
    let clear_reg = reg (Reg_spec.create () ~clock) (counter ==:. 2) -- "clear" in
    let out_reg =
      let clock = wireof (wireof (wireof (wireof clock))) in
      reg_fb (Reg_spec.create () ~clock ~clear:clear_reg) ~width:3 ~f:(fun r -> r +:. 1)
    in
    { O.out = out_reg }
  ;;

  let%expect_test "able to handle delayed clocks" =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked ((clock (__1)) (edge Rising)))
      (3):            Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      clear:          Reg[id:8 bits:1 names:__7 deps:7,2]
      clear:          Wire[id:9 bits:1 names:__8 deps:8] -> 8 (output)
      counter:        Reg[id:3 bits:2 names:__2 deps:1,2]

      (Clocked ((clock (__2)) (edge Rising)))
      (7):            Wire[id:12 bits:1 names:__2 deps:] -> () (input)
      clear:          Wire[id:13 bits:1 names:__3 deps:] -> () (input)
      out:            Wire[id:11 bits:3 names:__1 deps:15] -> 15
      out:            Wire[id:18 bits:3 names:__8 deps:11] -> 11 (output)

      Floating
      (3):            Wire[id:26 bits:1 names:__7 deps:20] -> 20 (output)
      (7):            Wire[id:25 bits:1 names:__6 deps:24] -> 24 (output)
      clock:          Wire[id:19 bits:1 names:__0 deps:] -> () (input)
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test ~sim_mode circuit ~handle_input:(fun { clock } ~create_clock ->
        { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │clear   ││               ┌─────┐      │
        │        ││───────────────┘     └──────│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │counter ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        └────────┘└────────────────────────────┘
        8bcfcf45eb49b274d1b8c078753aafaa
        |}])
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 3] } [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let div_clock =
      reg_fb (Reg_spec.create () ~clock) ~width:1 ~f:(fun r -> r +:. 1) -- "div-clock"
    in
    let counter =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "counter"
    in
    let clear = (counter ==:. 2) -- "clear" in
    let out_reg =
      reg_fb (Reg_spec.create () ~clock:div_clock ~clear) ~width:3 ~f:(fun r -> r +:. 1)
    in
    { O.out = out_reg }
  ;;

  let%expect_test "a clock divider circuit maybe give different results due to delta \
                   step differences"
    =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked ((clock (__2)) (edge Rising)))
      (3):            Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      clear:          Op[id:11 bits:1 names:__10 deps:4,10] = eq
      clear:          Wire[id:13 bits:1 names:__12 deps:11] -> 11 (output)
      counter:        Reg[id:4 bits:2 names:__3 deps:1,3]
      div-clock:      Reg[id:7 bits:1 names:__6 deps:2,3]
      div-clock:      Wire[id:12 bits:1 names:__11 deps:7] -> 7 (output)

      (Clocked ((clock (__2)) (edge Rising)))
      clear:          Wire[id:17 bits:1 names:__3 deps:] -> () (input)
      div-clock:      Wire[id:16 bits:1 names:__2 deps:] -> () (input)
      out:            Wire[id:15 bits:3 names:__1 deps:19] -> 19
      out:            Wire[id:22 bits:3 names:__8 deps:15] -> 15 (output)

      Floating
      (3):            Wire[id:25 bits:1 names:__2 deps:24] -> 24 (output)
      clock:          Wire[id:23 bits:1 names:__0 deps:] -> () (input)
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test ~sim_mode circuit ~handle_input:(fun { clock } ~create_clock ->
        { clock = create_clock ~time:3 clock.signal }));
    [%expect
      {|
      ┌Signals─┐┌Waves───────────────────────┐
      │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
      │        ││───┘  └──┘  └──┘  └──┘  └──┘│
      │clear   ││         ┌─────┐            │
      │        ││─────────┘     └────────────│
      │        ││───┬─────┬─────┬─────┬─────┬│
      │counter ││ 0 │1    │2    │3    │0    ││
      │        ││───┴─────┴─────┴─────┴─────┴│
      │div-cloc││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │        ││───┬───────────┬───────────┬│
      │out     ││ 0 │1          │0          ││
      │        ││───┴───────────┴───────────┴│
      └────────┘└────────────────────────────┘
      e743424f9a9f6a1b3cf42c424fcbbf3e
      ┌Signals─┐┌Waves───────────────────────┐
      │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
      │        ││───┘  └──┘  └──┘  └──┘  └──┘│
      │clear   ││         ┌─────┐            │
      │        ││─────────┘     └────────────│
      │        ││───┬─────┬─────┬─────┬─────┬│
      │counter ││ 0 │1    │2    │3    │0    ││
      │        ││───┴─────┴─────┴─────┴─────┴│
      │div-cloc││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │        ││───┬───────────┬───────────┬│
      │out     ││ 0 │1          │2          ││
      │        ││───┴───────────┴───────────┴│
      └────────┘└────────────────────────────┘
      f82f9ddc31df94fe58bcc935c178e079
      ┌Signals─┐┌Waves───────────────────────┐
      │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
      │        ││───┘  └──┘  └──┘  └──┘  └──┘│
      │clear   ││         ┌─────┐            │
      │        ││─────────┘     └────────────│
      │        ││───┬─────┬─────┬─────┬─────┬│
      │counter ││ 0 │1    │2    │3    │0    ││
      │        ││───┴─────┴─────┴─────┴─────┴│
      │div-cloc││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │        ││───┬───────────┬───────────┬│
      │out     ││ 0 │1          │2          ││
      │        ││───┴───────────┴───────────┴│
      └────────┘└────────────────────────────┘
      f82f9ddc31df94fe58bcc935c178e079
      |}]
  ;;
end

module%test _ = struct
  module Inner = struct
    module I = struct
      type 'a t =
        { clock : 'a [@bits 1]
        ; sel : 'a [@bits 1]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
    end

    let create _scope ({ clock; sel } : _ I.t) =
      let open Signal in
      let r =
        reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r ->
          let add = r +:. 1 -- "inner-add" in
          mux2 sel (zero 2) add -- "inner-mux")
        -- "r-inner"
      in
      { O.out = r }
    ;;

    let hierarchical scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical ~scope ~instance:"inner" ~name:"inner_n" create i
    ;;
  end

  module Passthrough = struct
    module I = struct
      type 'a t =
        { clock : 'a [@bits 1]
        ; sel : 'a [@bits 2]
        }
      [@@deriving hardcaml]
    end

    module O = I

    let create _scope ({ clock; sel } : _ I.t) = { O.clock; sel }

    let hierarchical scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical ~scope ~instance:"pass" ~name:"pass_n" create i
    ;;
  end

  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let create scope ({ clock } : _ I.t) =
    let open Signal in
    let r =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "r-outer"
    in
    let sel = select r ~low:1 ~high:1 in
    let%tydi { clock; sel } =
      Passthrough.hierarchical scope (Passthrough.hierarchical scope { clock; sel })
    in
    let%tydi { out } = Inner.hierarchical scope { clock; sel } in
    { O.out }
  ;;

  let hierarchical scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~instance:"outer" ~name:"outer_n" create i
  ;;

  let%expect_test "Outputs of clock domains propegate correctly in a hierarchy" =
    let module Test = Make_test (I) (O) in
    let () =
      let scope =
        Hardcaml.Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
      in
      Test.show_domains (hierarchical scope);
      [%expect
        {|
        (Clocked ((clock (__4)) (edge Rising)))
        (15):           Wire[id:4 bits:2 names:__3 deps:] -> () (input)
        inner-add:      Op[id:8 bits:2 names:__7 deps:6,7] = add
        inner-add:      Wire[id:9 bits:2 names:__8 deps:8] -> 8 (output)
        out:            Wire[id:10 bits:2 names:__9 deps:3] -> 3 (output)
        out:            Wire[id:3 bits:2 names:__2 deps:2] -> 2
        outer$inner$i$clock:Wire[id:5 bits:1 names:__4 deps:] -> () (input)
        outer$inner$o$out:Wire[id:1 bits:2 names:__0 deps:6] -> 6
        outer$o$out:    Wire[id:2 bits:2 names:__1 deps:1] -> 1
        r-inner:        Reg[id:6 bits:2 names:__5 deps:4,5]

        (Clocked ((clock (__6)) (edge Rising)))
        outer$i$clock:  Wire[id:17 bits:1 names:__6 deps:] -> () (input)
        outer$inner$i$sel:Wire[id:16 bits:1 names:__5 deps:15] -> 15
        outer$inner$i$sel:Wire[id:22 bits:1 names:__11 deps:16] -> 16 (output)
        outer$pass$i$sel:Wire[id:12 bits:1 names:__1 deps:21] -> 21
        outer$pass$o$sel:Wire[id:13 bits:1 names:__2 deps:12] -> 12
        outer$pass_1$i$sel:Wire[id:14 bits:1 names:__3 deps:13] -> 13
        outer$pass_1$o$sel:Wire[id:15 bits:1 names:__4 deps:14] -> 14
        r-outer:        Reg[id:18 bits:2 names:__7 deps:11,17]

        Floating
        (15):           Wire[id:37 bits:2 names:__14 deps:23] -> 23 (output)
        clock:          Wire[id:24 bits:1 names:__1 deps:] -> () (input)
        inner-add:      Wire[id:33 bits:2 names:__10 deps:] -> () (input)
        inner-mux:      Op[id:35 bits:2 names:__12 deps:32,33,34] = mux
        outer$i$clock:  Wire[id:26 bits:1 names:__3 deps:25] -> 25
        outer$i$clock:  Wire[id:38 bits:1 names:__15 deps:26] -> 26 (output)
        outer$inner$i$clock:Wire[id:31 bits:1 names:__8 deps:30] -> 30
        outer$inner$i$clock:Wire[id:36 bits:1 names:__13 deps:31] -> 31 (output)
        outer$inner$i$sel:Wire[id:32 bits:1 names:__9 deps:] -> () (input)
        outer$pass$i$clock:Wire[id:27 bits:1 names:__4 deps:26] -> 26
        outer$pass$o$clock:Wire[id:28 bits:1 names:__5 deps:27] -> 27
        outer$pass_1$i$clock:Wire[id:29 bits:1 names:__6 deps:28] -> 28
        outer$pass_1$o$clock:Wire[id:30 bits:1 names:__7 deps:29] -> 29
        |}]
    in
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      let scope =
        Hardcaml.Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
      in
      Test.test
        ~sim_mode
        (hierarchical scope)
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬───────────┬│
        │inner-ad││ 1 │2    │3    │1          ││
        │        ││───┴─────┴─────┴───────────┴│
        │        ││───┬─────┬───────────┬─────┬│
        │inner-mu││ 1 │2    │0          │1    ││
        │        ││───┴─────┴───────────┴─────┴│
        │        ││───┬─────┬─────┬───────────┬│
        │out     ││ 0 │1    │2    │0          ││
        │        ││───┴─────┴─────┴───────────┴│
        │outer$i$││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$in││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$in││         ┌───────────┐      │
        │        ││─────────┘           └──────│
        │        ││───┬─────┬─────┬───────────┬│
        │outer$in││ 0 │1    │2    │0          ││
        │        ││───┴─────┴─────┴───────────┴│
        │        ││───┬─────┬─────┬───────────┬│
        │outer$o$││ 0 │1    │2    │0          ││
        │        ││───┴─────┴─────┴───────────┴│
        │outer$pa││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$pa││         ┌───────────┐      │
        │        ││─────────┘           └──────│
        │outer$pa││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$pa││         ┌───────────┐      │
        │        ││─────────┘           └──────│
        │outer$pa││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$pa││         ┌───────────┐      │
        │        ││─────────┘           └──────│
        │outer$pa││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$pa││         ┌───────────┐      │
        │        ││─────────┘           └──────│
        │        ││───┬─────┬─────┬───────────┬│
        │r-inner ││ 0 │1    │2    │0          ││
        │        ││───┴─────┴─────┴───────────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │r-outer ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        └────────┘└────────────────────────────┘
        6eee06f52330b5bed82970918c0161c9
        |}])
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let reg_output = wire 1 -- "reg_ouptut" in
    let reg_input = ~:reg_output -- "reg_input" in
    let r = reg (Reg_spec.create () ~clock) reg_input -- "r" in
    reg_output <-- r;
    let some_flips = ~:(~:(~:r)) -- "some_flips" in
    let out = some_flips ^: r in
    { O.out }
  ;;

  let%expect_test "We are able to handle tracing an internal cyclesim signal" =
    let module Test = Make_test (I) (O) in
    Test.show_domains circuit;
    [%expect
      {|
      (Clocked ((clock (__3)) (edge Rising)))
      (2):            Wire[id:4 bits:1 names:__3 deps:] -> () (input)
      out:            Wire[id:10 bits:1 names:__9 deps:2] -> 2 (output)
      out:            Wire[id:2 bits:1 names:__1 deps:9] -> 9
      r:              Reg[id:5 bits:1 names:__4 deps:3,4]
      reg_input:      Op[id:3 bits:1 names:__2 deps:1] = not
      reg_ouptut:     Wire[id:1 bits:1 names:__0 deps:5] -> 5
      some_flips:     Op[id:8 bits:1 names:__7 deps:7] = not

      Floating
      (2):            Wire[id:13 bits:1 names:__2 deps:12] -> 12 (output)
      clock:          Wire[id:11 bits:1 names:__0 deps:] -> () (input)
      |}];
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      Test.test ~sim_mode circuit ~handle_input:(fun { clock } ~create_clock ->
        { clock = create_clock ~time:3 clock.signal }));
    [%expect
      {|
      ┌Signals─┐┌Waves───────────────────────┐
      │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
      │        ││───┘  └──┘  └──┘  └──┘  └──┘│
      │out     ││────────────────────────────│
      │        ││                            │
      │r       ││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │reg_inpu││───┐     ┌─────┐     ┌─────┐│
      │        ││   └─────┘     └─────┘     └│
      │reg_oupt││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │some_fli││───┐     ┌─────┐     ┌─────┐│
      │        ││   └─────┘     └─────┘     └│
      └────────┘└────────────────────────────┘
      2f878df26c3076c83e534921d296834d
      ┌Signals─┐┌Waves───────────────────────┐
      │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
      │        ││───┘  └──┘  └──┘  └──┘  └──┘│
      │out     ││────────────────────────────│
      │        ││                            │
      │r       ││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │reg_inpu││───┐     ┌─────┐     ┌─────┐│
      │        ││   └─────┘     └─────┘     └│
      │reg_oupt││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │some_fli││───┐     ┌─────┐     ┌─────┐│
      │        ││   └─────┘     └─────┘     └│
      └────────┘└────────────────────────────┘
      e819e2d82fccc39aab9c11ada9ec65d4
      ┌Signals─┐┌Waves───────────────────────┐
      │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
      │        ││───┘  └──┘  └──┘  └──┘  └──┘│
      │out     ││────────────────────────────│
      │        ││                            │
      │r       ││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │reg_inpu││───┐     ┌─────┐     ┌─────┐│
      │        ││   └─────┘     └─────┘     └│
      │reg_oupt││   ┌─────┐     ┌─────┐     ┌│
      │        ││───┘     └─────┘     └─────┘│
      │some_fli││───┐     ┌─────┐     ┌─────┐│
      │        ││   └─────┘     └─────┘     └│
      └────────┘└────────────────────────────┘
      e819e2d82fccc39aab9c11ada9ec65d4
      |}]
  ;;
end

module%test _ = struct
  module Inner = struct
    module I = struct
      type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
    end

    let create _scope ({ clock } : _ I.t) =
      let open Signal in
      let zero = zero 1 in
      let counter =
        reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "counter"
      in
      let memory =
        multiport_memory
          1
          ~write_ports:
            [| { write_clock = clock
               ; write_address = zero
               ; write_enable = vdd
               ; write_data = counter
               }
            |]
          ~read_addresses:[| zero |]
      in
      let read_out = memory.(0) -- "read-out" in
      (match read_out with
       | Mem_read_port { memory; _ } ->
         let (_ : t) = memory -- "mem" in
         ()
       | _ -> raise_s [%message "unexpected"]);
      { O.out = read_out }
    ;;

    let hierarchical scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical ~scope ~instance:"inner" ~name:"inner_n" create i
    ;;
  end

  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let create scope ({ clock } : _ I.t) =
    let open Signal in
    let r =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "r-outer"
    in
    let%tydi { out } = Inner.hierarchical scope { clock } in
    let out = r +: out in
    { O.out }
  ;;

  let hierarchical scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~instance:"outer" ~name:"outer_n" create i
  ;;

  let%expect_test "memories in heirarchy" =
    let module Test = Make_test (I) (O) in
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      let scope =
        Hardcaml.Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
      in
      Test.test
        ~sim_mode
        (hierarchical scope)
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │counter ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││────────────────────────────│
        │mem     ││ 0                          │
        │        ││────────────────────────────│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │3    │1    │3    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │outer$i$││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$in││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││─────────┬─────┬─────┬─────┬│
        │outer$in││ 0       │1    │2    │3    ││
        │        ││─────────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │outer$o$││ 0 │1    │3    │1    │3    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │r-outer ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││─────────┬─────┬─────┬─────┬│
        │read-out││ 0       │1    │2    │3    ││
        │        ││─────────┴─────┴─────┴─────┴│
        │vdd     ││────────────────────────────│
        │        ││                            │
        └────────┘└────────────────────────────┘
        3e067e19f2af7a6ecdf438f7e5b80130
        |}])
  ;;
end

module%test _ = struct
  module Inner = struct
    module I = struct
      type 'a t =
        { clock : 'a [@bits 1]
        ; reset_signal : 'a [@bits 1]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
    end

    let create _scope ({ clock; reset_signal } : _ I.t) =
      let open Signal in
      let r_reset =
        reg_fb (Reg_spec.create () ~clock ~reset:reset_signal) ~width:2 ~f:(fun r ->
          r +:. 1)
        -- "r-reset"
      in
      { O.out = r_reset }
    ;;

    let hierarchical scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical ~scope ~instance:"inner" ~name:"inner_n" create i
    ;;
  end

  module I = struct
    type 'a t = { clock : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 2] } [@@deriving hardcaml]
  end

  let create scope ({ clock } : _ I.t) =
    let open Signal in
    let r_signal =
      reg_fb (Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1) -- "r-signal"
    in
    let reset_signal = (r_signal ==:. 2) -- "reset-signal" in
    let%tydi { out } = Inner.hierarchical scope { clock; reset_signal } in
    { O.out }
  ;;

  let hierarchical scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~instance:"outer" ~name:"outer_n" create i
  ;;

  let%expect_test "resets in heirarchy" =
    let module Test = Make_test (I) (O) in
    List.iter sim_modes_to_test ~f:(fun sim_mode ->
      let scope =
        Hardcaml.Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
      in
      Test.test
        ~sim_mode
        (hierarchical scope)
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │0    │1    │2    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │outer$i$││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$in││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │outer$in││         ┌─────┐            │
        │        ││─────────┘     └────────────│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │outer$in││ 0 │1    │0    │1    │2    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │outer$o$││ 0 │1    │0    │1    │2    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │r-reset ││ 0 │1    │0    │1    │2    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │r-signal││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │reset-si││         ┌─────┐            │
        │        ││─────────┘     └────────────│
        └────────┘└────────────────────────────┘
        2ff342135c5b4dcb8adcba2f7c0dc9eb
        |}])
  ;;
end
