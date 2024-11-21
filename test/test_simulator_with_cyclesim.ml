open! Core
open Hardcaml

module Make_test (Input : Interface.S) (Output : Interface.S) : sig
  val show_domains : (Signal.t Input.t -> Signal.t Output.t) -> unit

  val test
    :  (Signal.t Input.t -> Signal.t Output.t)
    -> height:int
    -> handle_input:
         (Bits.t Hardcaml_event_driven_sim.Port.t Input.t
          -> create_clock:
               (?initial_delay:int
                -> time:int
                -> Bits.t Event_driven_sim.Simulator.Signal.t
                -> Event_driven_sim.Simulator.Process.t)
          -> Event_driven_sim.Simulator.Process.t Input.t)
    -> use_cyclesim:bool
    -> unit
end = struct
  open Hardcaml_event_driven_sim
  module Logic = Two_state_logic
  module Sim_without_interface = Make (Logic)
  module Sim_interface = Sim_without_interface.With_interface (Input) (Output)

  module Clock_domain_splitting_test =
    Test_clock_domain_splitting.Make_test (Input) (Output)

  let show_domains circuit = Clock_domain_splitting_test.test circuit

  let test circuit ~height ~handle_input ~use_cyclesim =
    let%tydi waves, { simulator; _ } =
      Sim_interface.with_waveterm
        ~config:{ Sim_without_interface.Config.default with use_cyclesim }
        circuit
        (fun input_ports _output_ports ->
           handle_input input_ports ~create_clock:Sim_interface.create_clock
           |> Input.to_list)
    in
    for _ = 1 to 20 do
      Event_driven_sim.Simulator.step simulator
    done;
    Hardcaml_event_driven_sim.Waveterm.Waveform.expect
      waves
      ~wave_width:(-1)
      ~display_width:40
      ~display_height:height
  ;;
end

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
    let reg_1 = reg (Hardcaml.Reg_spec.create () ~clock:clock_1) zero -- "reg_1" in
    let reg_2 =
      reg_fb (Hardcaml.Reg_spec.create () ~clock:clock_2) ~width:1 ~f:(fun r -> r +:. 1)
      -- "reg_2"
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
    List.iter Bool.all ~f:(fun use_cyclesim ->
      Test.test
        ~use_cyclesim
        circuit
        ~height:16
        ~handle_input:(fun { clock_1; clock_2 } ~create_clock ->
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
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        └────────┘└────────────────────────────┘
        0c8cf49fe2c2d47d5d721bc7f7613822
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
    memory_input <== read_out +:. 1;
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
    List.iter Bool.all ~f:(fun use_cyclesim ->
      Test.test
        ~use_cyclesim
        circuit
        ~height:12
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │2    │3    │0    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        └────────┘└────────────────────────────┘
        b03c80f54215de2abcb204e889229946
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
      reg_fb (Hardcaml.Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1)
      -- "r-signal"
    in
    let reset_signal = r_signal ==:. 2 in
    let r_regular =
      reg_fb (Hardcaml.Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1)
      -- "r-regular"
    in
    let r_reset =
      reg_fb
        (Hardcaml.Reg_spec.create () ~clock ~reset:reset_signal)
        ~width:2
        ~f:(fun r -> r +:. 1)
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
    List.iter Bool.all ~f:(fun use_cyclesim ->
      Test.test
        ~use_cyclesim
        circuit
        ~height:10
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
    type 'a t = { out : 'a [@bits 3] } [@@deriving hardcaml]
  end

  let circuit ({ clock } : _ I.t) =
    let open Signal in
    let counter =
      reg_fb (Hardcaml.Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1)
      -- "counter"
    in
    let clear_reg =
      reg (Hardcaml.Reg_spec.create () ~clock) (counter ==:. 2) -- "clear"
    in
    let out_reg =
      let clock = wireof (wireof (wireof (wireof clock))) in
      reg_fb (Hardcaml.Reg_spec.create () ~clock ~clear:clear_reg) ~width:3 ~f:(fun r ->
        r +:. 1)
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
    List.iter Bool.all ~f:(fun use_cyclesim ->
      Test.test
        ~use_cyclesim
        circuit
        ~height:12
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬─────┬─────┬│
        │out     ││ 0 │1    │2    │0    │1    ││
        │        ││───┴─────┴─────┴─────┴─────┴│
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        │        ││                            │
        └────────┘└────────────────────────────┘
        b33caaae3f21f0aeb5a5a99b473f14aa
        |}])
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
        reg_fb (Hardcaml.Reg_spec.create () ~clock) ~width:2 ~f:(fun r ->
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
      reg_fb (Hardcaml.Reg_spec.create () ~clock) ~width:2 ~f:(fun r -> r +:. 1)
      -- "r-outer"
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
    List.iter Bool.all ~f:(fun use_cyclesim ->
      let scope =
        Hardcaml.Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
      in
      Test.test
        ~use_cyclesim
        (hierarchical scope)
        ~height:10
        ~handle_input:(fun { clock } ~create_clock ->
          { clock = create_clock ~time:3 clock.signal });
      [%expect
        {|
        ┌Signals─┐┌Waves───────────────────────┐
        │clock   ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌│
        │        ││───┘  └──┘  └──┘  └──┘  └──┘│
        │        ││───┬─────┬─────┬───────────┬│
        │out     ││ 0 │1    │2    │0          ││
        │        ││───┴─────┴─────┴───────────┴│
        │        ││                            │
        │        ││                            │
        │        ││                            │
        └────────┘└────────────────────────────┘
        3e2b1ef41d542b7be3a53b122baaf9ed
        |}])
  ;;
end
