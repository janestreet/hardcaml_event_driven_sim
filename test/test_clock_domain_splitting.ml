open! Core
open Hardcaml
module Clock_domain_splitting = Hardcaml_event_driven_sim.Clock_domain_splitting

module Make_test (Input : Interface.S) (Output : Interface.S) : sig
  val test
    :  ?show:[ `All | `Named_and_interfaces ]
    -> ?show_kind:bool
    -> (Signal.t Input.t -> Signal.t Output.t)
    -> unit
end = struct
  module With_interface = Circuit.With_interface (Input) (Output)

  let should_show show ~has_name ~is_input ~is_output =
    match show with
    | `All -> true
    | `Named_and_interfaces -> has_name || is_input || is_output
  ;;

  let test ?(show = `Named_and_interfaces) ?(show_kind = true) circuit =
    let circuit = With_interface.create_exn circuit ~name:"test" in
    let make_signal_set signals =
      signals |> List.map ~f:Signal.uid |> Hash_set.of_list (module Signal.Uid)
    in
    let inputs = Circuit.inputs circuit |> make_signal_set in
    let outputs = Circuit.outputs circuit |> make_signal_set in
    let clock_domains =
      Hardcaml_event_driven_sim.Clock_domain_splitting.group_by_clock_domain circuit
    in
    clock_domains
    |> Map.to_alist
    |> List.iter ~f:(fun (clock_domain, signal_graph) ->
      print_s [%sexp (clock_domain : Clock_domain_splitting.Clock_domain.t)];
      signal_graph
      |> Signal_graph.fold ~init:[] ~f:(fun acc signal -> signal :: acc)
      |> List.filter_map ~f:(fun signal ->
        let uid = Signal.uid signal in
        let is_input = Hash_set.mem inputs uid in
        let is_output = Hash_set.mem outputs uid in
        let has_name = Signal.Type.has_name signal in
        if should_show show ~has_name ~is_input ~is_output
        then (
          let name =
            match Signal.names signal |> List.hd with
            | None ->
              let uid = Signal.uid signal in
              [%string "(%{uid#Signal.Uid})"]
            | Some name -> name
          in
          let kind = if show_kind then Signal.Type.to_string signal else "" in
          let is_input = if is_input then " (input)" else "" in
          let is_output = if is_output then " (output)" else "" in
          let details = [%string "%{kind}%{is_input}%{is_output}"] in
          let description = sprintf "%-16s%s" (name ^ ":") details in
          Some description)
        else None)
      |> List.sort ~compare:[%compare: string]
      |> List.iter ~f:(fun signal_description -> print_endline signal_description);
      print_endline "")
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t = { unused : 'a [@bits 1] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ unused = _ } : _ I.t) =
    let open Signal in
    let ones = ones 1 -- "ones" in
    let wire1 = wireof ones -- "wire1" in
    let wire2 = wireof wire1 -- "wire2" in
    { O.out = wire2 }
  ;;

  let%expect_test "Test outputs that are just driven by constants and unused inputs" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      Floating
      ones:           Const[id:4 bits:1 names:ones deps:] = 1
      out:            Wire[id:3 bits:1 names:out deps:2] -> 2 (output)
      wire1:          Wire[id:1 bits:1 names:wire1 deps:4] -> 4
      wire2:          Wire[id:2 bits:1 names:wire2 deps:1] -> 1
      |}]
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
    let reg_1 = reg (Hardcaml.Reg_spec.create () ~clock:clock_1) (zero 1) -- "reg_1" in
    let reg_2 = reg (Hardcaml.Reg_spec.create () ~clock:clock_2) (zero 1) -- "reg_2" in
    let xor = (reg_1 ^: reg_2) -- "xor" in
    { O.out = xor }
  ;;

  let%expect_test "The register outputs are in their own clock domains, but the xor-ed \
                   combination of the outputs is floating"
    =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 2) (edge Rising)))
      reg_2:          Reg[id:9 bits:1 names:reg_2 deps:8,2]

      (Clocked ((clock 4) (edge Rising)))
      reg_1:          Reg[id:7 bits:1 names:reg_1 deps:6,4]

      Floating
      clock_1:        Wire[id:3 bits:1 names:clock_1 deps:] -> () (input)
      clock_2:        Wire[id:1 bits:1 names:clock_2 deps:] -> () (input)
      out:            Wire[id:5 bits:1 names:out deps:10] -> 10 (output)
      xor:            Op[id:10 bits:1 names:xor deps:7,9] = xor
      |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t =
      { clock_1 : 'a [@bits 1]
      ; clock_2 : 'a [@bits 1]
      ; inpt : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ clock_1; clock_2; inpt } : _ I.t) =
    let open Signal in
    let reg_1_output = wire 1 -- "reg1_output" in
    let reg_1_input = inpt +: reg_1_output -- "reg1_input" in
    let reg_1 = reg (Hardcaml.Reg_spec.create () ~clock:clock_1) reg_1_input -- "reg_1" in
    reg_1_output <== reg_1;
    let reg_2_output = wire 1 -- "reg2_output" in
    let reg_2_input = reg_1_output +: (reg_2_output ^: zero 1) in
    let reg_2 = reg (Hardcaml.Reg_spec.create () ~clock:clock_2) reg_2_input -- "reg_2" in
    reg_2_output <== reg_2;
    { O.out = reg_2 }
  ;;

  let%expect_test "Inputs to registers to not affect the register's clock domain" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 2) (edge Rising)))
      out:            Wire[id:9 bits:1 names:out deps:13] -> 13 (output)
      reg2_output:    Wire[id:3 bits:1 names:reg2_output deps:13] -> 13
      reg_2:          Reg[id:13 bits:1 names:reg_2 deps:12,2]

      (Clocked ((clock 5) (edge Rising)))
      reg1_output:    Wire[id:8 bits:1 names:reg1_output deps:15] -> 15
      reg_1:          Reg[id:15 bits:1 names:reg_1 deps:14,5]

      Floating
      clock_1:        Wire[id:4 bits:1 names:clock_1 deps:] -> () (input)
      clock_2:        Wire[id:1 bits:1 names:clock_2 deps:] -> () (input)
      inpt:           Wire[id:6 bits:1 names:inpt deps:] -> () (input)
      reg1_input:     Op[id:14 bits:1 names:reg1_input deps:7,8] = add
      |}]
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
    let zero = zero 1 -- "zero" in
    let write_ports =
      [| { Write_port.write_clock = clock
         ; write_address = zero
         ; write_enable = zero
         ; write_data = zero
         }
      |]
    in
    let read_addresses = [| zero |] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ "mem" ]
     | _ -> ());
    let out = mem.(0) -- "mem_read_out" in
    { O.out }
  ;;

  let%expect_test "If only one clock drives memory, it's in its own clock domain" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 2) (edge Rising)))
      mem:            Multiport_mem[id:5 bits:1 names:mem deps:2,4,4,4]
      mem_read_out:   Mem_read_port[id:6 bits:1 names:mem_read_out deps:4,5]
      out:            Wire[id:3 bits:1 names:out deps:6] -> 6 (output)
      zero:           Const[id:4 bits:1 names:zero deps:] = 0

      Floating
      clock:          Wire[id:1 bits:1 names:clock deps:] -> () (input)
      |}]
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
    let write_ports =
      Array.of_list
        [ { Write_port.write_clock = clock_1
          ; write_address = zero
          ; write_enable = zero
          ; write_data = zero
          }
        ; { Write_port.write_clock = clock_2
          ; write_address = zero
          ; write_enable = zero
          ; write_data = zero
          }
        ]
    in
    let read_addresses = Array.of_list [ zero ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ "mem" ]
     | _ -> ());
    let out = mem.(0) -- "mem_read_out" in
    { O.out }
  ;;

  let%expect_test "If multiple clocks drive memory, the memory is floating" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      Floating
      clock_1:        Wire[id:3 bits:1 names:clock_1 deps:] -> () (input)
      clock_2:        Wire[id:1 bits:1 names:clock_2 deps:] -> () (input)
      mem:            Multiport_mem[id:7 bits:1 names:mem deps:4,6,6,6,2,6,6,6]
      mem_read_out:   Mem_read_port[id:8 bits:1 names:mem_read_out deps:6,7]
      out:            Wire[id:5 bits:1 names:out deps:8] -> 8 (output)
      zero:           Const[id:6 bits:1 names:zero deps:] = 0
      |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t =
      { read_clock : 'a [@bits 1]
      ; write_clock : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ read_clock; write_clock } : _ I.t) =
    let open Signal in
    let zero = zero 1 -- "zero" in
    let write_ports =
      Array.of_list
        [ { Write_port.write_clock
          ; write_address = zero
          ; write_enable = zero
          ; write_data = zero
          }
        ]
    in
    let read_address =
      reg (Hardcaml.Reg_spec.create () ~clock:read_clock) zero -- "read_addr_reg"
    in
    let read_addresses = Array.of_list [ read_address ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ "mem" ]
     | _ -> ());
    let read_output =
      reg (Hardcaml.Reg_spec.create () ~clock:read_clock) (mem.(0) -- "mem_read_out")
      -- "read_output_reg"
    in
    { O.out = read_output }
  ;;

  let%expect_test "If a different clock drives the read address from the write address, \
                   then the read output and memory is floating"
    =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 4) (edge Rising)))
      out:            Wire[id:5 bits:1 names:out deps:10] -> 10 (output)
      read_addr_reg:  Reg[id:7 bits:1 names:read_addr_reg deps:6,4]
      read_output_reg:Reg[id:10 bits:1 names:read_output_reg deps:9,4]
      zero:           Const[id:6 bits:1 names:zero deps:] = 0

      Floating
      mem:            Multiport_mem[id:8 bits:1 names:mem deps:2,6,6,6]
      mem_read_out:   Mem_read_port[id:9 bits:1 names:mem_read_out deps:7,8]
      read_clock:     Wire[id:3 bits:1 names:read_clock deps:] -> () (input)
      write_clock:    Wire[id:1 bits:1 names:write_clock deps:] -> () (input)
      zero:           Const[id:6 bits:1 names:zero deps:] = 0
      |}]
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
    let zero = zero 1 -- "zero" in
    let write_ports =
      Array.of_list
        [ { Write_port.write_clock = clock
          ; write_address = zero
          ; write_enable = zero
          ; write_data = zero
          }
        ]
    in
    let read_address = reg (Hardcaml.Reg_spec.create () ~clock) zero -- "read_addr_reg" in
    let read_addresses = Array.of_list [ read_address ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ "mem" ]
     | _ -> ());
    let out = mem.(0) in
    { O.out }
  ;;

  let%expect_test "If the same clock drives the read address and the write address, then \
                   the read output and memory are in the same clock domain"
    =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 2) (edge Rising)))
      mem:            Multiport_mem[id:6 bits:1 names:mem deps:2,4,4,4]
      out:            Wire[id:3 bits:1 names:out deps:7] -> 7 (output)
      read_addr_reg:  Reg[id:5 bits:1 names:read_addr_reg deps:4,2]
      zero:           Const[id:4 bits:1 names:zero deps:] = 0

      Floating
      clock:          Wire[id:1 bits:1 names:clock deps:] -> () (input)
      |}]
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
    let zero = zero 1 -- "zero" in
    let inverted_clock = ~:clock -- "inverted_clock" in
    let reg_1 = reg (Hardcaml.Reg_spec.create ~clock ()) zero -- "reg_1" in
    let reg_2 =
      reg (Hardcaml.Reg_spec.create ~clock:inverted_clock ()) reg_1 -- "reg_2"
    in
    { O.out = reg_2 }
  ;;

  let%expect_test "different clock signals produce different clock domains" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 2) (edge Rising)))
      reg_1:          Reg[id:5 bits:1 names:reg_1 deps:4,2]
      zero:           Const[id:4 bits:1 names:zero deps:] = 0

      (Clocked ((clock (inverted_clock)) (edge Rising)))
      out:            Wire[id:3 bits:1 names:out deps:7] -> 7 (output)
      reg_2:          Reg[id:7 bits:1 names:reg_2 deps:5,6]

      Floating
      clock:          Wire[id:1 bits:1 names:clock deps:] -> () (input)
      inverted_clock: Op[id:6 bits:1 names:inverted_clock deps:2] = not
      |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t =
      { clock1 : 'a [@bits 1]
      ; clock2 : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ clock1; clock2 } : _ I.t) =
    let open Signal in
    let zero = zero 1 -- "zero" in
    let write_ports =
      Array.of_list
        [ { Write_port.write_clock = clock1
          ; write_address = zero
          ; write_enable = zero
          ; write_data = zero
          }
        ]
    in
    let read_address =
      reg (Hardcaml.Reg_spec.create () ~clock:clock2) zero -- "read1_addr_reg"
    in
    let read_port_1_output = wire 1 -- "read2_addr" in
    let read_addresses = Array.of_list [ read_address; read_port_1_output ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ "mem" ]
     | _ -> ());
    read_port_1_output <== mem.(0);
    let out = mem.(1) in
    { O.out }
  ;;

  let%expect_test "The clock domain of a read address propegates through the entire \
                   memory unit"
    =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 4) (edge Rising)))
      read1_addr_reg: Reg[id:8 bits:1 names:read1_addr_reg deps:7,4]
      zero:           Const[id:7 bits:1 names:zero deps:] = 0

      Floating
      clock1:         Wire[id:1 bits:1 names:clock1 deps:] -> () (input)
      clock2:         Wire[id:3 bits:1 names:clock2 deps:] -> () (input)
      mem:            Multiport_mem[id:9 bits:1 names:mem deps:2,7,7,7]
      out:            Wire[id:6 bits:1 names:out deps:11] -> 11 (output)
      read2_addr:     Wire[id:5 bits:1 names:read2_addr deps:10] -> 10
      zero:           Const[id:7 bits:1 names:zero deps:] = 0
      |}]
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
    let zero = zero 1 -- "zero" in
    let write_ports = [||] in
    let read_address =
      reg (Hardcaml.Reg_spec.create () ~clock) zero -- "read1_addr_reg"
    in
    let read_addresses = [| read_address |] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ "mem" ]
     | _ -> ());
    let out = mem.(0) in
    { O.out }
  ;;

  let%expect_test "If this no longer raises, we need to change the clock domain \
                   splitting code"
    =
    let module Test = Make_test (I) (O) in
    Expect_test_helpers_base.require_does_raise (fun () -> Test.test circuit);
    [%expect {| "[Signal.multiport_memory] requires at least one write port" |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t =
      { clock : 'a [@bits 1]
      ; inpt : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ clock; inpt } : _ I.t) =
    let open Signal in
    let open Unoptimized in
    let zero = zero 1 -- "zero" in
    let one = one 1 -- "one" in
    let xor = (zero ^: one) -- "xor" in
    let register_output = wire 1 -- "wire" in
    let value = mux2 register_output xor inpt -- "value" in
    let register = reg (Hardcaml.Reg_spec.create () ~clock) value -- "read1_addr_reg" in
    register_output <== register;
    { O.out = register }
  ;;

  let%expect_test "Constant chains are lifted to their appropriate domain" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock 2) (edge Rising)))
      out:            Wire[id:6 bits:1 names:out deps:11] -> 11 (output)
      read1_addr_reg: Reg[id:11 bits:1 names:read1_addr_reg deps:10,2]
      wire:           Wire[id:5 bits:1 names:wire deps:11] -> 11

      Floating
      clock:          Wire[id:1 bits:1 names:clock deps:] -> () (input)
      inpt:           Wire[id:3 bits:1 names:inpt deps:] -> () (input)
      one:            Const[id:8 bits:1 names:one,vdd deps:] = 1
      value:          Op[id:10 bits:1 names:value deps:5,4,9] = mux
      xor:            Op[id:9 bits:1 names:xor deps:7,8] = xor
      zero:           Const[id:7 bits:1 names:zero deps:] = 0
      |}]
  ;;
end

module%test _ = struct
  module Fifo = Async_fifo.Make (struct
      (* 8 deep, 4 wide *)
      let width = 4
      let log2_depth = 3
    end)

  let fifo () =
    Fifo.create
      ~scope:(Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
  ;;

  let%expect_test "async fifo has 2 clock domains. The multiport memory is the only \
                   non-input part of the circuit in the floating clock domain"
    =
    let module Test = Make_test (Fifo.I) (Fifo.O) in
    Test.test ~show_kind:false (fifo ());
    [%expect
      {|
      (Clocked ((clock 14) (edge Rising)))
      almost_empty:    (output)
      data_out:
      data_out:        (output)
      raddr_rd:
      valid:           (output)
      waddr_rd:
      waddr_rd_ff_0:

      (Clocked ((clock 25) (edge Rising)))
      full:            (output)
      raddr_wd:
      raddr_wd_ff_0:
      waddr_wd:

      Floating
      clock_read:      (input)
      clock_write:     (input)
      data_in:         (input)
      gnd:
      ram:
      read_enable:     (input)
      reset_read:      (input)
      reset_write:     (input)
      write_enable:    (input)
      |}]
  ;;
end
