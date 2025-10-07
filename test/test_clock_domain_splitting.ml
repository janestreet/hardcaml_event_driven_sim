open! Core
open Hardcaml
module Clock_domain_splitting = Hardcaml_event_driven_sim.Private.Clock_domain_splitting

module Make_test (Input : Interface.S) (Output : Interface.S) : sig
  val test
    :  ?show:[ `All | `Named_and_interfaces | `Named ]
    -> ?show_kind:bool
    -> (Signal.t Input.t -> Signal.t Output.t)
    -> unit
end = struct
  module With_interface = Circuit.With_interface (Input) (Output)

  let should_show show ~has_name ~is_input ~is_output =
    match show with
    | `All -> true
    | `Named_and_interfaces -> has_name || is_input || is_output
    | `Named -> has_name
  ;;

  let make_signal_set signals =
    signals |> List.map ~f:Signal.uid |> Hash_set.of_list (module Signal.Type.Uid)
  ;;

  let require_single_uid = function
    | [ uid ] -> uid
    | result ->
      raise_s [%message "Not found or multiple found" (result : Signal.Type.Uid.t list)]
  ;;

  let check_invariants
    ~original_circuit
    ~(clock_domains :
        Clock_domain_splitting.Copied_circuit.t Clock_domain_splitting.Clock_domain.Map.t)
    =
    let clock_domain_inputs = Hash_set.create (module Signal.Type.Uid) in
    let clock_domain_outputs = Hash_set.create (module Signal.Type.Uid) in
    (* Construct sets of all signals in the original circuit that are either inputs to a
       clock domain or outputs of a clock domain *)
    clock_domains
    |> Map.data
    |> List.iter ~f:(fun { circuit; new_signals_by_original_uids } ->
      let inputs = Circuit.inputs circuit |> make_signal_set in
      let outputs = Circuit.outputs circuit |> make_signal_set in
      let new_signals_by_original_uids = new_signals_by_original_uids |> Map.to_alist in
      Hash_set.iter inputs ~f:(fun new_uid ->
        let old_input_signal_uid =
          List.filter_map new_signals_by_original_uids ~f:(fun (old_signal, new_signal) ->
            match new_signal with
            | Internal new_signal | Output { output_wire = _; output_driver = new_signal }
              ->
              Option.some_if ([%equal: Signal.Type.Uid.t] new_signal new_uid) old_signal)
          |> require_single_uid
        in
        Hash_set.add clock_domain_inputs old_input_signal_uid);
      Hash_set.iter outputs ~f:(fun new_uid ->
        let old_output_signal_uid =
          List.filter_map new_signals_by_original_uids ~f:(fun (old_signal, new_signal) ->
            match new_signal with
            | Internal new_signal | Output { output_wire = new_signal; output_driver = _ }
              ->
              Option.some_if ([%equal: Signal.Type.Uid.t] new_signal new_uid) old_signal)
          |> require_single_uid
        in
        Hash_set.add clock_domain_outputs old_output_signal_uid));
    let circuit_inputs = Circuit.inputs original_circuit |> make_signal_set in
    let circuit_outputs = Circuit.outputs original_circuit |> make_signal_set in
    (* Check some invariants about the boundaries of clock domains:

       - All clock domain inputs are either circuit inputs or outputs of
         another clock domain
       - All clock domain outputs are either circuit outputs or inputs to
         another clock domain
       - All original circuit inputs and outputs are accounted for across
         all clock domains
    *)
    Hash_set.iter clock_domain_inputs ~f:(fun input_uid ->
      let is_clock_domain_output = Hash_set.mem clock_domain_outputs input_uid in
      let is_circuit_input = Hash_set.mem circuit_inputs input_uid in
      if not (is_circuit_input || is_clock_domain_output)
      then
        raise_s
          [%message
            "Clock domain input expected to be either circuit input or clock domain \
             output"];
      (* All circuit inputs should appear in clock domains *)
      if is_circuit_input then Hash_set.remove circuit_inputs input_uid);
    Hash_set.iter clock_domain_outputs ~f:(fun output_uid ->
      let is_clock_domain_input = Hash_set.mem clock_domain_inputs output_uid in
      let is_circuit_output = Hash_set.mem circuit_outputs output_uid in
      if not (is_circuit_output || is_clock_domain_input)
      then
        raise_s
          [%message
            "Clock domain output expected to be either circuit input or clock domain \
             output"];
      (* All circuit outputs should appear in clock domains *)
      if is_circuit_output then Hash_set.remove circuit_outputs output_uid);
    if not (Hash_set.is_empty circuit_inputs)
    then
      raise_s
        [%message
          "Some circuit inputs not in a clock domain"
            (circuit_inputs : Signal.Type.Uid.t Hash_set.t)];
    if not (Hash_set.is_empty circuit_outputs)
    then
      raise_s
        [%message
          "Some circuit outputs not in a clock domain"
            (circuit_outputs : Signal.Type.Uid.t Hash_set.t)]
  ;;

  let circuit_signals_by_uid circuit =
    let signals_by_uid = Hashtbl.create (module Signal.Type.Uid) in
    circuit
    |> Circuit.signal_graph
    |> Signal_graph.iter ~f:(fun signal ->
      Hashtbl.set signals_by_uid ~key:(Signal.uid signal) ~data:signal);
    Staged.stage (fun uid -> Hashtbl.find_exn signals_by_uid uid)
  ;;

  let test ?(show = `Named_and_interfaces) ?(show_kind = true) circuit =
    let circuit = With_interface.create_exn circuit ~name:"test" in
    let get_old_signal_by_uid = circuit_signals_by_uid circuit |> Staged.unstage in
    let clock_domains =
      Hardcaml_event_driven_sim.Private.Clock_domain_splitting.group_by_clock_domain
        (Circuit.signal_graph circuit)
        ~extra_outputs:[]
    in
    clock_domains
    |> Map.to_alist
    |> List.iter ~f:(fun (clock_domain, { circuit; new_signals_by_original_uids }) ->
      print_s [%sexp (clock_domain : Clock_domain_splitting.Clock_domain.t)];
      let inputs = Circuit.inputs circuit |> make_signal_set in
      let outputs = Circuit.outputs circuit |> make_signal_set in
      let new_to_old_mapping =
        new_signals_by_original_uids
        |> Map.to_alist
        |> List.concat_map ~f:(fun (old_signal, new_signal) ->
          match new_signal with
          | Internal new_signal -> [ new_signal, old_signal ]
          | Output { output_wire; output_driver } ->
            [ output_wire, old_signal; output_driver, old_signal ])
        |> Hashtbl.of_alist_exn (module Signal.Type.Uid)
      in
      circuit
      |> Circuit.signal_graph
      |> Signal_graph.fold ~init:[] ~f:(fun acc signal -> signal :: acc)
      |> List.filter_map ~f:(fun new_signal ->
        let uid = Signal.uid new_signal in
        let is_input = Hash_set.mem inputs uid in
        let is_output = Hash_set.mem outputs uid in
        let old_signal =
          Hashtbl.find_exn new_to_old_mapping uid |> get_old_signal_by_uid
        in
        let has_name = Signal.Type.has_name old_signal in
        if should_show show ~has_name ~is_input ~is_output
        then (
          let name =
            match Signal.names old_signal |> List.hd with
            | None ->
              let old_uid = Signal.uid old_signal in
              [%string "(%{old_uid#Signal.Type.Uid})"]
            | Some name -> name
          in
          let kind = if show_kind then Signal.Type.to_string new_signal else "" in
          let is_input = if is_input then " (input)" else "" in
          let is_output = if is_output then " (output)" else "" in
          let details = [%string "%{kind}%{is_input}%{is_output}"] in
          let description = sprintf "%-16s%s" (name ^ ":") details in
          Some description)
        else None)
      |> List.sort ~compare:[%compare: string]
      |> List.iter ~f:(fun name -> print_endline name);
      print_endline "");
    check_invariants ~original_circuit:circuit ~clock_domains
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
      ones:           Const[id:4 bits:1 names:__3 deps:] = 1
      out:            Wire[id:3 bits:1 names:__2 deps:2] -> 2
      out:            Wire[id:5 bits:1 names:__4 deps:3] -> 3 (output)
      wire1:          Wire[id:1 bits:1 names:__0 deps:4] -> 4
      wire2:          Wire[id:2 bits:1 names:__1 deps:1] -> 1
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
    let reg_1 = reg (Reg_spec.create () ~clock:clock_1) (zero 1) -- "reg_1" in
    let reg_2 = reg (Reg_spec.create () ~clock:clock_2) (zero 1) -- "reg_2" in
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
      (Clocked ((clock (__1)) (edge Rising)))
      (2):            Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      reg_2:          Reg[id:3 bits:1 names:__2 deps:1,2]
      reg_2:          Wire[id:4 bits:1 names:__3 deps:3] -> 3 (output)

      (Clocked ((clock (__1)) (edge Rising)))
      (4):            Wire[id:6 bits:1 names:__1 deps:] -> () (input)
      reg_1:          Reg[id:7 bits:1 names:__2 deps:5,6]
      reg_1:          Wire[id:8 bits:1 names:__3 deps:7] -> 7 (output)

      Floating
      (2):            Wire[id:18 bits:1 names:__9 deps:12] -> 12 (output)
      (4):            Wire[id:19 bits:1 names:__10 deps:10] -> 10 (output)
      clock_1:        Wire[id:9 bits:1 names:__0 deps:] -> () (input)
      clock_2:        Wire[id:11 bits:1 names:__2 deps:] -> () (input)
      out:            Wire[id:13 bits:1 names:__4 deps:16] -> 16
      out:            Wire[id:17 bits:1 names:__8 deps:13] -> 13 (output)
      reg_1:          Wire[id:14 bits:1 names:__5 deps:] -> () (input)
      reg_2:          Wire[id:15 bits:1 names:__6 deps:] -> () (input)
      xor:            Op[id:16 bits:1 names:__7 deps:14,15] = xor
      |}]
  ;;

  let%expect_test "show clock domain stats" =
    let module With_interface = Circuit.With_interface (I) (O) in
    let circuit = With_interface.create_exn circuit ~name:"test" in
    let stats =
      Clock_domain_splitting.For_testing.Stats.create (Circuit.signal_graph circuit)
    in
    let clock_domain_size =
      Clock_domain_splitting.For_testing.Stats.clock_domain_size stats
    in
    Map.iteri clock_domain_size ~f:(fun ~key:clock_domain ~data:size ->
      print_s
        [%message
          (clock_domain : Clock_domain_splitting.For_testing.Clock_domain.t) (size : int)]);
    [%expect
      {|
      ((clock_domain Any) (size 2))
      ((clock_domain (Clocked ((clock 2) (edge Rising)))) (size 1))
      ((clock_domain (Clocked ((clock 4) (edge Rising)))) (size 1))
      ((clock_domain (Floating Input)) (size 4))
      ((clock_domain (Floating Multiple_clock_domains)) (size 2))
      |}];
    Clock_domain_splitting.For_testing.Stats.to_stat_summary_string stats |> print_endline;
    [%expect
      {|
      num total nodes: 10
      num nodes not any: 8
      num clocked nodes: 2
      percent of clocked non-any nodes: 25%
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
    let reg_1 = reg (Reg_spec.create () ~clock:clock_1) reg_1_input -- "reg_1" in
    reg_1_output <-- reg_1;
    let reg_2_output = wire 1 -- "reg2_output" in
    let reg_2_input = reg_1_output +: (reg_2_output ^: zero 1) in
    let reg_2 = reg (Reg_spec.create () ~clock:clock_2) reg_2_input -- "reg_2" in
    reg_2_output <-- reg_2;
    { O.out = reg_2 }
  ;;

  let%expect_test "Inputs to registers to not affect the register's clock domain" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock (__3)) (edge Rising)))
      (11):           Wire[id:9 bits:1 names:__8 deps:7] -> 7 (output)
      (12):           Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      (2):            Wire[id:4 bits:1 names:__3 deps:] -> () (input)
      out:            Wire[id:2 bits:1 names:__1 deps:5] -> 5
      out:            Wire[id:8 bits:1 names:__7 deps:2] -> 2 (output)
      reg2_output:    Wire[id:1 bits:1 names:__0 deps:5] -> 5
      reg_2:          Reg[id:5 bits:1 names:__4 deps:3,4]

      (Clocked ((clock (__2)) (edge Rising)))
      (5):            Wire[id:12 bits:1 names:__2 deps:] -> () (input)
      reg1_input:     Wire[id:11 bits:1 names:__1 deps:] -> () (input)
      reg1_output:    Wire[id:10 bits:1 names:__0 deps:13] -> 13
      reg1_output:    Wire[id:14 bits:1 names:__4 deps:10] -> 10 (output)
      reg_1:          Reg[id:13 bits:1 names:__3 deps:11,12]

      Floating
      (11):           Wire[id:23 bits:1 names:__8 deps:] -> () (input)
      (12):           Wire[id:28 bits:1 names:__13 deps:24] -> 24 (output)
      (2):            Wire[id:26 bits:1 names:__11 deps:18] -> 18 (output)
      (5):            Wire[id:25 bits:1 names:__10 deps:20] -> 20 (output)
      clock_1:        Wire[id:19 bits:1 names:__4 deps:] -> () (input)
      clock_2:        Wire[id:17 bits:1 names:__2 deps:] -> () (input)
      inpt:           Wire[id:15 bits:1 names:__0 deps:] -> () (input)
      reg1_input:     Op[id:22 bits:1 names:__7 deps:16,21] = add
      reg1_input:     Wire[id:27 bits:1 names:__12 deps:22] -> 22 (output)
      reg1_output:    Wire[id:21 bits:1 names:__6 deps:] -> () (input)
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
     | Mem_read_port { memory; _ } -> set_names memory [ { name = "mem"; loc = [%here] } ]
     | _ -> ());
    let out = mem.(0) -- "mem_read_out" in
    { O.out }
  ;;

  let%expect_test "If only one clock drives memory, it's in its own clock domain" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock (__2)) (edge Rising)))
      (2):            Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      mem:            Multiport_mem[id:4 bits:1 names:__3 deps:3,2,2,2]
      mem_read_out:   Mem_read_port[id:5 bits:1 names:__4 deps:2,4]
      out:            Wire[id:1 bits:1 names:__0 deps:5] -> 5
      out:            Wire[id:6 bits:1 names:__5 deps:1] -> 1 (output)
      zero:           Const[id:2 bits:1 names:__1 deps:] = 0

      Floating
      (2):            Wire[id:9 bits:1 names:__2 deps:8] -> 8 (output)
      clock:          Wire[id:7 bits:1 names:__0 deps:] -> () (input)
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
     | Mem_read_port { memory; _ } -> set_names memory [ { name = "mem"; loc = [%here] } ]
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
      clock_1:        Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      clock_2:        Wire[id:1 bits:1 names:__0 deps:] -> () (input)
      mem:            Multiport_mem[id:7 bits:1 names:__6 deps:4,6,6,6,2,6,6,6]
      mem_read_out:   Mem_read_port[id:8 bits:1 names:__7 deps:6,7]
      out:            Wire[id:5 bits:1 names:__4 deps:8] -> 8
      out:            Wire[id:9 bits:1 names:__8 deps:5] -> 5 (output)
      zero:           Const[id:6 bits:1 names:__5 deps:] = 0
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
      reg (Reg_spec.create () ~clock:read_clock) zero -- "read_addr_reg"
    in
    let read_addresses = Array.of_list [ read_address ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ { name = "mem"; loc = [%here] } ]
     | _ -> ());
    let read_output =
      reg (Reg_spec.create () ~clock:read_clock) (mem.(0) -- "mem_read_out")
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
      (Clocked ((clock (__2)) (edge Rising)))
      (4):            Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      mem_read_out:   Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      out:            Wire[id:1 bits:1 names:__0 deps:4] -> 4
      out:            Wire[id:8 bits:1 names:__7 deps:1] -> 1 (output)
      read_addr_reg:  Reg[id:6 bits:1 names:__5 deps:5,3]
      read_addr_reg:  Wire[id:7 bits:1 names:__6 deps:6] -> 6 (output)
      read_output_reg:Reg[id:4 bits:1 names:__3 deps:2,3]
      zero:           Const[id:5 bits:1 names:__4 deps:] = 0

      Floating
      (4):            Wire[id:17 bits:1 names:__8 deps:12] -> 12 (output)
      mem:            Multiport_mem[id:15 bits:1 names:__6 deps:10,14,14,14]
      mem_read_out:   Mem_read_port[id:16 bits:1 names:__7 deps:13,15]
      mem_read_out:   Wire[id:18 bits:1 names:__9 deps:16] -> 16 (output)
      read_addr_reg:  Wire[id:13 bits:1 names:__4 deps:] -> () (input)
      read_clock:     Wire[id:11 bits:1 names:__2 deps:] -> () (input)
      write_clock:    Wire[id:9 bits:1 names:__0 deps:] -> () (input)
      zero:           Const[id:14 bits:1 names:__5 deps:] = 0
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
    let read_address = reg (Reg_spec.create () ~clock) zero -- "read_addr_reg" in
    let read_addresses = Array.of_list [ read_address ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ { name = "mem"; loc = [%here] } ]
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
      (Clocked ((clock (__2)) (edge Rising)))
      (2):            Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      mem:            Multiport_mem[id:5 bits:1 names:__4 deps:3,2,2,2]
      out:            Wire[id:1 bits:1 names:__0 deps:6] -> 6
      out:            Wire[id:7 bits:1 names:__6 deps:1] -> 1 (output)
      read_addr_reg:  Reg[id:4 bits:1 names:__3 deps:2,3]
      zero:           Const[id:2 bits:1 names:__1 deps:] = 0

      Floating
      (2):            Wire[id:10 bits:1 names:__2 deps:9] -> 9 (output)
      clock:          Wire[id:8 bits:1 names:__0 deps:] -> () (input)
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
    let reg_1 = reg (Reg_spec.create ~clock ()) zero -- "reg_1" in
    let reg_2 = reg (Reg_spec.create ~clock:inverted_clock ()) reg_1 -- "reg_2" in
    { O.out = reg_2 }
  ;;

  let%expect_test "different clock signals produce different clock domains" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock (__1)) (edge Rising)))
      (2):            Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      reg_1:          Reg[id:3 bits:1 names:__2 deps:1,2]
      reg_1:          Wire[id:4 bits:1 names:__3 deps:3] -> 3 (output)
      zero:           Const[id:1 bits:1 names:__0 deps:] = 0

      (Clocked ((clock (__2)) (edge Rising)))
      inverted_clock: Wire[id:7 bits:1 names:__2 deps:] -> () (input)
      out:            Wire[id:5 bits:1 names:__0 deps:8] -> 8
      out:            Wire[id:9 bits:1 names:__4 deps:5] -> 5 (output)
      reg_1:          Wire[id:6 bits:1 names:__1 deps:] -> () (input)
      reg_2:          Reg[id:8 bits:1 names:__3 deps:6,7]

      Floating
      (2):            Wire[id:14 bits:1 names:__4 deps:11] -> 11 (output)
      clock:          Wire[id:10 bits:1 names:__0 deps:] -> () (input)
      inverted_clock: Op[id:12 bits:1 names:__2 deps:11] = not
      inverted_clock: Wire[id:13 bits:1 names:__3 deps:12] -> 12 (output)
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
    let read_address = reg (Reg_spec.create () ~clock:clock2) zero -- "read1_addr_reg" in
    let read_port_1_output = wire 1 -- "read2_addr" in
    let read_addresses = Array.of_list [ read_address; read_port_1_output ] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ { name = "mem"; loc = [%here] } ]
     | _ -> ());
    read_port_1_output <-- mem.(0);
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
      (Clocked ((clock (__1)) (edge Rising)))
      (4):            Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      read1_addr_reg: Reg[id:3 bits:1 names:__2 deps:1,2]
      read1_addr_reg: Wire[id:4 bits:1 names:__3 deps:3] -> 3 (output)
      zero:           Const[id:1 bits:1 names:__0 deps:] = 0

      Floating
      (4):            Wire[id:17 bits:1 names:__12 deps:6] -> 6 (output)
      clock1:         Wire[id:7 bits:1 names:__2 deps:] -> () (input)
      clock2:         Wire[id:5 bits:1 names:__0 deps:] -> () (input)
      mem:            Multiport_mem[id:13 bits:1 names:__8 deps:8,12,12,12]
      out:            Wire[id:10 bits:1 names:__5 deps:15] -> 15
      out:            Wire[id:16 bits:1 names:__11 deps:10] -> 10 (output)
      read1_addr_reg: Wire[id:11 bits:1 names:__6 deps:] -> () (input)
      read2_addr:     Wire[id:9 bits:1 names:__4 deps:14] -> 14
      zero:           Const[id:12 bits:1 names:__7 deps:] = 0
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
    let read_address = reg (Reg_spec.create () ~clock) zero -- "read1_addr_reg" in
    let read_addresses = [| read_address |] in
    let mem = multiport_memory 1 ~write_ports ~read_addresses in
    (match mem.(0) with
     | Mem_read_port { memory; _ } -> set_names memory [ { name = "mem"; loc = [%here] } ]
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
    let register = reg (Reg_spec.create () ~clock) value -- "read1_addr_reg" in
    register_output <-- register;
    { O.out = register }
  ;;

  let%expect_test "Constant chains are lifted to their appropriate domain" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock (__3)) (edge Rising)))
      (2):            Wire[id:4 bits:1 names:__3 deps:] -> () (input)
      out:            Wire[id:2 bits:1 names:__1 deps:5] -> 5
      out:            Wire[id:6 bits:1 names:__5 deps:2] -> 2 (output)
      read1_addr_reg: Reg[id:5 bits:1 names:__4 deps:3,4]
      value:          Wire[id:3 bits:1 names:__2 deps:] -> () (input)
      wire:           Wire[id:1 bits:1 names:__0 deps:5] -> 5
      wire:           Wire[id:7 bits:1 names:__6 deps:1] -> 1 (output)

      Floating
      (2):            Wire[id:18 bits:1 names:__10 deps:9] -> 9 (output)
      clock:          Wire[id:8 bits:1 names:__0 deps:] -> () (input)
      inpt:           Wire[id:10 bits:1 names:__2 deps:] -> () (input)
      one:            Const[id:14 bits:1 names:__6 deps:] = 1
      value:          Op[id:16 bits:1 names:__8 deps:12,11,15] = mux
      value:          Wire[id:17 bits:1 names:__9 deps:16] -> 16 (output)
      wire:           Wire[id:12 bits:1 names:__4 deps:] -> () (input)
      xor:            Op[id:15 bits:1 names:__7 deps:13,14] = xor
      zero:           Const[id:13 bits:1 names:__5 deps:] = 0
      |}]
  ;;
end

module%test _ = struct
  module I = struct
    type 'a t =
      { clock1 : 'a [@bits 1]
      ; clock2 : 'a [@bits 1]
      ; inpt : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { out : 'a [@bits 1] } [@@deriving hardcaml]
  end

  let circuit ({ clock1; clock2; inpt } : _ I.t) =
    let open Signal in
    let one = one 1 -- "one" in
    let xor = (inpt ^: one) -- "xor" in
    let r1 = reg (Reg_spec.create () ~clock:clock1) xor -- "r1" in
    let r2 = reg (Reg_spec.create () ~clock:clock2) xor -- "r2" in
    let out = r1 +: r2 -- "add" in
    { O.out }
  ;;

  let%expect_test "Some signals can be inputs to multiple clock domains" =
    let module Test = Make_test (I) (O) in
    Test.test circuit;
    [%expect
      {|
      (Clocked ((clock (__1)) (edge Rising)))
      (2):            Wire[id:2 bits:1 names:__1 deps:] -> () (input)
      r2:             Reg[id:3 bits:1 names:__2 deps:1,2]
      r2:             Wire[id:4 bits:1 names:__3 deps:3] -> 3 (output)
      xor:            Wire[id:1 bits:1 names:__0 deps:] -> () (input)

      (Clocked ((clock (__1)) (edge Rising)))
      (4):            Wire[id:6 bits:1 names:__1 deps:] -> () (input)
      r1:             Reg[id:7 bits:1 names:__2 deps:5,6]
      r1:             Wire[id:8 bits:1 names:__3 deps:7] -> 7 (output)
      xor:            Wire[id:5 bits:1 names:__0 deps:] -> () (input)

      Floating
      (2):            Wire[id:21 bits:1 names:__12 deps:15] -> 15 (output)
      (4):            Wire[id:22 bits:1 names:__13 deps:13] -> 13 (output)
      add:            Op[id:18 bits:1 names:__9 deps:16,17] = add
      clock1:         Wire[id:12 bits:1 names:__3 deps:] -> () (input)
      clock2:         Wire[id:14 bits:1 names:__5 deps:] -> () (input)
      inpt:           Wire[id:10 bits:1 names:__1 deps:] -> () (input)
      one:            Const[id:19 bits:1 names:__10 deps:] = 1
      out:            Wire[id:24 bits:1 names:__15 deps:9] -> 9 (output)
      out:            Wire[id:9 bits:1 names:__0 deps:18] -> 18
      r1:             Wire[id:16 bits:1 names:__7 deps:] -> () (input)
      r2:             Wire[id:17 bits:1 names:__8 deps:] -> () (input)
      xor:            Op[id:20 bits:1 names:__11 deps:11,19] = xor
      xor:            Wire[id:23 bits:1 names:__14 deps:20] -> 20 (output)
      |}]
  ;;
end

module%test _ = struct
  module Fifo = Async_fifo.Make (struct
      (* 8 deep, 4 wide *)
      let width = 4
      let log2_depth = 3
      let optimize_for_same_clock_rate_and_always_reading = false
    end)

  let fifo () =
    Fifo.create
      ~scope:(Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
  ;;

  let%expect_test "async fifo has 2 clock domains. The multiport memory is the only \
                   non-input part of the circuit in the floating clock domain"
    =
    let module Test = Make_test (Fifo.I) (Fifo.O) in
    Test.test (fifo ()) ~show:`Named ~show_kind:false;
    [%expect
      {|
      (Clocked
       ((clock (__5)) (edge Rising) (reset ((signal (__6)) (edge Rising)))))
      almost_empty:
      almost_empty:    (output)
      data_out:
      data_out:
      data_out:        (output)
      raddr_rd:
      raddr_rd:        (output)
      valid:
      valid:           (output)
      waddr_rd:
      waddr_rd_ff_0:

      (Clocked
       ((clock (__5)) (edge Rising) (reset ((signal (__6)) (edge Rising)))))
      full:
      full:            (output)
      raddr_wd:
      raddr_wd_ff_0:
      waddr_wd:
      waddr_wd:        (output)

      Floating
      clock_read:      (input)
      clock_write:     (input)
      data_in:         (input)
      gnd:
      raddr_rd:        (input)
      ram:
      read_enable:     (input)
      reset_read:      (input)
      reset_write:     (input)
      waddr_wd:        (input)
      write_enable:    (input)
      |}]
  ;;
end
