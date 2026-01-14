open! Core
open Core
module Simulator = Event_driven_sim.Simulator

module type S = Ops_intf.S

let ( !! ) = Simulator.( !! )
let ( !& ) = Simulator.( !& )
let ( <-- ) = Simulator.( <-- )
let ( <--- ) = Simulator.( <--- )

module Make (Comb : Logic.S) = struct
  module Signal = Hardcaml.Signal

  let to_bool sim_signal =
    let bits = ( !! ) sim_signal in
    assert (Comb.width bits = 1);
    Comb.to_bool bits
  ;;

  let is_edge sim_signal edge =
    let bits = ( !! ) sim_signal in
    assert (Comb.width bits = 1);
    let last_bits = Simulator.Signal.read_last sim_signal in
    match (edge : Hardcaml.Edge.t) with
    | Rising -> Comb.to_bool bits && not (Comb.to_bool last_bits)
    | Falling -> (not (Comb.to_bool bits)) && Comb.to_bool last_bits
  ;;

  let edge_to_bool edge =
    match (edge : Hardcaml.Edge.t) with
    | Falling -> false
    | Rising -> true
  ;;

  let compile_reg ~to_sim_signal signal ~source reg =
    let { Signal.Type.Reg.Register.clock = { clock; clock_edge }
        ; reset
        ; clear
        ; initialize_to = _
        ; enable
        }
      =
      reg
    in
    let to_sim_signal_opt t = Option.map t ~f:to_sim_signal in
    let sim_target = to_sim_signal signal in
    let sim_source = to_sim_signal source in
    let sim_clock = to_sim_signal clock in
    let sim_reset, reset_edge, sim_reset_to =
      Option.value_map
        reset
        ~default:(None, Hardcaml.Edge.Rising, None)
        ~f:(fun { reset; reset_edge; reset_to; _ } ->
          Some (to_sim_signal reset), reset_edge, Some (to_sim_signal reset_to))
    in
    let sim_clear, sim_clear_to =
      Option.value_map clear ~default:(None, None) ~f:(fun { clear; clear_to } ->
        Some (to_sim_signal clear), Some (to_sim_signal clear_to))
    in
    let sim_enable = to_sim_signal_opt enable in
    let source_width = Signal.width source in
    let value_or_zero value width =
      Option.value
        value
        ~default:(Comb.create_signal ~initial_value:(Comb.zero width) width)
    in
    [ ( List.filter_opt [ Some !&sim_clock; Option.map ~f:( !& ) sim_reset ]
      , fun () ->
          if match sim_reset with
             | Some sim_reset_v ->
               Bool.( = ) (to_bool sim_reset_v) (edge_to_bool reset_edge)
             | None -> false
          then sim_target <-- !!(value_or_zero sim_reset_to source_width)
          else if is_edge sim_clock clock_edge
          then
            if match sim_clear with
               | Some sim_clear_v -> to_bool sim_clear_v
               | None -> false
            then sim_target <-- !!(value_or_zero sim_clear_to source_width)
            else if Option.value_map sim_enable ~default:true ~f:to_bool
            then sim_target <-- !!sim_source )
    ]
  ;;

  module Memory_read_port = struct
    type t =
      { read_address : Comb.t Simulator.Signal.t
      ; read_data : Comb.t Simulator.Signal.t
      }
  end

  module Memory_data = struct
    type t =
      { array : Comb.t array
      ; read_ports : Memory_read_port.t list
      }
    [@@deriving fields ~getters]
  end

  let compile_write_port ~to_sim_signal ~memory_version memory_array write_port =
    let { Hardcaml.Write_port.write_clock; write_address; write_enable; write_data } =
      write_port
    in
    let sim_write_clock = to_sim_signal write_clock in
    let sim_write_address = to_sim_signal write_address in
    let sim_write_enable = to_sim_signal write_enable in
    let sim_write_data = to_sim_signal write_data in
    ( [ !&sim_write_clock ]
    , fun () ->
        if is_edge sim_write_clock Rising && to_bool sim_write_enable
        then (
          let address = Comb.to_int_trunc !!sim_write_address in
          if Comb.compare memory_array.(address) !!sim_write_data <> 0
          then (
            memory_array.(address) <- !!sim_write_data;
            Simulator.Version_signal.increment memory_version)) )
  ;;

  let compile_multiport_mem ~memories ~to_sim_signal memory_uid write_ports =
    let { Memory_data.array; read_ports } = Map.find_exn memories memory_uid in
    let memory_version = Simulator.Version_signal.create () in
    let write_processes =
      List.map
        ~f:(compile_write_port ~to_sim_signal ~memory_version array)
        (Array.to_list write_ports)
    in
    let read_processes =
      List.map read_ports ~f:(fun { Memory_read_port.read_address; read_data } ->
        ( [ Simulator.Signal.id read_address; Simulator.Signal.id memory_version ]
        , fun () ->
            let address = Comb.to_int_trunc !!read_address in
            read_data <-- array.(address) ))
    in
    read_processes @ write_processes
  ;;

  let process_for_signal ~to_sim_signal ~external_insts ~delay ~memories signal =
    let[@inline] comb_process (eval_f : unit -> Comb.t) =
      let sim_signal = to_sim_signal signal in
      let deps =
        Signal.Type.Deps.map signal ~f:(Fn.compose Simulator.Signal.id to_sim_signal)
      in
      [ (deps, fun () -> ( <--- ) sim_signal (eval_f ()) ~delay) ]
    in
    match (signal : Signal.t) with
    | Empty -> failwith "can't compile empty signal"
    | Wire { driver = None; _ } -> failwith "Cannot compile undriven wire"
    | Const { constant; _ } -> comb_process (fun () -> Comb.of_bits constant)
    | Not { arg; _ } ->
      let d = to_sim_signal arg in
      comb_process (fun () -> Comb.( ~: ) (Simulator.Signal.read d))
    | Cat { args; _ } ->
      let deps = List.map ~f:to_sim_signal args in
      comb_process (fun () -> Comb.concat_msb (List.map ~f:Simulator.Signal.read deps))
    | Mux { select; cases; _ } ->
      let d = to_sim_signal select in
      let rest = List.map ~f:to_sim_signal cases in
      comb_process (fun () ->
        Comb.mux (Simulator.Signal.read d) (List.map ~f:Simulator.Signal.read rest))
    | Cases { select; cases; default; _ } ->
      let select = to_sim_signal select in
      let default = to_sim_signal default in
      let cases =
        List.map cases ~f:(fun (match_with, value) ->
          to_sim_signal match_with, to_sim_signal value)
      in
      comb_process (fun () ->
        Comb.cases
          ~default:(Simulator.Signal.read default)
          (Simulator.Signal.read select)
          (List.map cases ~f:(fun (match_with, value) ->
             Simulator.Signal.read match_with, Simulator.Signal.read value)))
    | Op2 { op; arg_a; arg_b; _ } ->
      let op2 op a b =
        let a = to_sim_signal a in
        let b = to_sim_signal b in
        comb_process (fun () -> op (Simulator.Signal.read a) (Simulator.Signal.read b))
      in
      (match op with
       | Add -> op2 Comb.( +: )
       | Sub -> op2 Comb.( -: )
       | Mulu -> op2 Comb.( *: )
       | Muls -> op2 Comb.( *+ )
       | And -> op2 Comb.( &: )
       | Or -> op2 Comb.( |: )
       | Xor -> op2 Comb.( ^: )
       | Eq -> op2 Comb.( ==: )
       | Lt -> op2 Comb.( <: ))
        arg_a
        arg_b
    | Wire { driver = Some driver; _ } ->
      let src = to_sim_signal driver in
      comb_process (fun () -> Simulator.Signal.read src)
    | Select { arg; high; low; _ } ->
      let d = to_sim_signal arg in
      comb_process (fun () -> Comb.select (Simulator.Signal.read d) ~high ~low)
    | Reg { register; d; _ } -> compile_reg ~to_sim_signal signal ~source:d register
    | Multiport_mem { write_ports; _ } ->
      compile_multiport_mem ~memories ~to_sim_signal (Signal.uid signal) write_ports
    | Mem_read_port _ -> []
    | Inst { instantiation = { inputs; _ }; _ } ->
      let inputs =
        List.map ~f:(fun { input_signal; _ } -> to_sim_signal input_signal) inputs
      in
      let output_signal = external_insts signal ~inputs in
      comb_process (fun () -> Simulator.Signal.read output_signal)
  ;;

  let create_from_signal ~signal =
    let width = Signal.width signal in
    let initial_value =
      match signal with
      | Reg { register = { initialize_to = Some initialize_to; _ }; _ } ->
        Comb.of_bits initialize_to
      | _ -> Comb.zero width
    in
    Comb.create_signal ~initial_value width
  ;;

  let make_simulator_signals graph =
    Hardcaml.Signal_graph.fold
      graph
      ~init:(Map.empty (module Signal.Type.Uid))
      ~f:(fun acc signal ->
        if not (Signal.is_empty signal)
        then (
          let data = create_from_signal ~signal in
          Map.add_exn acc ~key:(Signal.uid signal) ~data)
        else acc)
  ;;

  let create_memories ~to_sim_signal graph =
    let memories =
      Hardcaml.Signal_graph.fold
        graph
        ~init:(Map.empty (module Signal.Type.Uid))
        ~f:(fun acc signal ->
          match signal with
          | Multiport_mem { size; initialize_to; _ } ->
            let data_width = Signal.width signal in
            Map.add_exn
              acc
              ~key:(Signal.uid signal)
              ~data:
                { Memory_data.array =
                    (match initialize_to with
                     | None -> Array.create ~len:size (Comb.zero data_width)
                     | Some initialize_to -> Array.map initialize_to ~f:Comb.of_bits)
                ; read_ports = []
                }
          | _ -> acc)
    in
    Hardcaml.Signal_graph.fold graph ~init:memories ~f:(fun acc signal ->
      match signal with
      | Mem_read_port { memory; read_address; _ } ->
        Map.update acc (Signal.uid memory) ~f:(fun memory_data_opt ->
          let memory = Option.value_exn memory_data_opt in
          { memory with
            Memory_data.read_ports =
              { Memory_read_port.read_address = to_sim_signal read_address
              ; read_data = to_sim_signal signal
              }
              :: Memory_data.read_ports memory
          })
      | _ -> acc)
  ;;

  let make_processes ~to_sim_signal ~external_insts ~delay graph =
    let memories = create_memories ~to_sim_signal graph in
    let processes =
      Hardcaml.Signal_graph.fold graph ~init:[] ~f:(fun acc signal ->
        if Signal.is_empty signal
           || (Signal.Type.is_wire signal
               && Option.is_none (Signal.Type.wire_driver signal))
        then acc
        else (
          let processes =
            process_for_signal
              ~to_sim_signal
              ~external_insts
              ~delay:(delay signal)
              ~memories
              signal
          in
          processes :: acc))
      |> List.concat
    in
    let memories =
      Map.map memories ~f:(fun memory_data ->
        Hardcaml.Private.Simulation_memory.of_evsim_memory memory_data.array)
    in
    List.map processes ~f:(fun (deps, f) -> Simulator.Process.create deps f), memories
  ;;

  let inst_not_supported signal ~inputs:_ =
    raise_s [%message "Inst signals are unsupported" (signal : Signal.t)]
  ;;

  type t =
    { processes : Simulator.Process.t list
    ; find_sim_signal : Hardcaml.Signal.t -> Comb.t Simulator.Signal.t
    ; fake_sim_signal : Hardcaml.Signal.t -> Comb.t Simulator.Signal.t
    ; memories : Comb.t Hardcaml.Private.Simulation_memory.t Map.M(Signal.Type.Uid).t
    }
  [@@deriving fields ~getters]

  let make_find_sim_signal signal_map signal ~map_uid =
    match Map.find signal_map (map_uid (Signal.uid signal)) with
    | Some s -> s
    | None ->
      raise_s
        [%message
          "signal not mapped to simulator signal"
            (signal : Signal.t)
            ~uid:(Signal.uid signal : Signal.Type.Uid.t)]
  ;;

  let aux_create ~processes ~find_sim_signal ~memories =
    let fake_sim_signal signal = create_from_signal ~signal in
    { processes; find_sim_signal; fake_sim_signal; memories }
  ;;

  let lookup_memory_exn { memories; _ } uid = Map.find_exn memories uid

  let with_maybe_combined_wires ~signal_graph ~internally_traced_signals ~combine_wires ~f
    =
    let%tydi { new_signals; old_signal_to_new_signal } =
      if combine_wires
      then Combine_wires.combine (Hardcaml.Signal_graph.outputs signal_graph)
      else (
        let old_signal_to_new_signal =
          (Hardcaml.Signal_graph.fold signal_graph)
            ~init:(Map.empty (module Signal.Type.Uid))
            ~f:(fun map signal -> Map.add_exn map ~key:(Signal.uid signal) ~data:signal)
        in
        { new_signals = Hardcaml.Signal_graph.outputs signal_graph
        ; old_signal_to_new_signal
        })
    in
    let signal_graph = Hardcaml.Signal_graph.create new_signals in
    let internally_traced_signals =
      List.map internally_traced_signals ~f:(fun traced_signal ->
        Map.find_exn old_signal_to_new_signal (Signal.uid traced_signal))
    in
    let processes, memories, signal_map = f ~signal_graph ~internally_traced_signals in
    (* [find_sim_signal] and [memories] need to be modified so that they are in terms of
       the original signal graph uids. Before this point everything is in terms of the
       combined wires uids. *)
    let find_sim_signal =
      let map_uid original_uid =
        Map.find_exn old_signal_to_new_signal original_uid |> Signal.uid
      in
      make_find_sim_signal signal_map ~map_uid
    in
    let memories =
      let old_signal_by_new_signal =
        Map.to_alist old_signal_to_new_signal
        |> List.map ~f:Tuple2.swap
        |> List.map ~f:(Tuple2.map_fst ~f:Signal.uid)
        |> Map.of_alist_multi (module Signal.Type.Uid)
      in
      memories
      |> Map.map_keys_exn (module Signal.Type.Uid) ~f:(fun new_uid ->
        match Map.find_exn old_signal_by_new_signal new_uid with
        | [ old_uid ] -> old_uid
        | _ -> raise_s [%message "Multiple uids for memory"])
    in
    aux_create ~processes ~find_sim_signal ~memories
  ;;

  let circuit_to_processes
    ?(delay = fun _ -> 0)
    ?(external_insts = inst_not_supported)
    circuit
    ~combine_wires
    =
    let signal_graph = Hardcaml.Circuit.signal_graph circuit in
    with_maybe_combined_wires
      ~signal_graph
      ~combine_wires
      ~internally_traced_signals:[]
      ~f:(fun ~signal_graph ~internally_traced_signals ->
        assert (List.is_empty internally_traced_signals);
        let signal_map = make_simulator_signals signal_graph in
        let find_sim_signal = make_find_sim_signal signal_map ~map_uid:Fn.id in
        let processes, memories =
          make_processes
            ~external_insts
            ~to_sim_signal:find_sim_signal
            ~delay
            signal_graph
        in
        processes, memories, signal_map)
  ;;

  (* Start of helper functions for hybrid evsim construction *)
  include struct
    open Hardcaml
    module Circuit = Circuit
    module Cyclesim = Cyclesim
    module Signal_graph = Signal_graph
  end

  let make_old_signal_by_old_uid ~original_signal_graph =
    original_signal_graph
    |> Signal_graph.fold
         ~init:(Map.empty (module Signal.Type.Uid))
         ~f:(fun old_signal_by_old_uid signal ->
           Map.add_exn old_signal_by_old_uid ~key:(Signal.uid signal) ~data:signal)
  ;;

  let make_sim_signals_by_new_uids ~old_uid_signal_kinds ~old_signal_by_old_uid =
    old_uid_signal_kinds
    |> Map.to_alist
    |> List.concat_map
         ~f:
           (fun
             (old_uid, (signal_kind : Clock_domain_splitting.Original_signal_kind.t)) ->
           let old_signal = Map.find_exn old_signal_by_old_uid old_uid in
           match signal_kind with
           | Circuit_input (Input { new_domain = _; new_uid }) ->
             let sim_signal = create_from_signal ~signal:old_signal in
             [ new_uid, sim_signal ]
           | Circuit_input
               (Boundary
                 { new_output = { new_output_wire; new_output_driver }
                 ; new_output_domain = _
                 ; new_inputs
                 })
           | Boundary
               { new_output = { new_output_wire; new_output_driver }
               ; new_output_domain = _
               ; new_inputs
               } ->
             let boundary_signal = create_from_signal ~signal:old_signal in
             (* If an original signal is the output of a clock domain and the input of
                other clock domains, the new signals all share the same evsim signal. This
                is how values are communicated across clock domains: the
                [new_output_domain] writes to the evsim signal and the input domains read
                from the evsim signal. *)
             let boundary_mappings =
               List.map (new_output_wire :: Map.data new_inputs) ~f:(fun new_uid ->
                 new_uid, boundary_signal)
             in
             (* Sometimes we don't need an evsim signal for the driver, like when the
                driver is a completely internal node of a cyclesim. But in all other cases
                we do need this evsim signal, so we just always add it. *)
             let driver_mapping =
               new_output_driver, create_from_signal ~signal:old_signal
             in
             driver_mapping :: boundary_mappings
           | Internal { new_uids } ->
             new_uids
             |> Map.to_alist
             |> List.filter_map ~f:(fun (clock_domain, new_uid) ->
               match clock_domain with
               | Floating ->
                 (* Only the floating domain needs an evsim signal for internal signal *)
                 let sim_signal = create_from_signal ~signal:old_signal in
                 Some (new_uid, sim_signal)
               | Clocked _ -> None))
    |> Map.of_alist_exn (module Signal.Type.Uid)
  ;;

  let rekey_memories_to_old_uids memories ~new_signals_by_original_uids =
    let new_uid_to_old_uid =
      new_signals_by_original_uids
      |> Map.to_alist
      |> List.map
           ~f:
             (fun
               ( old_signal
               , (new_signal : Clock_domain_splitting.Copied_circuit.New_signal.t) )
             ->
             let new_signal =
               match new_signal with
               | Internal new_signal
               | Output { output_wire = _; output_driver = new_signal } -> new_signal
             in
             new_signal, old_signal)
      |> Hashtbl.of_alist_exn (module Signal.Type.Uid)
    in
    Map.map_keys_exn (module Signal.Type.Uid) memories ~f:(fun new_uid ->
      Hashtbl.find_exn new_uid_to_old_uid new_uid)
  ;;

  type signal_port_list = (Signal.t * Hardcaml.Bits.t ref) list

  (* The signals in the returned [signal_port_list]s are the new signals *)
  let create_cyclesim_from_circuit
    (clock_domain : Clock_domain_splitting.Clock_spec.t)
    ({ circuit; new_signals_by_original_uids } : Clock_domain_splitting.Copied_circuit.t)
    ~combinational_ops_database
    ~random_initializer
    ~cyclesim_create
    : (signal_port_list, signal_port_list) Cyclesim.t
      * Comb.t Hardcaml.Private.Simulation_memory.t Map.M(Signal.Type.Uid).t
    =
    let create_name_to_signal_map signals =
      List.map signals ~f:(fun signal ->
        let name = Signal.names signal |> List.hd_exn in
        name, signal)
      |> String.Map.of_alist_exn
    in
    let input_signals_by_name = Circuit.inputs circuit |> create_name_to_signal_map in
    let output_signals_by_name = Circuit.outputs circuit |> create_name_to_signal_map in
    let clock_names = Signal.names clock_domain.clock in
    let cyclesim =
      cyclesim_create
        ~config:
          { Cyclesim.Config.is_internal_port =
              Some (fun signal -> Signal.Type.is_mem signal)
              (* Trace all memories so that [Cyclesim.lookup_mem_by_name] works below *)
          ; combinational_ops_database
          ; deduplicate_signals =
              false
              (* [Hardcaml.Dedup.deduplicate] only deduplicates signals without a name.
                 Since all signals have a name due to the clock domain splitting code,
                 setting this to true won't do anything (it actually makes things a smidge
                 slower because it adds a wire between every register and its output). *)
          ; store_circuit = false
          ; random_initializer
          ; clock_mode = All_one_domain
          }
        ~clock_names
        circuit
    in
    let port_to_signal_bit_map port_list ~name_to_signal_map =
      List.map port_list ~f:(fun (name, bits) ->
        let signal = Map.find_exn name_to_signal_map name in
        signal, bits)
    in
    let memories =
      circuit
      |> Circuit.signal_graph
      |> Signal_graph.fold ~init:[] ~f:(fun memories signal ->
        if Signal.Type.is_mem signal
        then (
          (* Sort of an abuse of abstraction, but the clock domain splitting algorithm
             assigns every signal a name, so this is guaranteed to always return a name *)
          let name = Signal.names signal |> List.hd_exn in
          let memory = Cyclesim.lookup_mem_by_name cyclesim name |> Option.value_exn in
          ( Signal.uid signal
          , Hardcaml.Private.Simulation_memory.of_cyclesim_memory
              memory
              ~of_bits:Comb.of_bits )
          :: memories)
        else memories)
      |> Map.of_alist_exn (module Signal.Type.Uid)
      |> rekey_memories_to_old_uids ~new_signals_by_original_uids
    in
    let cyclesim =
      Cyclesim.Private.coerce
        cyclesim
        ~to_input:(port_to_signal_bit_map ~name_to_signal_map:input_signals_by_name)
        ~to_output:(port_to_signal_bit_map ~name_to_signal_map:output_signals_by_name)
    in
    cyclesim, memories
  ;;

  let compute_circuit_max_depth circuit =
    let module Deps = Signal_graph.Deps_for_simulation_scheduling in
    let signal_depth = Hashtbl.create (module Signal.Type.Uid) in
    circuit
    |> Circuit.signal_graph
    |> Signal_graph.depth_first_search
         ~deps:(module Deps)
         ~init:0
         ~f_after:(fun max_depth signal ->
           let max_dep_depth =
             Deps.fold signal ~init:0 ~f:(fun max dep ->
               Int.max (Hashtbl.find_exn signal_depth (Signal.uid dep)) max)
           in
           let depth = max_dep_depth + 1 in
           Hashtbl.set signal_depth ~key:(Signal.uid signal) ~data:depth;
           Int.max depth max_depth)
  ;;

  let create_cyclesim_process
    ({ clock; edge; reset } : Clock_domain_splitting.Clock_spec.t)
    cyclesim
    ~find_sim_signal
    ~output_depth
    =
    let sim_clock = find_sim_signal clock in
    let sim_reset_and_edge =
      Option.map reset ~f:(fun { signal; edge } -> find_sim_signal signal, edge)
    in
    let signals_with_bits_to_simulator_signals signal_to_bits_map =
      signal_to_bits_map
      |> List.map ~f:(fun (signal, bits) -> find_sim_signal signal, bits)
      |> Array.of_list
    in
    let inputs =
      (* Only include inputs that aren't the clock. This is mostly because
         [Hardcaml_verilator] already sets the clocks from low to high, so setting the
         clock again here causes registers to get updated twice on the first clock cycle.
         More generally, it seems reasonable to exclude the clock of the [Cyclesim.t] from
         being set since the [Cyclesim.t] is supposed to handle the clock. *)
      let inputs_not_clock =
        List.filter (Cyclesim.inputs cyclesim) ~f:(fun (input_signal, _) ->
          not ([%equal: Signal.Type.Uid.t] (Signal.uid input_signal) (Signal.uid clock)))
      in
      signals_with_bits_to_simulator_signals inputs_not_clock
    in
    let outputs =
      signals_with_bits_to_simulator_signals (Cyclesim.outputs ~clock_edge:After cyclesim)
    in
    let read_inputs () =
      Array.iter inputs ~f:(fun (signal, bits) ->
        bits := Comb.to_bits_exn (Simulator.Signal.read signal))
    in
    let write_outputs () =
      Array.iter outputs ~f:(fun (signal, bits) -> signal <-- Comb.of_bits !bits)
    in
    let recompute_comb () =
      read_inputs ();
      Cyclesim.cycle_check cyclesim;
      Cyclesim.cycle_before_clock_edge cyclesim;
      Cyclesim.cycle_after_clock_edge cyclesim
    in
    let update_outputs_signal = Comb.create_signal ~initial_value:(Comb.zero 1) 1 in
    (* We do this to delay writing the outputs a number of delta steps equal to the
       maximum number of nodes between a register and an output of the circuit. This
       allows to roughly have the correct delta step delay on each of the output signals. *)
    let update_outputs_delayed_procs =
      let signals =
        List.init output_depth ~f:(fun _ ->
          Comb.create_signal ~initial_value:(Comb.zero 1) 1)
      in
      let last_signal, processes =
        List.fold
          signals
          ~init:(update_outputs_signal, [])
          ~f:(fun (prev_signal, processes) signal ->
            ( signal
            , Simulator.Process.create [ !&prev_signal ] (fun () ->
                signal <-- !!prev_signal)
              :: processes ))
      in
      let is_initial_run = ref true in
      Simulator.Process.create [ !&last_signal ] (fun () ->
        if !is_initial_run then is_initial_run := false else write_outputs ())
      :: processes
    in
    let deps =
      List.filter_opt
        [ Some !&sim_clock
        ; Option.map sim_reset_and_edge ~f:(fun (reset_signal, _edge) -> !&reset_signal)
        ]
    in
    let is_initial_run = ref true in
    let f () =
      if !is_initial_run
      then (
        recompute_comb ();
        update_outputs_signal <-- Comb.( ~: ) !!update_outputs_signal;
        is_initial_run := false)
      else if match sim_reset_and_edge with
              | Some (reset_signal, edge) ->
                Bool.( = ) (to_bool reset_signal) (edge_to_bool edge)
              | None -> false
      then (
        Cyclesim.reset cyclesim;
        recompute_comb ();
        update_outputs_signal <-- Comb.( ~: ) !!update_outputs_signal)
      else if is_edge sim_clock edge
      then (
        read_inputs ();
        Cyclesim.cycle cyclesim;
        update_outputs_signal <-- Comb.( ~: ) !!update_outputs_signal)
    in
    Simulator.Process.create deps f :: update_outputs_delayed_procs
  ;;

  let find_sim_signals_for_old_uids ~old_uid_signal_kinds ~sim_signals_by_new_uid =
    old_uid_signal_kinds
    |> Map.to_alist
    |> List.filter_map
         ~f:
           (fun
             (old_uid, (signal_kind : Clock_domain_splitting.Original_signal_kind.t)) ->
           let maybe_representative_new_uid =
             match signal_kind with
             | Circuit_input (Input { new_domain = _; new_uid }) -> Some new_uid
             | Circuit_input
                 (Boundary
                   { new_output = { new_output_wire = _; new_output_driver }
                   ; new_output_domain = _
                   ; new_inputs = _
                   }) ->
               (* For circuit inputs, we want to use the [new_output_driver], since it
                  won't be driven by a process *)
               Some new_output_driver
             | Boundary
                 { new_output = { new_output_wire; new_output_driver = _ }
                 ; new_output_domain = _
                 ; new_inputs = _
                 } ->
               (* For clock domain outputs, new know that the [new_output_wire] is
                  definitely going to be driven, so use that as the representative uid *)
               Some new_output_wire
             | Internal { new_uids } -> Map.find new_uids Floating
           in
           Option.map maybe_representative_new_uid ~f:(fun new_uid ->
             let sim_signal = Map.find_exn sim_signals_by_new_uid new_uid in
             old_uid, sim_signal))
    |> Map.of_alist_exn (module Signal.Type.Uid)
  ;;

  let circuit_to_hybrid_processes
    ?(delay = fun _ -> 0)
    ?(external_insts = inst_not_supported)
    original_circuit
    ~internally_traced_signals
    ~combine_wires
    ~combinational_ops_database
    ~random_initializer
    ~config_options:{ Hybrid_sim_options.cyclesim_create }
    =
    let signal_graph = Circuit.signal_graph original_circuit in
    with_maybe_combined_wires
      ~signal_graph
      ~internally_traced_signals
      ~combine_wires
      ~f:(fun ~signal_graph ~internally_traced_signals ->
        let traced_memory_signals, internally_traced_signals =
          internally_traced_signals
          |> List.partition_tf ~f:(fun signal -> Hardcaml.Signal.Type.is_mem signal)
        in
        let clock_domains =
          Clock_domain_splitting.group_by_clock_domain
            signal_graph
            ~extra_outputs:internally_traced_signals
        in
        let old_uid_signal_kinds =
          Clock_domain_splitting.classify_original_uids
            ~original_signal_graph:signal_graph
            ~clock_domains
        in
        let sim_signals_by_new_uid =
          let old_signal_by_old_uid =
            make_old_signal_by_old_uid ~original_signal_graph:signal_graph
          in
          make_sim_signals_by_new_uids ~old_uid_signal_kinds ~old_signal_by_old_uid
        in
        let find_sim_signal ~new_signal =
          Map.find_exn sim_signals_by_new_uid (Signal.uid new_signal)
        in
        let floating_circuit, clocked_circuits =
          clock_domains
          |> Map.to_alist
          |> List.partition_map ~f:(fun (clock_domain, graph) ->
            match (clock_domain : Clock_domain_splitting.Clock_domain.t) with
            | Floating -> First graph
            | Clocked clock -> Second (clock, graph))
        in
        let cyclesims_by_clock_domain, cyclesim_memories =
          List.map clocked_circuits ~f:(fun (clock_domain, copied_circuit) ->
            let cyclesim, memories =
              create_cyclesim_from_circuit
                clock_domain
                copied_circuit
                ~combinational_ops_database
                ~random_initializer
                ~cyclesim_create
            in
            (clock_domain, (copied_circuit, cyclesim)), memories)
          |> List.unzip
        in
        let processes, memories =
          let floating_processes, memories =
            List.map floating_circuit ~f:(fun { circuit; new_signals_by_original_uids } ->
              let floating_processes, memories =
                make_processes
                  ~external_insts
                  ~to_sim_signal:(fun signal -> find_sim_signal ~new_signal:signal)
                  ~delay
                  (Circuit.signal_graph circuit)
              in
              ( floating_processes
              , rekey_memories_to_old_uids memories ~new_signals_by_original_uids ))
            |> List.unzip
          in
          let cyclesim_processes =
            List.map
              cyclesims_by_clock_domain
              ~f:
                (fun
                  (clock_domain, ({ circuit; new_signals_by_original_uids = _ }, cyclesim))
                ->
                let output_depth = compute_circuit_max_depth circuit in
                create_cyclesim_process
                  clock_domain
                  cyclesim
                  ~output_depth
                  ~find_sim_signal:(fun signal -> find_sim_signal ~new_signal:signal))
          in
          List.concat (floating_processes @ cyclesim_processes), memories
        in
        let sim_signals_by_old_uid =
          find_sim_signals_for_old_uids ~old_uid_signal_kinds ~sim_signals_by_new_uid
        in
        let sim_signals_by_old_uid =
          (* Add in the traced memory signals as fake signals *)
          List.fold
            traced_memory_signals
            ~init:sim_signals_by_old_uid
            ~f:(fun sim_signals_by_old_uid signal ->
              Map.set
                sim_signals_by_old_uid
                ~key:(Signal.uid signal)
                ~data:(create_from_signal ~signal))
        in
        let memories =
          List.fold
            (memories @ cyclesim_memories)
            ~init:(Map.empty (module Signal.Type.Uid))
            ~f:
              (Map.merge_skewed
                 ~combine:
                   (fun
                     ~key:uid
                     (_ : _ Hardcaml.Private.Simulation_memory.t)
                     (_ : _ Hardcaml.Private.Simulation_memory.t)
                   ->
                   raise_s
                     [%message
                       "BUG: memory in multiple clock domains" (uid : Signal.Type.Uid.t)]))
        in
        processes, memories, sim_signals_by_old_uid)
  ;;
end
