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
  module Reg_spec = Hardcaml.Reg_spec

  let to_bool sim_signal =
    let bits = ( !! ) sim_signal in
    assert (Comb.width bits = 1);
    Comb.is_vdd bits
  ;;

  let is_edge sim_signal edge =
    let bits = ( !! ) sim_signal in
    assert (Comb.width bits = 1);
    let last_bits = Simulator.Signal.read_last sim_signal in
    match (edge : Hardcaml.Edge.t) with
    | Rising -> Comb.is_vdd bits && Comb.is_gnd last_bits
    | Falling -> Comb.is_gnd bits && Comb.is_vdd last_bits
  ;;

  let edge_to_bool edge =
    match (edge : Hardcaml.Edge.t) with
    | Falling -> false
    | Rising -> true
  ;;

  let level_to_bool level =
    match (level : Hardcaml.Level.t) with
    | Low -> false
    | High -> true
  ;;

  let compile_reg ~to_sim_signal signal ~source reg =
    let { Reg_spec.reg_clock
        ; reg_clock_edge
        ; reg_reset
        ; reg_reset_edge
        ; reg_reset_value
        ; reg_clear
        ; reg_clear_level
        ; reg_clear_value
        ; reg_enable
        }
      =
      reg
    in
    let to_sim_signal_opt reg =
      if Signal.is_empty reg then None else Some (to_sim_signal reg)
    in
    let sim_target = to_sim_signal signal in
    let sim_source = to_sim_signal source in
    let sim_clock = to_sim_signal reg_clock in
    let sim_reset = to_sim_signal_opt reg_reset in
    let sim_reset_value = to_sim_signal_opt reg_reset_value in
    let sim_clear = to_sim_signal_opt reg_clear in
    let sim_clear_value = to_sim_signal_opt reg_clear_value in
    let sim_enable = to_sim_signal reg_enable in
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
               Bool.( = ) (to_bool sim_reset_v) (edge_to_bool reg_reset_edge)
             | None -> false
          then sim_target <-- !!(value_or_zero sim_reset_value source_width)
          else if is_edge sim_clock reg_clock_edge
          then
            if match sim_clear with
               | Some sim_clear_v ->
                 Bool.( = ) (to_bool sim_clear_v) (level_to_bool reg_clear_level)
               | None -> false
            then sim_target <-- !!(value_or_zero sim_clear_value source_width)
            else if to_bool sim_enable
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
          let address = Comb.to_int !!sim_write_address in
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
            let address = Comb.to_int !!read_address in
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
    | Op2 { op; arg_a; arg_b; _ } ->
      let op2 op a b =
        let a = to_sim_signal a in
        let b = to_sim_signal b in
        comb_process (fun () -> op (Simulator.Signal.read a) (Simulator.Signal.read b))
      in
      (match op with
       | Signal_add -> op2 Comb.( +: )
       | Signal_sub -> op2 Comb.( -: )
       | Signal_mulu -> op2 Comb.( *: )
       | Signal_muls -> op2 Comb.( *+ )
       | Signal_and -> op2 Comb.( &: )
       | Signal_or -> op2 Comb.( |: )
       | Signal_xor -> op2 Comb.( ^: )
       | Signal_eq -> op2 Comb.( ==: )
       | Signal_lt -> op2 Comb.( <: ))
        arg_a
        arg_b
    | Wire { driver; _ } ->
      let src = to_sim_signal !driver in
      comb_process (fun () -> Simulator.Signal.read src)
    | Select { arg; high; low; _ } ->
      let d = to_sim_signal arg in
      comb_process (fun () -> Comb.select (Simulator.Signal.read d) high low)
    | Reg { register; d; _ } -> compile_reg ~to_sim_signal signal ~source:d register
    | Multiport_mem { write_ports; _ } ->
      compile_multiport_mem ~memories ~to_sim_signal (Signal.uid signal) write_ports
    | Mem_read_port _ -> []
    | Inst { instantiation = { inst_inputs; _ }; _ } ->
      let inputs = List.map ~f:(Fn.compose to_sim_signal snd) inst_inputs in
      let output_signal = external_insts signal ~inputs in
      comb_process (fun () -> Simulator.Signal.read output_signal)
  ;;

  let create_from_signal ~signal =
    let width = Signal.width signal in
    Comb.create_signal ~initial_value:(Comb.zero width) width
  ;;

  let make_simulator_signals graph =
    Hardcaml.Signal_graph.fold
      graph
      ~init:(Map.empty (module Signal.Uid))
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
        ~init:(Map.empty (module Signal.Uid))
        ~f:(fun acc signal ->
          match signal with
          | Multiport_mem { size; write_ports; _ } ->
            let data_width = Signal.width write_ports.(0).write_data in
            Map.add_exn
              acc
              ~key:(Signal.uid signal)
              ~data:
                { Memory_data.array = Array.create ~len:size (Comb.zero data_width)
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
    List.map processes ~f:(fun (deps, f) -> Simulator.Process.create deps f)
  ;;

  let inst_not_supported signal ~inputs:_ =
    raise_s [%message "Inst signals are unsupported" (signal : Signal.t)]
  ;;

  type t =
    { processes : Simulator.Process.t list
    ; find_sim_signal : Hardcaml.Signal.t -> Comb.t Simulator.Signal.t
    ; fake_sim_signal : Hardcaml.Signal.t -> Comb.t Simulator.Signal.t
    }
  [@@deriving fields ~getters]

  let circuit_to_processes
    ?(delay = fun _ -> 0)
    ?(external_insts = inst_not_supported)
    circuit
    =
    let graph = Hardcaml.Circuit.signal_graph circuit in
    let signal_map = make_simulator_signals graph in
    let find_sim_signal signal =
      match Map.find signal_map (Signal.uid signal) with
      | Some s -> s
      | None ->
        raise_s
          [%message
            "signal not mapped to simulator signal"
              (signal : Signal.t)
              ~uid:(Signal.uid signal : Signal.Uid.t)]
    in
    let fake_sim_signal signal = create_from_signal ~signal in
    let processes =
      make_processes ~external_insts ~to_sim_signal:find_sim_signal ~delay graph
    in
    { processes; find_sim_signal; fake_sim_signal }
  ;;
end
