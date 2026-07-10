open Base
module M = Waveterm_intf.M
module Sim = Event_driven_sim.Simulator
module Events = Hardcaml.Wave_data_in_events.Bits

module Make (Logic : Logic.S) = struct
  type t =
    { processes : Sim.Process.t list
    ; waveform : Hardcaml.Wave_data.t
    }

  let create_waves typ (ports_to_events : (Logic.t Port.t, Events.t) List.Assoc.t) =
    List.map ports_to_events ~f:(fun (port, events) ->
      List.map port.mangled_names ~f:(fun name ->
        { Hardcaml.Wave_data.Wave.name
        ; width = Hardcaml.Signal.width port.base_signal
        ; typ
        ; wave_format = Hardcaml.Signal.Type.get_wave_format port.base_signal
        ; is_pseudo_clock = false
        ; wave_data = events
        }))
    |> List.concat
  ;;

  let create_process port events ~max_time =
    let create_process_skip_first_run port ~f =
      let is_initial_run = ref true in
      Sim.Process.create
        ~here:[%here]
        [ Sim.Signal.id port.Port.signal ]
        (fun () -> if !is_initial_run then is_initial_run := false else f ())
    in
    create_process_skip_first_run port ~f:(fun () ->
      let time = Sim.Async.current_time () in
      let data = Sim.Signal.read port.Port.signal |> Logic.to_bits_exn in
      Events.Event_store.insert events time data;
      max_time := Int.max time !max_time)
  ;;

  let create ~inputs ~outputs ~internal =
    (* We track the max event time across all signals so they display consistently *)
    let max_time = ref 0 in
    let processes_and_events (signals_to_trace : Logic.t Port.t list) =
      let t =
        List.map signals_to_trace ~f:(fun port ->
          let events = Events.create (Hardcaml.Signal.width port.base_signal) max_time in
          (* The waveform viewer requires an event at index 0 or will fail. The simulator
             can filter events until a change and may start recoding after time 0. *)
          Events.Event_store.insert
            (Events.event_store events)
            0
            (Hardcaml.Bits.zero (Hardcaml.Signal.width port.base_signal));
          let process = create_process port (Events.event_store events) ~max_time in
          port, events, process)
      in
      let processes = List.map t ~f:(fun (_, _, process) -> process) in
      let ports_to_events = List.map t ~f:(fun (port, event, _) -> port, event) in
      processes, ports_to_events
    in
    let input_processes, input_ports_to_events = processes_and_events inputs in
    let output_processes, output_ports_to_events = processes_and_events outputs in
    let internal_processes, internal_ports_to_events = processes_and_events internal in
    { processes = List.concat [ input_processes; output_processes; internal_processes ]
    ; waveform =
        List.concat
          [ create_waves Input input_ports_to_events
          ; create_waves Output output_ports_to_events
          ; create_waves Internal internal_ports_to_events
          ]
        |> Array.of_list
        |> Hardcaml.Wave_data.By_event
    }
  ;;
end
