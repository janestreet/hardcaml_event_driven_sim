open Base
module Sim = Event_driven_sim.Simulator
module Time = Int

module Data = struct
  include Hardcaml.Bits

  let none = empty
  let merge _ b = b
end

module Events = struct
  module E = Hardcaml_waveterm_event_store.Event_store.Make (Time) (Data)

  type t =
    { t : E.t
    ; width : int
    ; max_time : int ref
    }
  [@@deriving sexp_of]

  let create width max_time = { t = E.create (); width; max_time }
  let length t = !(t.max_time) + 1
  let get t = E.get t.t
  let equal _ _ = false
  let width t = t.width
end

module Wave_port = Hardcaml_waveterm_kernel.Expert.Port
module Wave_port_name = Hardcaml_waveterm_kernel.Expert.Port_name
include Hardcaml_waveterm_kernel.Make (Events)

module Make (Logic : Logic.S) = struct
  type t =
    { processes : Sim.Process.t list
    ; waveform : Waveform.t
    }

  let create_waves (ports_to_events : (Logic.t Port.t, Events.t) List.Assoc.t) =
    let waves =
      List.map ports_to_events ~f:(fun (port, events) ->
        List.map port.mangled_names ~f:(fun name ->
          let width = Hardcaml.Signal.width port.base_signal in
          let wave =
            if width = 1
            then Wave.Binary (name, events)
            else Data (name, events, Binary, Left)
          in
          wave))
      |> List.concat
    in
    let ports =
      List.map ports_to_events ~f:(fun (port, _) ->
        List.map port.mangled_names ~f:(fun name ->
          { Wave_port.type_ = Internal
          ; port_name = Wave_port_name.of_string name
          ; width = Hardcaml.Signal.width port.base_signal
          }))
      |> List.concat
    in
    Waveform.create_from_data ~waves ~ports
  ;;

  let create (signals_to_trace : Logic.t Port.t list) =
    (* We track the max event time across all signals so they display consistently *)
    let max_time = ref 0 in
    let t =
      List.map signals_to_trace ~f:(fun port ->
        let events = Events.create (Hardcaml.Signal.width port.base_signal) max_time in
        (* The waveform viewer requires an event at index 0 or will fail. The simulator
           can filter events until a change and may start recoding after time 0. *)
        Events.E.insert
          events.t
          0
          (Hardcaml.Bits.zero (Hardcaml.Signal.width port.base_signal));
        let process =
          Sim.Async.create_process (fun () ->
            let open Sim.Async in
            let%bind.Deferred () = wait_for_change (Sim.Signal.id port.signal) in
            let time = current_time () in
            let data = Sim.Signal.read port.signal |> Logic.to_bits_exn in
            Events.E.insert events.t time data;
            max_time := Int.max time !max_time;
            Deferred.return ())
        in
        port, events, process)
    in
    let processes = List.map t ~f:(fun (_, _, process) -> process) in
    let ports_to_events = List.map t ~f:(fun (port, event, _) -> port, event) in
    { processes; waveform = create_waves ports_to_events }
  ;;
end
