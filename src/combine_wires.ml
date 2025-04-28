open! Core
open! Hardcaml

module Copied_signals = struct
  type t =
    { new_signals : Signal.t list
    ; old_signal_to_new_signal : Signal.t Map.M(Signal.Type.Uid).t
    }
end

let expecting_a_wire signal =
  raise_s [%message "expecting a wire (internal error)" (signal : Signal.t)]
;;

let not_expecting_a_wire signal =
  raise_s [%message "not expecting a wire (internal error)" (signal : Signal.t)]
;;

let assign_fresh_name () =
  let next_id = ref 0 in
  fun signal ->
    let id = !next_id in
    incr next_id;
    Signal.( -- ) signal [%string "__%{id#Int}"]
;;

let combine signals =
  let open Signal in
  let fresh_signal_id =
    let fresh_id =
      let `New new_id, _ = Signal.Type.Uid.generator () in
      new_id
    in
    fun (signal_id : Signal.Type.signal_id) : Signal.Type.signal_id ->
      { s_id = fresh_id (); s_width = signal_id.s_width; s_metadata = None }
  in
  let assign_fresh_name = assign_fresh_name () in
  let new_signal_by_old_uid = Hashtbl.create (module Signal.Type.Uid) in
  let add_mapping ~old_signal ~new_signal =
    Hashtbl.add_exn new_signal_by_old_uid ~key:(uid old_signal) ~data:new_signal
  in
  (* create unattached wires *)
  let wires_to_rewrite =
    let wires_to_rewrite = ref [] in
    let wire_uid_to_base_wire = Hashtbl.create (module Type.Uid) in
    let rec get_base_wire ~(signal_id : Signal.Type.signal_id) ~(driver : Signal.t option)
      =
      match Hashtbl.find wire_uid_to_base_wire signal_id.s_id with
      | Some base_wire -> base_wire
      | None ->
        let base_wire =
          match driver with
          | Some (Wire { signal_id = inner_signal_id; driver }) ->
            get_base_wire ~signal_id:inner_signal_id ~driver
          | maybe_signal ->
            let wire =
              Signal.Type.Wire { signal_id = fresh_signal_id signal_id; driver = None }
              |> assign_fresh_name
            in
            (* This is a base wire. Its driver will need to be rewritten to whatever the
               new signal of [maybe_signal] becomes. *)
            wires_to_rewrite
            := (`Unassigned_wire wire, `Old_driver maybe_signal) :: !wires_to_rewrite;
            wire
        in
        Hashtbl.add_exn wire_uid_to_base_wire ~key:signal_id.s_id ~data:base_wire;
        base_wire
    in
    let old_wires = Signal_graph.create signals |> Signal_graph.filter ~f:Type.is_wire in
    (* Add a mapping for every old wire to its new base wire. *)
    List.iter old_wires ~f:(fun old_wire ->
      add_mapping
        ~old_signal:old_wire
        ~new_signal:
          (match old_wire with
           | Wire { signal_id; driver } -> get_base_wire ~signal_id ~driver
           | _ -> expecting_a_wire old_wire));
    !wires_to_rewrite
  in
  (* rewrite from every wire and the input signals *)
  let () =
    let rec rewrite_signal_upto_wires signal ~seen_uids =
      let uid = uid signal in
      match Hashtbl.find new_signal_by_old_uid uid with
      | Some x -> x
      | None ->
        (match Set.mem seen_uids uid with
         | true ->
           raise_s
             [%message
               "Encountered a loop when rewriting signals"
                 (seen_uids : Set.M(Type.Uid).t)
                 (uid : Type.Uid.t)]
         | false ->
           let new_signal =
             match signal with
             | Wire _ -> not_expecting_a_wire signal
             | _ ->
               Signal.Type.map_signal_id
                 (Signal.Type.map_dependant
                    signal
                    ~f:(rewrite_signal_upto_wires ~seen_uids:(Set.add seen_uids uid)))
                 ~f:fresh_signal_id
               |> assign_fresh_name
           in
           add_mapping ~old_signal:signal ~new_signal;
           new_signal)
    in
    let rewrite_signal_upto_wires =
      rewrite_signal_upto_wires ~seen_uids:(Set.empty (module Type.Uid))
    in
    let old_wire_drivers =
      List.filter_map wires_to_rewrite ~f:(fun (`Unassigned_wire _, `Old_driver driver) ->
        driver)
    in
    List.iter (old_wire_drivers @ signals) ~f:(fun signal ->
      ignore (rewrite_signal_upto_wires signal : Signal.t))
  in
  let new_signal signal =
    match Hashtbl.find new_signal_by_old_uid (uid signal) with
    | None ->
      raise_s
        [%message "[Combine_wires.combine] failed to rewrite signal" (signal : Signal.t)]
    | Some s -> s
  in
  (* re-attach wires *)
  List.iter
    wires_to_rewrite
    ~f:(fun (`Unassigned_wire new_wire, `Old_driver old_driver) ->
      Option.iter old_driver ~f:(fun old_driver ->
        let new_driver = new_signal old_driver in
        Signal.(new_wire <-- new_driver)));
  let new_signals = List.map signals ~f:new_signal in
  let old_signal_to_new_signal =
    new_signal_by_old_uid |> Hashtbl.to_alist |> Map.of_alist_exn (module Type.Uid)
  in
  { Copied_signals.new_signals; old_signal_to_new_signal }
;;
