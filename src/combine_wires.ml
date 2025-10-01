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

(* The basic idea is to rewrite the circuit represented by [signals] by collapsing all
   chains of wires down to a single wire.

   At first glance, a possible solution would have been to just copy the circuit and
   rewrite all the drivers of the wires in the copied circuit to the "base" driver of a
   chain of wires. However, this doesn't work for some subtle reasons.

   Firstly, we need to consider the case of an output signal that is directly connected to
   an input signal. An output signal must have a driver, and naively compressing an output
   wire like this would result in an output with a [None] driver.

   Secondly, we need to be careful to not create new inputs. This is because we also
   return a map mapping each old signal to it's new signal. We can only map one old signal
   to one new signal. If we split a single input into multiple inputs because we compress
   2 wires that were connected to the same input down to [None], then we would need to map
   the old input to the 2 new input wires (if we just chose one new input to map to, the
   other input wouldn't get updated.)


   Thirdly, if 2 wires are driven by the same base signal but both wires are used as
   inputs to different signals, we will end up with more wires than we need to. Consider
   the following set of signals, where S represents some other signal node, and W
   represents a wire:

   {v
     ____         ____       ____       ____
    |    |       |    |     |    |     |    |
    | S1 | ----->| W1 | --->| W2 | --->| S3 |
    |    |       |    |     |    |     |    |
    ------       ------     ------     ------
                   |
                   v
                 ____
                |    |
                | S2 |
                |    |
                ------
   v}

   If we just compress W1 and W2 to be driven by S1 we end up with this arrangement of
   signals:

   {v
     ____         ____       ____
    |    |       |    |     |    |
    | S1 | ----->| W1 | --->| S2 |
    |    |       |    |     |    |
    ------       ------     ------
      |
      v
     ____        ____
    |    |      |    |
    | W2 | ---->| S3 |
    |    |      |    |
    ------      ------
   v}

   However, the following arrangement of signals is better because if W1 and W2 are clock
   inputs to S2 and S3, then S2 and S3 will be in the same clock domain (in the previous
   configuration they would be considered to be in different clock domains):

   {v
     ____         ____       ____
    |    |       |    |     |    |
    | S1 | ----->| W1 | --->| S2 |
    |    |       |    |     |    |
    ------       ------     ------
                   |
                   v
                  ____
                 |    |
                 | S3 |
                 |    |
                 ------
   v}


   So we need a slightly more clever algorithm that first finds the "base wire" of all
   chains of wires and then rewrites every signal which has a wire as an input to use the
   "base wire" instead. This is what the algorithm below does.
*)
let combine signals =
  let open Signal in
  let fresh_signal_info =
    let fresh_id =
      let `New new_id, _ = Type.Uid.generator () in
      new_id
    in
    fun (info : Signal.Type.Info.t) : Signal.Type.Info.t ->
      { uid = fresh_id (); width = info.width; metadata = None }
  in
  let assign_fresh_name = assign_fresh_name () in
  let new_signal_by_old_uid = Hashtbl.create (module Type.Uid) in
  let add_mapping ~old_signal ~new_signal =
    Hashtbl.add_exn new_signal_by_old_uid ~key:(uid old_signal) ~data:new_signal
  in
  (* create unattached wires *)
  let wires_to_rewrite =
    (* Every chain of wires in the original circuit (even singleton chains of wires) will
       get mapped to a single 'base wire' in the new circuit. [wire_uid_to_base_wire]
       stores this mapping *)
    let wire_uid_to_base_wire = Hashtbl.create (module Type.Uid) in
    (* This list keeps track of all of the 'base wires' in the new circuit. It also stores
       the original driver signal of the chain of wires represented by each 'base wire'.
       We will lookup the original driver signal in the new circuit and attach that new
       signal to the 'base wire' in the new circuit. *)
    let wires_to_rewrite = ref [] in
    let rec get_base_wire ~(signal_info : Signal.Type.Info.t) ~(driver : Signal.t option) =
      match Hashtbl.find wire_uid_to_base_wire signal_info.uid with
      | Some base_wire -> base_wire
      | None ->
        let base_wire =
          match driver with
          | Some (Wire { info = inner_signal_info; driver }) ->
            get_base_wire ~signal_info:inner_signal_info ~driver
          | maybe_signal ->
            let wire =
              Signal.Type.Wire { info = fresh_signal_info signal_info; driver = None }
              |> assign_fresh_name
            in
            (* This is a base wire. Its driver will need to be rewritten to whatever the
               new signal of [maybe_signal] becomes. *)
            wires_to_rewrite
            := (`Unassigned_wire wire, `Old_driver maybe_signal) :: !wires_to_rewrite;
            wire
        in
        Hashtbl.add_exn wire_uid_to_base_wire ~key:signal_info.uid ~data:base_wire;
        base_wire
    in
    let old_wires = Signal_graph.create signals |> Signal_graph.filter ~f:Type.is_wire in
    (* Add a mapping for every old wire to its new base wire. *)
    List.iter old_wires ~f:(fun old_wire ->
      add_mapping
        ~old_signal:old_wire
        ~new_signal:
          (match old_wire with
           | Wire { info = signal_info; driver } -> get_base_wire ~signal_info ~driver
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
               Signal.Type.map_info
                 (Signal.Type.map_dependant
                    signal
                    ~f:(rewrite_signal_upto_wires ~seen_uids:(Set.add seen_uids uid)))
                 ~f:fresh_signal_info
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
