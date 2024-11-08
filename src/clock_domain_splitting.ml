open! Core
open Hardcaml

(* Override the signal module so that we can create hashtbls and a map based on it's type *)
module Signal = struct
  include Signal

  let hash a = Uid.hash (uid a)
  let compare a b = Uid.compare (uid a) (uid b)

  let sexp_of_t t =
    match names t with
    | [] -> Uid.sexp_of_t (uid t)
    | names -> [%sexp_of: string list] names
  ;;
end

module Clock_spec = struct
  type t =
    { clock : Signal.t
    ; edge : Edge.t
    }
  [@@deriving compare, equal, sexp_of]
end

module Clock_domain_with_any = struct
  type t =
    | Any
    | Clocked of Clock_spec.t
    | Floating
  [@@deriving equal]

  (* values of [t] form a lattice with [Any] as the bottom element, [Floating]
     as the top element, and all [Clocked] values in between. [merge] is the join of 2
     elements in this lattice *)
  let merge t1 t2 =
    match t1, t2 with
    | Any, t | t, Any -> t
    | Clocked c1, Clocked c2 ->
      if [%equal: Clock_spec.t] c1 c2 then Clocked c1 else Floating
    | Floating, _ | _, Floating -> Floating
  ;;
end

(* The 'clock domain deps' of a signal are the deps whose clock domains influence the
   clock domain of the signal. The clock domain of a signal is the join of the signal's
   clock domain and the clock domains of all of its 'clock domain deps'.

   One thing that we have to ensure is that all [Mem_read_ports] for a [Multiport_mem]
   have to be in the same clock domain as the [Multiport_mem]. This is because we can't
   have a [Multiport_mem] signal be the output of a [Cyclesim.t]. The way we accomplish
   this is by adding the read addresses of all read ports of a [Multiport_mem] as
   dependencies of that [Multiport_mem]. These additional edges may cause 'clock domain
   deps' to contain loops for valid circuits[0]. This means we need to run a worklist
   algorithm to determine the clock domain of all the signals.

   [0] Example:
   [M] is a multiport memory, [R1] and [R2] are read ports of [M], and [A] is the read
   address of [R1] and the output of [R1] is the read address of [R2]. This is a valid
   circuit, but if we add the read addresses of [R1] and [R2] as dependencies of [M], then
   we will get a cycle: [M] -> [R1] -> [M].

   {v
            [A]
             |
             V
     [M] -> [R1] ----
      |             |
      ----> [R2]    |
             ^      |
             |      |
             --------
   v}
*)
let create_clock_domain_deps graph =
  let additional_deps = Hashtbl.create (module Signal) in
  Signal_graph.iter graph ~f:(fun signal ->
    match signal with
    | Mem_read_port { read_address; memory; _ } ->
      Hashtbl.add_multi additional_deps ~key:memory ~data:read_address
    | _ -> ());
  let additional_deps =
    Hashtbl.map additional_deps ~f:(Hash_set.of_list (module Signal))
  in
  let module Deps =
    Signal.Type.Make_deps (struct
      let fold (t : Signal.t) ~init ~f =
        let acc =
          match t with
          | Reg _ | Multiport_mem _ -> init
          | Empty
          | Const _
          | Op2 _
          | Mux _
          | Cases _
          | Cat _
          | Not _
          | Wire _
          | Select _
          | Inst _
          | Mem_read_port _ -> Signal.Type.Deps.fold t ~init ~f
        in
        Option.fold
          (Hashtbl.find additional_deps t)
          ~init:acc
          ~f:(fun acc additional_deps ->
            Hash_set.fold additional_deps ~init:acc ~f:(fun acc dep -> f acc dep))
      ;;
    end)
  in
  (module Deps : Signal.Type.Deps)
;;

let clock_domain (signal : Signal.t) : Clock_domain_with_any.t =
  match signal with
  | Reg { register = { clock = { clock; clock_edge }; _ }; _ } ->
    Clocked { clock; edge = clock_edge }
  | Multiport_mem { write_ports; _ } ->
    write_ports
    |> Array.to_list
    |> List.map ~f:(fun { write_clock; _ } ->
      Clock_domain_with_any.Clocked { clock = write_clock; edge = Rising })
    |> List.reduce ~f:Clock_domain_with_any.merge
    |> (* A multiport memory must have at least one write port *)
    Option.value_exn
  | Wire { driver = None; _ } ->
    (* All inputs are floating *)
    Floating
  | Wire { driver = Some _; _ }
  | Op2 _
  | Mux _
  | Cases _
  | Cat _
  | Not _
  | Select _
  | Inst _
  | Mem_read_port _
  | Const _
  | Empty ->
    (* All other signals just inherit the signals of their dependencies *)
    Any
;;

let assert_no_stateful_signals_in_the_any_clock_domain
  (clock_domain_by_signal : (Signal.t, Clock_domain_with_any.t) Hashtbl.t)
  =
  Hashtbl.iteri clock_domain_by_signal ~f:(fun ~key:signal ~data:clock_domain ->
    if [%equal: Clock_domain_with_any.t] clock_domain Any
    then (
      match signal with
      | Reg _ | Multiport_mem _ ->
        raise_s [%message "BUG: Signal is stateful but in Any clock domain"]
      | Wire _
      | Op2 _
      | Mux _
      | Cases _
      | Cat _
      | Not _
      | Select _
      | Inst _
      | Mem_read_port _
      | Const _
      | Empty -> ()))
;;

let get_clock_domain_of_signal (circuit : Hardcaml.Circuit.t) =
  let graph = Circuit.signal_graph circuit in
  let (module Clock_domain_deps) = create_clock_domain_deps graph in
  (* Data structures for worklist algorithm *)
  let clock_domain_by_signal = Hashtbl.create (module Signal) in
  let worklist = Hash_queue.create (Hashtbl.Hashable.of_key (module Signal)) in
  let signal_to_dependents = Hashtbl.create (module Signal) in
  (* Initialize worklist algorithm datastructures *)
  Signal_graph.iter graph ~f:(fun signal ->
    Hashtbl.set clock_domain_by_signal ~key:signal ~data:Clock_domain_with_any.Any;
    let (`Key_already_present | `Ok) = Hash_queue.enqueue_back worklist signal () in
    Clock_domain_deps.iter signal ~f:(fun dependency ->
      let dependents =
        Hashtbl.find_or_add signal_to_dependents dependency ~default:(fun () ->
          Hash_set.create (module Signal))
      in
      Hash_set.add dependents signal));
  let rec process_worklist () =
    match Hash_queue.dequeue_front_with_key worklist with
    | None -> ()
    | Some (signal, ()) ->
      let current_clock_domain = Hashtbl.find_exn clock_domain_by_signal signal in
      let new_clock_domain =
        Clock_domain_deps.fold
          signal
          ~init:(clock_domain signal)
          ~f:(fun clock_domain dependency ->
            let clock_domain_of_dependency =
              Hashtbl.find_exn clock_domain_by_signal dependency
            in
            Clock_domain_with_any.merge clock_domain clock_domain_of_dependency)
      in
      if [%equal: Clock_domain_with_any.t] current_clock_domain new_clock_domain
      then ()
      else (
        Hashtbl.set clock_domain_by_signal ~key:signal ~data:new_clock_domain;
        Hashtbl.find signal_to_dependents signal
        |> Option.iter
             ~f:
               (Hash_set.iter ~f:(fun dependent ->
                  let (`Key_already_present | `Ok) =
                    Hash_queue.enqueue_back worklist dependent ()
                  in
                  ())));
      process_worklist ()
  in
  (* The worklist algorithm is guaranteed to terminate by the following argument:

     - When a signal is removed from the worklist, the clock domain of the signal either
       stays the same or it moves up the lattice defined by [merge].
     - Once the clock domain is [Floating], the clock domain of a signal no longer
       changes.
     - Because the height of the lattice is 3, the maximum number of times a signal can
       change its clock domain is 2.
     - A signal is only added to the worklist (after everything is initially added) if the
       clock domain of one of its dependencies changes.
     - Therefore, a signal can only be added to the worklist algorithm a max of 1+2*#deps
       times.
     - Therefore, the algorithm terminates and processes each signal at most 1+2*#deps times.

     Note: I think in practice, the algorithm will only add each signal at most 3 times,
     because it processes all the dependencies of a signal before processing the signal
     because we are using a queue for the worklist.
  *)
  process_worklist ();
  assert_no_stateful_signals_in_the_any_clock_domain clock_domain_by_signal;
  clock_domain_by_signal
;;

module Clock_domain = struct
  module T = struct
    type t =
      | Clocked of Clock_spec.t
      | Floating
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let of_clock_domain_with_any_opt (clock_domain_with_any : Clock_domain_with_any.t) =
    match clock_domain_with_any with
    | Any -> None
    | Clocked clock_spec -> Some (Clocked clock_spec)
    | Floating -> Some Floating
  ;;
end

module Signal_graph_creator = struct
  (* This type accumulates the uptos and outputs of a signal graph, and can then be
     'finalized' to a signal graph *)
  type t =
    { outputs : Signal.t list
    ; upto : Signal.t list
    }

  let empty = { outputs = []; upto = [] }
  let add_output t output = { t with outputs = output :: t.outputs }
  let add_upto t upto = { t with upto = upto :: t.upto }

  let to_signal_graph { outputs; upto } =
    let dedup_signals signals =
      Hash_set.to_list (Hash_set.of_list (module Signal) signals)
    in
    Signal_graph.create ~upto:(dedup_signals upto) (dedup_signals outputs)
  ;;
end

let clock_domain_signal_graphs_of_clock_domain_by_signal clock_domain_by_signal ~outputs =
  let clock_domain_by_signal =
    Hashtbl.map clock_domain_by_signal ~f:Clock_domain.of_clock_domain_with_any_opt
  in
  let update_signal_graph clock_domain_signal_graphs clock_domain ~f =
    Map.update clock_domain_signal_graphs clock_domain ~f:(fun existing ->
      let existing = Option.value existing ~default:Signal_graph_creator.empty in
      f existing)
  in
  (* Put all of the outputs into a clock domain *)
  let clock_domain_signal_graphs =
    List.fold
      outputs
      ~init:Clock_domain.Map.empty
      ~f:(fun clock_domain_signal_graphs output ->
        let output_clock_domain = Hashtbl.find_exn clock_domain_by_signal output in
        let output_clock_domain =
          match output_clock_domain with
          | None ->
            (* In this case, the output is only connected to constant values. So we just
               put it in the Floating clock domain *)
            Clock_domain.Floating
          | Some output_clock_domain -> output_clock_domain
        in
        update_signal_graph
          clock_domain_signal_graphs
          output_clock_domain
          ~f:(Fn.flip Signal_graph_creator.add_output output))
  in
  (* For each signal, see if it is on the boundary of a clock domain by looking at the
     clock domains of its dependants *)
  let signal_graph_creators_by_clock_domain =
    Hashtbl.fold
      clock_domain_by_signal
      ~init:clock_domain_signal_graphs
      ~f:(fun ~key:signal ~data:clock_domain clock_domain_signal_graphs ->
        Signal.Type.Deps.fold
          signal
          ~init:clock_domain_signal_graphs
          ~f:(fun clock_domain_signal_graphs dep ->
            let dep_clock_domain = Hashtbl.find_exn clock_domain_by_signal dep in
            match dep_clock_domain with
            | None ->
              (* If the dependency has no clock domain, then we will treat that dependency
               as part of our clock domain by not adding it to our upto's *)
              clock_domain_signal_graphs
            | Some dep_clock_domain ->
              (match clock_domain with
               | None ->
                 raise_s
                   [%message
                     "BUG: Crossed from no clock domain to a clock domain. This should \
                      be impossible"
                       (signal : Signal.t)
                       (dep : Signal.t)
                       (dep_clock_domain : Clock_domain.t)]
               | Some clock_domain ->
                 if [%equal: Clock_domain.t] clock_domain dep_clock_domain
                 then clock_domain_signal_graphs
                 else (
                   (* Found a clock domain boundary: [dep] is an upto of [signal]'s clock
                    domain and [dep] is output of [dep]'s clock domain *)
                   let clock_domain_signal_graphs =
                     update_signal_graph
                       clock_domain_signal_graphs
                       clock_domain
                       ~f:(Fn.flip Signal_graph_creator.add_upto dep)
                   in
                   update_signal_graph
                     clock_domain_signal_graphs
                     dep_clock_domain
                     ~f:(Fn.flip Signal_graph_creator.add_output dep)))))
  in
  Map.map signal_graph_creators_by_clock_domain ~f:Signal_graph_creator.to_signal_graph
;;

(* This function takes a circuit, creates a mapping from each signal in the circuit to the
   clock domain of that signal, and then creates signal graphs for each clock domain. A
   signal graph can be interpreted as a subset of a circuit (all of the signals from the
   outputs of the signal graph up to but not including the uptos). Each clock domain is a
   subset of the original circuit. There may be some signals shared between clock domains,
   particularly constants. This is fine as long as those signals are purely combinational
   signals (i.e. not stateful signals).
*)
let group_by_clock_domain circuit =
  let outputs = Circuit.outputs circuit in
  let clock_domain_by_signal = get_clock_domain_of_signal circuit in
  let signal_graphs_by_clock_domain =
    clock_domain_signal_graphs_of_clock_domain_by_signal clock_domain_by_signal ~outputs
  in
  signal_graphs_by_clock_domain
;;
