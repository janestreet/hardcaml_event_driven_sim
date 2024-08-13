open Core

module type Value_S = sig
  type t [@@deriving sexp_of]

  val ( = ) : t -> t -> bool
  val resolve_value : [ `Unresolved | `Func of last_value:t -> t list -> t ]

  (** Checks if [t] is a correct value for this signal type. *)
  val check_value_compatibility : t -> unit

  val initial_value : t
end

module Delta_step = struct
  type t =
    { time : int
    ; delta : int
    }
  [@@deriving equal, fields ~getters, sexp_of]

  let zero = { time = 0; delta = 0 }
  let before_zero = { time = 0; delta = -1 }
end

module Process_id = struct
  type t = int [@@deriving hash, compare, sexp]

  let empty = -1
end

module Signal_id = struct
  (* Signal_id is only used in sensitivity lists - we only need to be able to modify
     on_change list. *)
  type t = { add_on_change : (unit -> unit) -> unit } [@@deriving fields ~getters]
end

module Signal = struct
  type 'value t =
    { m : (module Value_S with type t = 'value)
    ; values : (Process_id.t, 'value) Hashtbl.t (* in case of resolved signal *)
    ; mutable current_value : 'value
    ; mutable last_value : 'value
    ; mutable last_change : Delta_step.t
    ; mutable scheduled_invoke_callbacks : bool
    ; mutable on_change : (unit -> unit) list
    ; (* optimization for unresolved signals: *)
      mutable writer_process : Process_id.t
    }
  [@@deriving fields ~getters]

  let sexp_of_current_value (type value) (t : value t) =
    let (module Value : Value_S with type t = value) = t.m in
    Value.sexp_of_t t.current_value
  ;;

  let sexp_of_t t =
    Sexp.List
      [ Sexp.Atom "signal"
      ; Sexp.List [ Sexp.Atom "current_value"; sexp_of_current_value t ]
      ]
  ;;

  let read = current_value
  let read_last = last_value

  let create (type value) m =
    let (module Value : Value_S with type t = value) = m in
    { m
    ; values = Hashtbl.create (module Process_id)
    ; writer_process = Process_id.empty
    ; current_value = Value.initial_value
    ; last_value = Value.initial_value
    ; last_change = Delta_step.before_zero
    ; scheduled_invoke_callbacks = false
    ; on_change = []
    }
  ;;

  let schedule_on_change t ~f = t.on_change <- f :: t.on_change
  let id t = { Signal_id.add_on_change = (fun f -> schedule_on_change t ~f) }
end

module Scheduled_event = struct
  type t =
    { time : int
    ; sequential_id : int
    ; f : unit -> unit
    }
  [@@deriving sexp_of, fields ~getters]

  let compare a b =
    Comparable.lift [%compare: int * int] ~f:(fun t -> t.time, t.sequential_id) a b
  ;;
end

type simulation_callbacks =
  { mutable at_start_of_time_step : (unit -> unit) list
  ; mutable at_end_of_time_step : (unit -> unit) list
  }

(* Although changes to [delta_updates] never interleave read / write, storing
   with a [Queue.t] is probably faster than storing it as a [list] since
   [Queue.t] is implemented using an array under the hood and saves
   unnecessary allocations. This is an untested claim, though.
*)
type t =
  { mutable delta_updates : Scheduled_event.t Queue.t
      (* keep second queue to avoid allocating every delta step *)
  ; mutable delta_updates_now : Scheduled_event.t Queue.t
  ; updates : Scheduled_event.t Pairing_heap.t
  ; mutable current_step : Delta_step.t
  ; mutable current_sequential_id : int
  ; simulation_callbacks : simulation_callbacks
  }
[@@deriving fields ~getters]

let schedule_call t ~delay ~f =
  let scheduled_update =
    { Scheduled_event.f
    ; time = delay + t.current_step.time
    ; sequential_id = t.current_sequential_id
    }
  in
  t.current_sequential_id <- t.current_sequential_id + 1;
  match Int.sign delay with
  | Sign.Neg -> failwith "negative delay"
  | Sign.Zero -> Queue.enqueue t.delta_updates scheduled_update
  | Sign.Pos -> Pairing_heap.add t.updates scheduled_update
;;

let invoke_on_change_callbacks (type value) (signal : value Signal.t) =
  let (module Value : Value_S with type t = value) = signal.m in
  if not (Value.( = ) signal.current_value signal.last_value)
  then (
    let on_change_callbacks = signal.on_change in
    signal.on_change <- [];
    List.iter on_change_callbacks ~f:(fun f -> f ()))
;;

let maybe_invoke_on_change_callbacks (type value) (signal : value Signal.t) =
  if signal.scheduled_invoke_callbacks
  then (
    signal.scheduled_invoke_callbacks <- false;
    invoke_on_change_callbacks signal)
;;

let apply_update
  (type value)
  simulator
  (signal : value Signal.t)
  (value : value)
  ~process_id
  =
  let (module Value : Value_S with type t = value) = signal.m in
  let new_current_value =
    match Value.resolve_value with
    | `Unresolved ->
      if signal.writer_process = Process_id.empty
      then signal.writer_process <- process_id
      else if signal.writer_process <> process_id
      then failwith "two processes attempt to drive an unresolved signal";
      value
    | `Func resolve_func ->
      Hashtbl.set signal.values ~key:process_id ~data:value;
      resolve_func ~last_value:signal.current_value (Hashtbl.data signal.values)
  in
  if not (Delta_step.equal signal.last_change simulator.current_step)
  then (
    (* first update in delta step *)
    maybe_invoke_on_change_callbacks signal;
    signal.scheduled_invoke_callbacks <- true;
    schedule_call simulator ~delay:0 ~f:(fun () ->
      maybe_invoke_on_change_callbacks signal);
    signal.last_change <- simulator.current_step;
    signal.last_value <- signal.current_value);
  signal.current_value <- new_current_value
;;

let[@inline] schedule_update
  (type value)
  t
  (signal : value Signal.t)
  (value : value)
  ~delay
  ~process_id
  =
  let (module Value : Value_S with type t = value) = Signal.m signal in
  Value.check_value_compatibility value;
  schedule_call t ~delay ~f:(fun () -> apply_update t signal value ~process_id)
;;

let schedule_external_set t signal value =
  schedule_update ~process_id:Process_id.empty ~delay:0 t signal value
;;

let rec progress_time_to t new_time =
  match Pairing_heap.top t.updates with
  | None -> ()
  | Some next_update ->
    if Int.( = ) (Scheduled_event.time next_update) new_time
    then (
      let new_update = Pairing_heap.pop_exn t.updates in
      Queue.enqueue t.delta_updates new_update;
      progress_time_to t new_time)
;;

let progress_time t =
  (* Move updates for a next time step from [updates] heap to [delta_update] queue
     and bump current_step. *)
  assert (Queue.length t.delta_updates = 0);
  match Pairing_heap.top t.updates with
  | None -> ()
  | Some next_update ->
    let new_time = Scheduled_event.time next_update in
    t.current_step <- { Delta_step.time = new_time; delta = 0 };
    progress_time_to t new_time
;;

let current_time t = Delta_step.time (current_step t)

module Global_state = struct
  let current_process_id : Process_id.t option ref = ref None
  let current_simulator : t option ref = ref None

  let get_current_simulator () =
    match !current_simulator with
    | None -> failwith "no simulation is currently running"
    | Some sim -> sim
  ;;

  let get_current_process_id () =
    match !current_process_id with
    | None -> failwith "no process is currently running"
    | Some id -> id
  ;;
end

let ( !! ) = Signal.read
let ( !& ) = Signal.id

let[@inline] set_after signal value ~delay =
  schedule_update
    (Global_state.get_current_simulator ())
    ~process_id:(Global_state.get_current_process_id ())
    signal
    value
    ~delay
;;

let[@inline] set signal value = set_after ~delay:0 signal value
let ( <--- ) = set_after
let ( <-- ) = set

module Async = struct
  module Let_syntax = Mini_async.Let_syntax.Let_syntax
  module Deferred = Mini_async.Deferred
  module Ivar = Mini_async.Ivar

  let preserve_process_id deferred =
    let current_process_id = !Global_state.current_process_id in
    Global_state.current_process_id := None;
    Deferred.map deferred ~f:(fun x ->
      Global_state.current_process_id := current_process_id;
      x)
  ;;

  let delay time_steps =
    let v = Ivar.create () in
    schedule_call
      (Global_state.get_current_simulator ())
      ~delay:time_steps
      ~f:(Ivar.fill v);
    preserve_process_id (Ivar.read v)
  ;;

  let current_time () = current_time (Global_state.get_current_simulator ())

  let wait_for_change signal_id =
    let v = Ivar.create () in
    Signal_id.add_on_change signal_id (fun () ->
      schedule_call (Global_state.get_current_simulator ()) ~delay:0 ~f:(Ivar.fill v));
    preserve_process_id (Ivar.read v)
  ;;

  let wait_forever () =
    (* Create a deferred that is never filled.  This will never return. *)
    let v = Ivar.create () in
    preserve_process_id (Ivar.read v)
  ;;

  let create_process f =
    let rec run () = Deferred.upon (f ()) run in
    run
  ;;

  let rec forever_helper f = Deferred.upon (f ()) (fun () -> forever_helper f)

  let forever f =
    forever_helper f;
    Ivar.read (Ivar.create ())
  ;;
end

module Change_monitor = struct
  (* Module responsible for waiting until a signal changes its value. *)
  open Async

  type t =
    { mutable is_scheduled : bool
    ; mutable callback : unit -> unit
    ; mutable process_id : Process_id.t
    ; mutable fill_wait_ivar : unit -> unit
    }

  let fill_wait_ivar t () =
    t.is_scheduled <- false;
    Global_state.current_process_id := Some t.process_id;
    t.callback ()
  ;;

  let rec changed t signal_id () =
    if not t.is_scheduled
    then (
      t.is_scheduled <- true;
      schedule_call (Global_state.get_current_simulator ()) ~f:t.fill_wait_ivar ~delay:0);
    Signal_id.add_on_change signal_id (changed t signal_id)
  ;;

  let create signals =
    let t =
      { is_scheduled = false
      ; callback = (fun () -> ())
      ; process_id = Process_id.empty
      ; fill_wait_ivar = (fun () -> failwith "BUG")
      }
    in
    (* optimization to avoid allocating closure *)
    t.fill_wait_ivar <- fill_wait_ivar t;
    List.iter signals ~f:(fun signal_id ->
      Signal_id.add_on_change signal_id (changed t signal_id));
    t
  ;;

  let wait_with_callback t cb =
    t.process_id <- Global_state.get_current_process_id ();
    t.callback <- cb
  ;;

  let wait t =
    let ivar = Ivar.create () in
    wait_with_callback t (Ivar.fill ivar);
    Ivar.read ivar
  ;;
end

module Process = struct
  type t = unit -> unit

  let create sensitivity_list update_f =
    let change_monitor = Change_monitor.create sensitivity_list in
    let rec run () =
      update_f ();
      Change_monitor.wait_with_callback change_monitor run
    in
    run
  ;;
end

let create processes =
  let t =
    { delta_updates = Queue.create ()
    ; delta_updates_now = Queue.create ()
    ; updates = Pairing_heap.create ~cmp:Scheduled_event.compare ()
    ; current_step = Delta_step.zero
    ; current_sequential_id = 1
    ; simulation_callbacks = { at_start_of_time_step = []; at_end_of_time_step = [] }
    }
  in
  (* run initial iteration of all processes *)
  List.iteri processes ~f:(fun process_id process_f ->
    Global_state.current_simulator := Some t;
    Global_state.current_process_id := Some process_id;
    process_f ();
    Global_state.current_simulator := None;
    Global_state.current_process_id := None);
  t
;;

let delta_step t =
  Global_state.current_simulator := Some t;
  t.current_step
  <- { t.current_step with Delta_step.delta = Delta_step.delta t.current_step + 1 };
  let updates = t.delta_updates in
  t.delta_updates <- t.delta_updates_now;
  t.delta_updates_now <- updates;
  (* run scheduled functions - updates signals and runs async delayed deferreds *)
  Queue.iter updates ~f:(fun event -> Scheduled_event.f event ());
  Queue.clear updates;
  (* run triggered processes - based on their sensitivity lists *)
  Global_state.current_process_id := None;
  Global_state.current_simulator := None
;;

let next_scheduled_time t =
  if Queue.length t.delta_updates <> 0
  then Some (current_time t)
  else (
    match Pairing_heap.top t.updates with
    | None -> None
    | Some next_update -> Some (Scheduled_event.time next_update))
;;

let rec step t =
  delta_step t;
  if Queue.length t.delta_updates = 0
  then (
    List.iter t.simulation_callbacks.at_end_of_time_step ~f:(fun f -> f ());
    progress_time t)
  else step t
;;

let step t =
  List.iter t.simulation_callbacks.at_start_of_time_step ~f:(fun f -> f ());
  step t
;;

let rec stabilise t =
  step t;
  if Queue.length t.delta_updates <> 0 then stabilise t
;;

let rec run t ~time_limit =
  step t;
  if current_time t < time_limit && Queue.length t.delta_updates <> 0
  then run t ~time_limit
;;

module Debug = struct
  let print_signal name signal =
    let open Async in
    create_process (fun () ->
      let%map () = wait_for_change (Signal.id signal) in
      printf
        "t=%d %s=%s\n"
        (current_time ())
        name
        (Sexp.to_string (Signal.sexp_of_current_value signal)))
  ;;

  let at_start_of_time_step t f =
    t.simulation_callbacks.at_start_of_time_step
    <- f :: t.simulation_callbacks.at_start_of_time_step
  ;;

  let at_end_of_time_step t f =
    t.simulation_callbacks.at_end_of_time_step
    <- f :: t.simulation_callbacks.at_end_of_time_step
  ;;
end

module Version_signal = struct
  module Version_value = struct
    type t = int [@@deriving sexp_of]

    let ( = ) = Int.( = )
    let resolve_value = `Func (fun ~last_value:_ xs -> List.fold xs ~init:0 ~f:Int.max)
    let check_value_compatibility _ = ()
    let initial_value = 0
  end

  let create () = Signal.create (module Version_value)
  let increment signal = signal <-- !!signal + 1
end

module Expert = struct
  let schedule_call = schedule_call
  let schedule_external_set = schedule_external_set
end
