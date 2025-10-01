(** Event_driven_simulator is an interface for writing event driven simulators (the main
    user is hardcaml-event-driven-sim). The simulator frameworks' semantics largely
    resembles those of VHDL. *)

open Core

module type Value_S = sig
  type t [@@deriving sexp_of]

  val ( = ) : t -> t -> bool

  (** What to do if there are multiple processes driving the signal?

      `Unresolved - throw an exception `Func - invoke a function with a list of value to
      determine the result *)
  val resolve_value : [ `Unresolved | `Func of last_value:t -> t list -> t ]

  (** Checks if [t] is a correct value for this signal type. *)
  val check_value_compatibility : t -> unit

  (** What value should this signal have before any processes starts driving it? *)
  val initial_value : t
end

module Signal_id : sig
  type t
end

module Signal : sig
  type 'value t

  val sexp_of_t : 'value t -> Sexp.t

  (** Creates a new signal with a given initial value. *)
  val create : (module Value_S with type t = 'value) -> 'value t

  (** Read current value of a signal. *)
  val read : 'value t -> 'value

  (** Read previous value of a signal. *)
  val read_last : 'value t -> 'value

  val id : 'value t -> Signal_id.t
end

module Process : sig
  type t

  val create : here:[%call_pos] -> Signal_id.t list -> (unit -> unit) -> t
end

type t

(** Creates a new simulator running given processes. *)
val create : Process.t list -> t

val current_time : t -> int

(** Advances time to the next time step. *)
val step : t -> unit

(** Advances time a single delta step. *)
val delta_step : t -> unit

(** Advances time until there are no updates scheduled. *)
val stabilise : t -> unit

(** At what time next event in the queue is scheduled? *)
val next_scheduled_time : t -> int option

(** Read current value of a signal (same as Signal.read) *)
val ( !! ) : 't Signal.t -> 't

(** Returns id of a signal. *)
val ( !& ) : 't Signal.t -> Signal_id.t

(** Schedule a change to a signal without any delay. *)

val set : 't Signal.t -> 't -> unit
val ( <-- ) : 't Signal.t -> 't -> unit

(** Schedule a change to a signal after a delay.

    The behaviour of this delay is the same as of VHDL transport delay. *)
val set_after : 't Signal.t -> 't -> delay:int -> unit

val ( <--- ) : 't Signal.t -> 't -> delay:int -> unit

(** Run a simulation until time [time_limit]. *)
val run : t -> time_limit:int -> unit

(** Returns a process that drives a given signal as a clock with a given time between
    transitions. *)
val create_clock
  :  ?initial_delay:int
       (** The offset of the first rising edge of the clock relative to the start of the
           simulation. The default value is [time], so that all clocks start on the
           falling edge. *)
  -> here:[%call_pos]
  -> time:int
  -> toggle:('a -> 'a)
  -> 'a Signal.t
  -> Process.t

module Expert : sig
  (** Schedules a new update.

      This should only be used by the simulation front-end - processes should use
      functions from [O] module. *)
  val schedule_external_set : t -> 'value Signal.t -> 'value -> unit

  (** Schedules a call to function [f].

      This should only be used by the simulation front-end. *)
  val schedule_call : t -> delay:int -> f:(unit -> unit) -> unit
end

module Async : sig
  (** Async-like wrapper around schedule_call and schedule_on_change. *)

  module Let_syntax = Mini_async.Let_syntax.Let_syntax
  module Deferred = Mini_async.Deferred
  module Ivar = Mini_async.Ivar

  (** Create a process that repeatedly run a given function. *)
  val create_process : here:[%call_pos] -> (unit -> unit Deferred.t) -> Process.t

  (** [delay n] returns deferred that will be filled after [n] time steps. *)
  val delay : int -> unit Deferred.t

  (** [wait_for_change sig] returns deferred that will be filled when [sig] changes for
      the first time. *)
  val wait_for_change : Signal_id.t -> unit Deferred.t

  val wait_forever : unit -> unit Deferred.t

  (** Execute given function in an infinite loop. *)
  val forever : (unit -> unit Deferred.t) -> unit Deferred.t

  val current_time : unit -> int
end

module Change_monitor : sig
  type t

  (** Setup a monitor for waiting on a given list of signals. *)
  val create : Signal_id.t list -> t

  (** Wait until any of the signals changes a value. *)
  val wait : t -> unit Async.Deferred.t
end

module Debug : sig
  (** Returns a process that prints given signal whenever it changes. *)
  val print_signal : string -> 't Signal.t -> Process.t

  val at_start_of_time_step : t -> (unit -> unit) -> unit
  val at_end_of_time_step : t -> (unit -> unit) -> unit
end

module Version_signal : sig
  (** A signal that keeps track of the state of some external data. *)

  val create : unit -> int Signal.t
  val increment : int Signal.t -> unit
end
