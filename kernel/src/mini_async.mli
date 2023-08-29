open Base

module Deferred : sig
  type 'a t

  val upon : 'a t -> ('a -> unit) -> unit
  val peek : 'a t -> 'a option
  val unit : unit t

  include Monad.S with type 'a t := 'a t
end

module Let_syntax = Deferred.Let_syntax

module Ivar : sig
  type 'a t

  val read : 'a t -> 'a Deferred.t
  val fill : 'a t -> 'a -> unit
  val is_filled : 'a t -> bool
  val create : unit -> 'a t
end
