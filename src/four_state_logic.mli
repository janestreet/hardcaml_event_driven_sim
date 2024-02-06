(** Four valued logic. (1, 0, X, Z) *)

open! Core
include Logic.S

val ( = ) : t -> t -> bool
val don't_care : int -> t
val high_impedance : int -> t
val resolve2 : t -> t -> t
