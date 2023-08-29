(** Four valued logic. (1, 0, X, Z) *)

open! Core
include Logic.S

val to_bits_exn : t -> Hardcaml.Bits.t
val ( = ) : t -> t -> bool
val don't_care : int -> t
val high_impedance : int -> t
val resolve2 : t -> t -> t
