open! Core

type t =
  { cyclesim_create :
      config:Hardcaml.Cyclesim.Config.t
      -> clock_names:string list
      -> Hardcaml.Circuit.t
      -> Hardcaml.Cyclesim.t_port_list
  }

val default : t
