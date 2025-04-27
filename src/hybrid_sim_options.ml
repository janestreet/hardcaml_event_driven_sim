open! Core

type t =
  { cyclesim_create :
      config:Hardcaml.Cyclesim.Config.t
      -> clock_names:string list
      -> Hardcaml.Circuit.t
      -> Hardcaml.Cyclesim.t_port_list
  }

let default =
  { cyclesim_create =
      (fun ~config ~clock_names:(_ : string list) circuit ->
        Hardcaml.Cyclesim.create ~config circuit)
  }
;;
