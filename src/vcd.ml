open Core
module Signal = Hardcaml.Signal
module Vcd = Hardcaml.Vcd
module Sim = Event_driven_sim.Simulator

module Make (Logic : Logic.S) = struct
  type t =
    { chan : Out_channel.t
    ; processes : Sim.Process.t list
    ; changes : (Vcd.Var.t, Logic.t) Hashtbl.t
    }
  [@@deriving fields ~getters]

  let signal_name s =
    match Signal.names s with
    | h :: _ -> h
    | [] -> "__" ^ Signal.Type.Uid.to_string (Signal.uid s)
  ;;

  let create chan (signals_to_trace : Logic.t Port.t list) =
    let gen = Vcd.Var.Generator.create () in
    let changes = Hashtbl.create (module Vcd.Var) in
    let vars =
      List.map signals_to_trace ~f:(fun port ->
        let id = Vcd.Var.Generator.next gen in
        let var =
          Vcd.Var.create
            ~name:(signal_name port.base_signal)
            ~id
            ~width:(Signal.width port.base_signal)
            ()
        in
        let process =
          Sim.Process.create
            [ Sim.Signal.id port.signal ]
            (fun () ->
              let data = Sim.Signal.read port.signal in
              Hashtbl.set changes ~key:var ~data)
        in
        process, var)
    in
    let scopes =
      [ Vcd.Scope.create_auto_hierarchy ~name:"traced" ~vars:(List.map vars ~f:snd) () ]
    in
    Vcd.write_header
      chan
      ~config:{ Vcd.Config.default with version = "hardcaml-evsim" }
      ~scopes;
    { chan; processes = List.map vars ~f:fst; changes }
  ;;

  let attach_to_simulator { chan; changes; _ } sim =
    Sim.Debug.at_end_of_time_step sim (fun () ->
      Vcd.write_time chan (Sim.current_time sim);
      Hashtbl.iteri changes ~f:(fun ~key:var ~data ->
        Vcd.Var.write_string chan var (Logic.to_string data));
      Hashtbl.clear changes)
  ;;
end
