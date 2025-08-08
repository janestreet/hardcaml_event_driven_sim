(* Test confirming that simulator behaviour is the same as VHDL one. *)

open Core
open Event_driven_sim

module Bool4 = struct
  type t =
    | True
    | False
    | X (* don't care *)
    | Z (* high impedence *)
  [@@deriving sexp_of, equal ~localize]

  let to_string = function
    | True -> "1"
    | False -> "0"
    | X -> "X"
    | Z -> "Z"
  ;;

  let ( = ) = equal

  let resolve2 x y =
    match x with
    | X -> X
    | Z -> y
    | _ -> if x = y then x else X
  ;;

  let resolve_func ~last_value = function
    | [] -> last_value
    | x :: xs -> List.fold_right xs ~f:resolve2 ~init:x
  ;;

  let resolve_value = `Func resolve_func
  let check_value_compatibility _ = ()
  let initial_value = Z

  let flip = function
    | X -> X
    | Z -> Z
    | True -> False
    | False -> True
  ;;
end

let%expect_test "conflict1.vhd" =
  let open Simulator in
  let sig_value = Signal.create (module Bool4) in
  let open Async in
  let sim =
    Simulator.create
      [ create_process (fun () ->
          let%map () = delay 30 in
          printf !"process1 %{Bool4}\n" !!sig_value;
          sig_value <-- Bool4.True)
      ; create_process (fun () ->
          let%map () = delay 30 in
          printf !"process2 %{Bool4}\n" !!sig_value;
          sig_value <-- Bool4.False)
      ]
  in
  run sim ~time_limit:100;
  [%expect
    {|
    process1 Z
    process2 Z
    process1 X
    process2 X
    process1 X
    process2 X
    |}]
;;

let%expect_test "conflict2.vhd" =
  let open Simulator in
  let sig_value = Signal.create (module Bool4) in
  let open Async in
  let sim =
    create
      [ create_process (fun () ->
          let%map () = delay 30 in
          printf !"process1 %{Bool4}\n" !!sig_value;
          sig_value <-- Bool4.False)
      ; create_process (fun () ->
          let%map () = delay 30 in
          printf !"process2 %{Bool4}\n" !!sig_value;
          sig_value <-- Bool4.False)
      ]
  in
  run sim ~time_limit:100;
  [%expect
    {|
    process1 Z
    process2 Z
    process1 0
    process2 0
    process1 0
    process2 0
    |}]
;;

let%expect_test "wakeup2.vhd" =
  let open Simulator in
  let open Async in
  let sig_value = Signal.create (module Bool4) in
  let sim =
    Simulator.create
      [ Process.create [ !&sig_value ] (fun () ->
          if Bool4.( = ) !!sig_value Bool4.Z then sig_value <-- Bool4.False;
          printf !"%d: P0: value is %{Bool4}\n" (current_time ()) !!sig_value;
          (sig_value <--- Bool4.flip !!sig_value) ~delay:30)
      ; Process.create [ !&sig_value ] (fun () ->
          if Bool4.( = ) !!sig_value Bool4.Z then sig_value <-- Bool4.False;
          printf !"%d: P1: value is %{Bool4}\n" (current_time ()) !!sig_value;
          (sig_value <--- Bool4.flip !!sig_value) ~delay:40)
      ]
  in
  Simulator.run sim ~time_limit:100;
  [%expect
    {|
    0: P0: value is Z
    0: P1: value is Z
    0: P0: value is 0
    0: P1: value is 0
    30: P1: value is X
    30: P0: value is X
    40: P0: value is 1
    40: P1: value is 1
    60: P1: value is X
    60: P0: value is X
    80: P0: value is 0
    80: P1: value is 0
    90: P1: value is X
    90: P0: value is X
    |}]
;;
