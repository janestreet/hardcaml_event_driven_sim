open Core
open Event_driven_sim

let undefined_value = -1000

module Value = struct
  type t = int [@@deriving sexp_of]

  let ( = ) = Int.( = )

  let resolve_func ~last_value x =
    let x = List.stable_dedup ~compare:Int.compare x in
    match x with
    | [] -> last_value
    | [ x' ] -> x'
    | _ -> undefined_value
  ;;

  let resolve_value = `Func resolve_func
  let initial_value = undefined_value
  let check_value_compatibility _ = ()
end

module Value_default_zero = struct
  type t = int [@@deriving sexp_of]

  let ( = ) = Int.( = )
  let resolve_value = Value.resolve_value
  let initial_value = 0
  let check_value_compatibility _ = ()
end

let%expect_test "trivial" =
  let open Simulator in
  let sig_a = Signal.create (module Value) in
  let sig_b = Signal.create (module Value) in
  let sig_c = Signal.create (module Value) in
  let sim =
    create [ Process.create [ !&sig_a; !&sig_b ] (fun () -> sig_c <-- !!sig_a + !!sig_b) ]
  in
  printf "%d %d %d\n" !!sig_a !!sig_b !!sig_c;
  [%expect {| -1000 -1000 -1000 |}];
  Expert.schedule_external_set sim sig_a 1;
  Expert.schedule_external_set sim sig_b 2;
  step sim;
  printf "%d %d %d\n" !!sig_a !!sig_b !!sig_c;
  [%expect {| 1 2 3 |}]
;;

let%expect_test "simple_clock" =
  let open Simulator in
  let sig_t = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ Process.create [ !&sig_t ] (fun () ->
          let v = !!sig_t in
          (sig_t <--- v + 1) ~delay:1)
      ]
  in
  printf "%d\n" !!sig_t;
  [%expect {| 0 |}];
  List.iter (List.range 0 10) ~f:(fun _ ->
    step sim;
    printf "%d:%d " (current_time sim) !!sig_t);
  [%expect {| 1:0 2:1 3:2 4:3 5:4 6:5 7:6 8:7 9:8 10:9 |}]
;;

let nand a b =
  match a, b with
  | 0, _ | _, 0 -> 1
  | 1, 1 -> 0
  | _, _ -> undefined_value
;;

let%expect_test "sr_flip_flop" =
  (* https://en.wikipedia.org/wiki/Flip-flop_(electronics)#SR_NAND_latch *)
  let open Simulator in
  let sig_s = Signal.create (module Value) in
  let sig_r = Signal.create (module Value) in
  let sig_qplus = Signal.create (module Value) in
  let sig_qminus = Signal.create (module Value) in
  let sim =
    create
      [ Process.create [ !&sig_s; !&sig_qminus ] (fun () ->
          sig_qplus <-- nand !!sig_s !!sig_qminus)
      ; Process.create [ !&sig_r; !&sig_qplus ] (fun () ->
          sig_qminus <-- nand !!sig_r !!sig_qplus)
      ]
  in
  let print_step () =
    step sim;
    printf "qplus=%d qminus=%d\n" !!sig_qplus !!sig_qminus
  in
  Expert.schedule_external_set sim sig_s 1;
  Expert.schedule_external_set sim sig_r 0;
  step sim;
  print_step ();
  [%expect {| qplus=0 qminus=1 |}];
  print_step ();
  [%expect {| qplus=0 qminus=1 |}];
  Expert.schedule_external_set sim sig_s 1;
  Expert.schedule_external_set sim sig_r 1;
  print_step ();
  [%expect {| qplus=0 qminus=1 |}];
  Expert.schedule_external_set sim sig_s 0;
  Expert.schedule_external_set sim sig_r 1;
  print_step ();
  [%expect {| qplus=1 qminus=0 |}];
  Expert.schedule_external_set sim sig_s 1;
  Expert.schedule_external_set sim sig_r 1;
  print_step ();
  [%expect {| qplus=1 qminus=0 |}]
;;

let%expect_test "long wire" =
  let open Simulator in
  let signals = List.init 10 ~f:(fun _ -> Signal.create (module Value)) in
  let copiers =
    List.map2_exn (List.drop_last_exn signals) (List.tl_exn signals) ~f:(fun sig1 sig2 ->
      Process.create [ !&sig1 ] (fun () -> sig2 <-- !!sig1 + 1))
  in
  let sim = create copiers in
  Expert.schedule_external_set sim (List.hd_exn signals) 0;
  step sim;
  List.iter signals ~f:(fun signal -> printf "%d " !!signal);
  [%expect {| 0 1 2 3 4 5 6 7 8 9 |}]
;;

let%expect_test "very long wire" =
  let open Simulator in
  let signals = List.init 10 ~f:(fun _ -> Signal.create (module Value_default_zero)) in
  let copiers =
    List.map2_exn (List.drop_last_exn signals) (List.tl_exn signals) ~f:(fun sig1 sig2 ->
      Process.create [ !&sig1 ] (fun () ->
        if !!sig1 > 0 then (sig2 <--- !!sig1 + 1) ~delay:200000))
  in
  let sim = create copiers in
  let print_test () =
    printf "time=%d : " (Simulator.current_time sim);
    List.iter signals ~f:(fun signal -> printf "%d " !!signal)
  in
  step sim;
  Expert.schedule_external_set sim (List.hd_exn signals) 1;
  step sim;
  print_test ();
  [%expect {| time=200000 : 1 0 0 0 0 0 0 0 0 0 |}];
  step sim;
  print_test ();
  [%expect {| time=400000 : 1 2 0 0 0 0 0 0 0 0 |}];
  Expert.schedule_external_set sim (List.nth_exn signals 6) 20;
  step sim;
  print_test ();
  [%expect {| time=600000 : 1 2 3 0 0 0 20 0 0 0 |}];
  step sim;
  print_test ();
  [%expect {| time=800000 : 1 2 3 4 0 0 20 21 0 0 |}];
  stabilise sim;
  print_test ();
  [%expect {| time=1200000 : 1 2 3 4 5 6 -1000 21 22 23 |}]
;;

let%expect_test "ppx_let integration" =
  let open Simulator in
  let open Async in
  let sig_a = Signal.create (module Value_default_zero) in
  let sig_b = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ Process.create [ !&sig_a ] (fun () -> (sig_b <--- 2 * !!sig_a) ~delay:300)
      ; create_process (fun () ->
          sig_a <-- !!sig_a + 1;
          delay 50)
      ; create_process (fun () ->
          let%map () = wait_for_change !&sig_b in
          printf "t=%d\n" (current_time ()))
      ]
  in
  run sim ~time_limit:700;
  [%expect
    {|
    t=300
    t=350
    t=400
    t=450
    t=500
    t=550
    t=600
    t=650
    |}]
;;

let%expect_test "undefined write 1" =
  let open Simulator in
  let open Async in
  let sig_a = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ create_process (fun () ->
          let%map () = delay 10 in
          sig_a <-- 2)
      ; create_process (fun () ->
          let%map () = delay 10 in
          sig_a <-- 1)
      ; Debug.print_signal "sig_a" sig_a
      ]
  in
  run sim ~time_limit:30;
  [%expect {| t=10 sig_a=-1000 |}]
;;

let%expect_test "undefined write 2" =
  let open Simulator in
  let open Async in
  let sig_a = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ create_process (fun () ->
          let%map () = delay 10 in
          printf "process 1 called\n";
          (* notice how this doesn't actually change value of sig_a, but is still counted
             as a write *)
          sig_a <-- 0)
      ; create_process (fun () ->
          let%map () = delay 10 in
          printf "process 2 called\n";
          sig_a <-- 1)
      ; Debug.print_signal "sig_a" sig_a
      ]
  in
  run sim ~time_limit:30;
  [%expect
    {|
    process 1 called
    process 2 called
    t=10 sig_a=-1000
    process 1 called
    process 2 called
    |}]
;;

let%expect_test "multiple writes from one process" =
  let open Simulator in
  let open Async in
  let sig_a = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ create_process (fun () ->
          let%map () = delay 10 in
          sig_a <-- 1;
          sig_a <-- 2)
      ; Debug.print_signal "sig_a" sig_a
      ]
  in
  Simulator.run sim ~time_limit:30;
  [%expect {| t=10 sig_a=2 |}]
;;

let%expect_test "schedule keeps ordering" =
  let open Simulator in
  let sig_a = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ Process.create [ !&sig_a ] (fun () ->
          printf "schedule write (a=%d)\n" !!sig_a;
          (sig_a <--- !!sig_a + 100) ~delay:10;
          (sig_a <--- !!sig_a + 1) ~delay:10)
      ; Debug.print_signal "sig_a" sig_a
      ]
  in
  run sim ~time_limit:30;
  [%expect
    {|
    schedule write (a=0)
    t=10 sig_a=1
    schedule write (a=1)
    t=20 sig_a=2
    schedule write (a=2)
    |}]
;;

let%expect_test "register" =
  let open Simulator in
  let reg ~clock ~src ~dst =
    Process.create [ !&clock ] (fun () ->
      if !!clock = 1 then (* rising edge *)
                       dst <-- !!src)
  in
  let sig_a = Signal.create (module Value_default_zero) in
  let sig_clock = Signal.create (module Value_default_zero) in
  let sig_b = Signal.create (module Value_default_zero) in
  let sim =
    create
      [ reg ~clock:sig_clock ~src:sig_a ~dst:sig_b
      ; Process.create [ !&sig_clock ] (fun () ->
          (sig_clock <--- 1 - !!sig_clock) ~delay:80)
      ; Process.create [ !&sig_a ] (fun () -> (sig_a <--- !!sig_a + 1) ~delay:30)
      ; Debug.print_signal "sig_a" sig_a
      ; Debug.print_signal "sig_b" sig_b
      ; Debug.print_signal "sig_clock" sig_clock
      ]
  in
  run sim ~time_limit:300;
  [%expect
    {|
    t=30 sig_a=1
    t=60 sig_a=2
    t=80 sig_clock=1
    t=80 sig_b=2
    t=90 sig_a=3
    t=120 sig_a=4
    t=150 sig_a=5
    t=160 sig_clock=0
    t=180 sig_a=6
    t=210 sig_a=7
    t=240 sig_clock=1
    t=240 sig_a=8
    t=240 sig_b=8
    t=270 sig_a=9
    |}]
;;
