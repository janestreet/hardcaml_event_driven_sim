open! Core
open Hardcaml.Signal

let create_process_with_initial_delay ~initial_delay f =
  let open Event_driven_sim.Simulator in
  Async.create_process
    (let did_initial_run = ref false in
     fun () ->
       if !did_initial_run
       then f ()
       else (
         did_initial_run := true;
         if initial_delay = 0
         then f ()
         else (
           let%bind.Async.Deferred () = Async.delay initial_delay in
           f ())))
;;

(* These tests show various edge cases with read/writing signals on an [Async.delay] that
   leads to potentially unexpected behavior.

   Everything is tested both with and without an initial delay as the specific reasons
   behind the timings demonstrated are different in each case. *)
module Test (Simulator : Hardcaml_event_driven_sim.S) = struct
  open Simulator

  (* If an input is written to on a [Async.delay] at the same timestep as a clock tick,
     then regs on that clock use the old value of the input unless that input feeds
     directly to the reg *)
  module%test [@name "Writing on [Async.delay]"] _ = struct
    module I = struct
      type 'a t =
        { clock : 'a [@bits 1]
        ; data : 'a [@bits 1]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { direct : 'a [@bits 1]
        ; delay : 'a [@bits 1]
        }
      [@@deriving hardcaml]
    end

    (* Set both [direct] and [delay] to [data] on [clock], but have a wire in the middle
       for [delay]. *)
    let f (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock () in
      let wire = wire 1 in
      wire <-- i.data;
      { O.direct = reg spec i.data; O.delay = reg spec wire }
    ;;

    (* Using [Async.delay], flip [data] on the timesteps of [clock]'s ticks. *)
    let run ~initial_delay =
      let open Simulator in
      let open Logic in
      let module Sim_interface = With_interface (I) (O) in
      let waves, { Sim_interface.simulator; _ } =
        Sim_interface.with_waveterm f (fun input _output ->
          let input = I.map input ~f:(fun v -> v.signal) in
          [ Sim_interface.create_clock input.clock ~time:1 ~initial_delay
          ; create_process_with_initial_delay ~initial_delay (fun () ->
              input.data <-- ~:(!!(input.data));
              Async.delay 2)
          ])
      in
      run simulator ~time_limit:10;
      Hardcaml_event_driven_sim.Waveterm.Waveform.expect
        waves
        ~wave_width:1
        ~display_width:50
    ;;

    (* For both of these tests, the order of operations when the clocks ticks is:

       1. Clock gets set high (this schedules the registers to update their values)
       2. Data gets set (this schedules the wire to update its value)
       3. The 2 registers update their values. Direct reads data (set in step 2), Delay
          reads the wire (old value of data)
       4. wire updates its value; it reads the value of Data *)

    let%expect_test "without initial delay" =
      run ~initial_delay:0;
      [%expect
        {|
        ┌Signals───┐┌Waves───────────────────────────────┐
        │clock     ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
        │          ││    └───┘   └───┘   └───┘   └───┘   │
        │data      ││────────┐       ┌───────┐       ┌───│
        │          ││        └───────┘       └───────┘   │
        │delay     ││        ┌───────┐       ┌───────┐   │
        │          ││────────┘       └───────┘       └───│
        │direct    ││────────┐       ┌───────┐       ┌───│
        │          ││        └───────┘       └───────┘   │
        └──────────┘└────────────────────────────────────┘
        7d2db14f42d455b97ff4b58504f0b7a7
        |}]
    ;;

    let%expect_test "with initial delay" =
      run ~initial_delay:1;
      [%expect
        {|
        ┌Signals───┐┌Waves───────────────────────────────┐
        │clock     ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
        │          ││────┘   └───┘   └───┘   └───┘   └───│
        │data      ││    ┌───────┐       ┌───────┐       │
        │          ││────┘       └───────┘       └───────│
        │delay     ││            ┌───────┐       ┌───────│
        │          ││────────────┘       └───────┘       │
        │direct    ││    ┌───────┐       ┌───────┐       │
        │          ││────┘       └───────┘       └───────│
        └──────────┘└────────────────────────────────────┘
        ea275f11ac42c367c03fbdc068af4473
        |}]
    ;;
  end

  (* If a clock is read on an [Async.delay] at the same timestep as the clock tick, then
     the old value of the clock is read except for on the first clock tick *)
  module%test [@name "Reading on [Async.delay]"] _ = struct
    module I = struct
      type 'a t =
        { clock : 'a [@bits 1]
        ; direct : 'a [@bits 1]
        ; delay : 'a [@bits 1]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { clock_delay : 'a [@bits 1] } [@@deriving hardcaml]
    end

    let f (i : _ I.t) = { O.clock_delay = i.clock }

    (* Using [Async.delay], on the timesteps of [clock]s ticks set [direct] to [clock] and
       [delay] to a wire of [clock] (Hardcaml makes [clock_delay] have its own wire) *)
    let run ~initial_delay =
      let open Simulator in
      let module Sim_interface = With_interface (I) (O) in
      let waves, { Sim_interface.simulator; _ } =
        Sim_interface.with_waveterm f (fun input output ->
          let input = I.map input ~f:(fun v -> v.signal) in
          let output = O.map output ~f:(fun v -> v.signal) in
          [ Sim_interface.create_clock input.clock ~time:1 ~initial_delay
          ; create_process_with_initial_delay ~initial_delay (fun () ->
              input.direct <-- !!(input.clock);
              input.delay <-- !!(output.clock_delay);
              Async.delay 2)
          ])
      in
      run simulator ~time_limit:10;
      Waveterm.Waveform.expect waves ~wave_width:1 ~display_width:50
    ;;

    (* When [initial_delay=0], both [direct] and [delay] are set with [clock]s old value
       of gnd *)
    let%expect_test "without initial delay" =
      (* The order of operations here whenever the clock goes high is:

         1. The clock is scheduled to go from 0 -> 1
         2. We schedule setting Direct and Delay using the current value of Clock (0) and
            Clock_delay (0)
         3. The clock gets set to 1
         4. Direct and Delay get set to 0
         5. Clock_delay gets scheduled to be set to the current value of Clock (1)
         6. Clock_delay gets set to 1
      *)
      run ~initial_delay:0;
      [%expect
        {|
        ┌Signals───┐┌Waves───────────────────────────────┐
        │clock     ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
        │          ││    └───┘   └───┘   └───┘   └───┘   │
        │clock_dela││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
        │          ││    └───┘   └───┘   └───┘   └───┘   │
        │delay     ││                                    │
        │          ││────────────────────────────────────│
        │direct    ││                                    │
        │          ││────────────────────────────────────│
        └──────────┘└────────────────────────────────────┘
        48469aea3b493d038e483dfdf1c74c2f
        |}]
    ;;

    let%expect_test "with initial delay" =
      (* The order of operations from the start is:

         1. Clock is scheduled to go from 0 -> 1 at [T=1]
         2. Process [P] is scheduled to run at [T=1] T = 1
         3. Clock value is set to 1
         4. Clock_delay is scheduled to go from 0 -> 1
         5. Process [P] runs, schedules Direct <- Clock (1), Delay <- Clock_delay (0),
            schedules [P] to run at [T=3]
         6. Direct is set to 1, Delay is set to 0
         7. Clock is scheduled to go from 1 -> 0 at [T=2]
         8. Clock_delay is set to 1
         9. Clock is set to 0
         10. Clock is scheduled to go from 0 -> 1 at [T=3] This is scheduled behind
             process [P]'s run at [T=3]

         Step 10. is why the initial behavior is different from subsequent behavior.
      *)
      run ~initial_delay:1;
      [%expect
        {|
        ┌Signals───┐┌Waves───────────────────────────────┐
        │clock     ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
        │          ││────┘   └───┘   └───┘   └───┘   └───│
        │clock_dela││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
        │          ││────┘   └───┘   └───┘   └───┘   └───│
        │delay     ││                                    │
        │          ││────────────────────────────────────│
        │direct    ││    ┌───────┐                       │
        │          ││────┘       └───────────────────────│
        └──────────┘└────────────────────────────────────┘
        d899c476a4bef7855d14ba8baeed3205
        |}]
    ;;
  end
end

module%test Two_state = Test (Hardcaml_event_driven_sim.Two_state_simulator)
