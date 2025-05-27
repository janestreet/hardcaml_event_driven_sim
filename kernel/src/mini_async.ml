open Core

module Deferred_basic = struct
  type 'a t = { mutable cell : 'a t_cell }

  and 'a t_cell =
    | Filled of 'a
    | Waiting of ('a -> unit) Doubly_linked.t
    | Ptr of 'a t
  (* Used to link binds. Having a special type here allows us to identity tail-recursive
     nested binds. See [bind] for more details. *)

  let rec cell_to_string (t : _ t_cell) =
    match t with
    | Filled _ -> "Filled"
    | Waiting list -> [%string {|Waiting on %{Doubly_linked.length list#Int}|}]
    | Ptr t1 -> [%string {|ptr [ %{to_string t1} ]|}]

  and to_string (t : _ t) = cell_to_string t.cell

  (* Returns the non-ptr at the bottom of a chain of ptrs. Additionally updates all ptrs
     in the chain to point to the final non-ptr at the bottom. See tests at bottom of file
     for examples. *)
  let squash t =
    let rec follow t ~deepest_known =
      match t.cell with
      | Ptr t1 -> follow t1 ~deepest_known:t1
      | _ -> deepest_known
    in
    let rec update t ~deepest =
      match t.cell with
      | Ptr t1 ->
        t.cell <- Ptr deepest;
        update t1 ~deepest
      | _ -> t
    in
    match t.cell with
    | Ptr t1 ->
      let deepest = follow t ~deepest_known:t1 in
      update t ~deepest
    | _ -> t
  ;;

  let upon t f =
    let add_to_front callbacks f =
      ignore (Doubly_linked.insert_first callbacks f : _ Doubly_linked.Elt.t)
    in
    let t = squash t in
    match t.cell with
    | Ptr _ -> assert false (* squash assures this *)
    | Filled value -> f value
    | Waiting callbacks -> add_to_front callbacks f
  ;;

  let return value = { cell = Filled value }

  let peek t =
    let t = squash t in
    match t.cell with
    | Ptr _ -> assert false (* squash assures this *)
    | Filled value -> Some value
    | Waiting _ -> None
  ;;

  module Ivar = struct
    type nonrec 'a t = 'a t

    let read t = t

    let fill t value =
      let t = squash t in
      match t.cell with
      | Ptr _ -> assert false (* squash assures this *)
      | Filled _ -> failwith "attempting to fill Ivar which is already filled"
      | Waiting callbacks ->
        t.cell <- Filled value;
        Doubly_linked.iter callbacks ~f:(fun f -> f value)
    ;;

    let is_filled t =
      let t = squash t in
      match t.cell with
      | Ptr _ -> assert false (* squash assures this *)
      | Filled _ -> true
      | Waiting _ -> false
    ;;

    let create () = { cell = Waiting (Doubly_linked.create ()) }
  end

  let connect ~bind_result ~bind_rhs =
    let rec redirect_ptrs t ~to_ =
      match t.cell with
      | Ptr t1 ->
        t.cell <- Ptr to_;
        redirect_ptrs t1 ~to_
      | Filled _ -> t.cell
      | Waiting _ as original_t ->
        if not (phys_equal t to_) (* Avoid self loops at bottom of chain. *)
        then t.cell <- Ptr to_;
        original_t
    in
    if not (phys_equal bind_result bind_rhs) (* Avoid self loops at top of chain.*)
    then (
      let bind_result = squash bind_result in
      let bind_rhs_cell = redirect_ptrs bind_rhs ~to_:bind_result in
      match bind_result.cell, bind_rhs_cell with
      | Ptr _, _ | _, Ptr _ -> assert false (* squashed redirect_ptrs assure this *)
      | Filled _, _ ->
        (* [connect] is only used in bind, whose ivar is only ever exported as a read-only
           deferred.  Thus, [bind_result] must be empty. *)
        assert false
      | _, Filled v -> Ivar.fill bind_result v
      | Waiting callbacks1, Waiting callbacks2 ->
        if not (phys_equal callbacks1 callbacks2)
        then Doubly_linked.transfer ~src:callbacks2 ~dst:callbacks1)
  ;;

  let bind t ~f =
    let bind_result = Ivar.create () in
    upon t (fun a -> connect ~bind_result ~bind_rhs:(f a));
    bind_result
  ;;

  let map =
    `Custom
      (fun t ~f ->
        let result = Ivar.create () in
        upon t (fun a -> Ivar.fill result (f a));
        result)
  ;;

  let unit = return ()
end

module Deferred = struct
  include Deferred_basic
  include Monad.Make (Deferred_basic)
end

module Let_syntax = Deferred.Let_syntax
module Ivar = Deferred_basic.Ivar

let test_squash t =
  let print_state t ptrs =
    print_endline [%string {|t = %{(Deferred.to_string t)}|}];
    List.iteri ptrs ~f:(fun i p ->
      print_endline [%string {|p%{i#Int} = %{(Deferred.to_string p)}|}]);
    print_endline ""
  in
  let rec get_ptrs (t : _ Ivar.t) ptrs =
    match t.cell with
    | Ptr t1 -> get_ptrs t1 (t :: ptrs)
    | _ -> ptrs
  in
  let ptrs = get_ptrs t [] in
  print_endline "---before";
  print_state t ptrs;
  let squashed = Deferred.squash t in
  print_endline "---after";
  print_state t ptrs;
  print_endline [%string {|squashed = %{(Deferred.to_string squashed)}|}]
;;

let filled () : _ Ivar.t = { cell = Filled () }
let ptr t : _ Ivar.t = { cell = Ptr t }

let%expect_test "squash - non ptr" =
  test_squash (filled ());
  [%expect
    {|
    ---before
    t = Filled

    ---after
    t = Filled

    squashed = Filled
    |}]
;;

let%expect_test "squash - single ptr" =
  test_squash (ptr (filled ()));
  [%expect
    {|
    ---before
    t = ptr [ Filled ]
    p0 = ptr [ Filled ]

    ---after
    t = ptr [ Filled ]
    p0 = ptr [ Filled ]

    squashed = Filled
    |}]
;;

let%expect_test "squash - nested ptr" =
  test_squash (ptr (ptr (filled ())));
  [%expect
    {|
    ---before
    t = ptr [ ptr [ Filled ] ]
    p0 = ptr [ Filled ]
    p1 = ptr [ ptr [ Filled ] ]

    ---after
    t = ptr [ Filled ]
    p0 = ptr [ Filled ]
    p1 = ptr [ Filled ]

    squashed = Filled
    |}]
;;
