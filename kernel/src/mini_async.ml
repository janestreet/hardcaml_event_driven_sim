open Base

module Deferred_basic = struct
  type 'a t_val =
    | Filled of 'a
    | Waiting of ('a -> unit) list

  type 'a t = 'a t_val ref

  let upon t f =
    match !t with
    | Filled value -> f value
    | Waiting callbacks -> t := Waiting (f :: callbacks)
  ;;

  let return value = ref (Filled value)

  let peek t =
    match !t with
    | Filled value -> Some value
    | Waiting _ -> None
  ;;

  module Ivar = struct
    type nonrec 'a t = 'a t

    let read t = t

    let fill t value =
      match !t with
      | Filled _ -> failwith "attempting to fill Ivar which is already filled"
      | Waiting callbacks ->
        t := Filled value;
        List.iter callbacks ~f:(fun f -> f value)
    ;;

    let is_filled t =
      match !t with
      | Filled _ -> true
      | Waiting _ -> false
    ;;

    let create () = ref (Waiting [])
  end

  let bind t ~f =
    let result = Ivar.create () in
    upon t (fun a -> upon (f a) (fun b -> Ivar.fill result b));
    result
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
