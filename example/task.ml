(* Work in progress, prototyped in https://try.ocamlpro.com/. To be modified
   for Base. *)

open Base

let listSelect fs = function
  | Either.First a -> List.map fs ~f:(fun f -> f a)
  | Either.Second b -> [b]

(* ListSelective.select [Either.First 1; Either.Second 2] [(fun x -> x * 10);
   (fun x -> x * 20)] - : int ListSelective.t = [10; 20; 2] *)
module ListSelective : Selective.S with type 'a t = 'a list =
Selective.Make (struct
  type 'a t = 'a list

  let return x = [x]

  let apply fs xs =
    List.concat_map xs ~f:(fun x -> List.map fs ~f:(fun f -> f x))

  let map = `Custom List.map

  let select xs fs = List.concat_map xs ~f:(fun x -> listSelect fs x)
end)

module type Task = sig
  type k

  type v

  module Make (S : Selective.S) : sig
    val run : (k -> v S.t) -> v S.t
  end
end

module Example : Task with type k := string and type v := int = struct
  module Make (S : Selective.S) = struct
    let run fetch =
      S.ifS
        (S.map (fetch "condition") ~f:(fun x -> x = 0))
        (fetch "zero") (fetch "non-zero")
  end
end

module type Monoid = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module Const (M : Monoid) = Selective.Make (struct
  type 'a t = Const of M.t

  let return _ = Const M.empty

  let apply (Const x) (Const y) = Const (M.append x y)

  let map = `Define_using_apply

  let select (Const x) (Const y) = Const (M.append x y)
end)
