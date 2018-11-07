open Base;;

module type Selective = sig
  include Applicative.Basic

  val select : (('a, 'b) Either.t) t -> ('a -> 'b) t -> 'b t
end;;

module Make_selective_helpers(S : Selective) = struct
  include Applicative.Make(S)

  let (<*?) x f = S.select x f

  let branch (x : (('a, 'b) Either.t) S.t) (l : ('a -> 'c) S.t) (r : ('b -> 'c) S.t) : 'c S.t =
    map x ~f:(fun e -> Either.map e ~first:Fn.id ~second:Either.first) <*?
    map l ~f:(Fn.compose Either.second)                                <*? r

  let ifS (c : bool S.t) (t : 'a S.t) (e : 'a S.t) : 'a S.t =
    branch (map c ~f:(fun b -> if b then Either.First () else Either.Second ()))
           (map t ~f:(fun a -> Fn.const a))
           (map e ~f:(fun a -> Fn.const a))
end
