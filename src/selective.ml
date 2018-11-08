open Base;;

module type Selective = sig
  include Applicative.Basic

  val select : (('a, 'b) Either.t) t -> ('a -> 'b) t -> 'b t
end;;

module Make_selective_helpers(S : Selective) = struct
  include Applicative.Make(S)

  let (<*?) x f = S.select x f

  let branch (x : (('a, 'b) Either.t) S.t) (l : ('a -> 'c) S.t) (r : ('b -> 'c) S.t) : 'c S.t =
    map x ~f:(Either.map ~first:Fn.id ~second:Either.first) <*?
    map l ~f:(Fn.compose Either.second) <*? r

  let ifS (x : bool S.t) (t : 'a S.t) (f : 'a S.t) : 'a S.t =
    branch (map x ~f:(fun b -> if b then Either.First () else Either.Second ()))
           (map t ~f:Fn.const)
           (map f ~f:Fn.const)
end
