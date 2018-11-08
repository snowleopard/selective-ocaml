open Base;;

module type Selective = sig
  include Applicative.Basic

  (* Selective applicative functors. You can think of [select] as a selective
  function application: you apply a function only when given a value [First a].
  Otherwise, you can skip the function and associted effects and return the [b]
  from [Second b].

  The type signature of [select] is reminiscent of both [<*>] and [>>=], and
  indeed a selective functor is in some sense a composition of an applicative
  functor and the [Either] monad. *)
  val select : (('a, 'b) Either.t) t -> ('a -> 'b) t -> 'b t
end;;

module Make_selective_helpers(S : Selective) = struct
  include Applicative.Make(S)

  (* An operator alias for [select], which is sometimes convenient. It tries to
  follow the notational convention for [Applicative] operators. The angle
  bracket pointing to the left means we always use the corresponding value. The
  value on the right, however, may be unnecessary, hence the question mark. *)
  let (<*?) x f = S.select x f

  (* The [branch] function is a natural generalisation of [select]: instead of
  skipping an unnecessary effect, it chooses which of the two given effectful
  functions to apply to a given argument. *)
  let branch (x : (('a, 'b) Either.t) S.t) (l : ('a -> 'c) S.t) (r : ('b -> 'c) S.t) : 'c S.t =
    map x ~f:(Either.map ~first:Fn.id ~second:Either.first) <*?
    map l ~f:(Fn.compose Either.second) <*? r

  (* Branch on a Boolean value, skipping unnecessary effects. *)
  let ifS (x : bool S.t) (t : 'a S.t) (f : 'a S.t) : 'a S.t =
    branch (map x ~f:(fun b -> if b then Either.First () else Either.Second ()))
           (map t ~f:Fn.const)
           (map f ~f:Fn.const)

  (* Conditionally perform an effect. *)
  let whenS (x : bool S.t) (act : unit S.t) : unit S.t = ifS x act (return ())

  (* A lifted version of lazy Boolean OR. *)
  let (<||>) (a : bool S.t) (b : bool S.t) : bool S.t = ifS a (return true) b

  (* A lifted version of lazy Boolean AND. *)
  let (<&&>) (a : bool S.t) (b : bool S.t) : bool S.t = ifS a b (return false)
end
