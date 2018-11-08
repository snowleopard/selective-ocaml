(** Selective functors *)

open Base

module type Selective = sig
  include Applicative.Basic

  (** Selective applicative functors. You can think of [select] as a selective
      function application: you apply a function only when given a value
      [First a]. Otherwise, you can skip the function and associted effects and
      return the [b] from [Second b].

      The type signature of [select] is reminiscent of both [<*>] and [>>=],
      and indeed a selective functor is in some sense a composition of an
      applicative functor and the [Either] monad. *)
  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
end

module Make_selective_helpers (S : Selective) : sig
  include Applicative.S with type 'a t = 'a S.t

  (** An operator alias for [select], which is sometimes convenient. It tries
      to follow the notational convention for [Applicative] operators. The
      angle bracket pointing to the left means we always use the corresponding
      value. The value on the right, however, may be unnecessary, hence the
      question mark. *)
  val ( <*? ) : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t

  (** The [branch] function is a natural generalisation of [select]: instead of
      skipping an unnecessary effect, it chooses which of the two given
      effectful functions to apply to a given argument. *)
  val branch :
    ('a, 'b) Either.t S.t -> ('a -> 'c) S.t -> ('b -> 'c) S.t -> 'c S.t

  (** Branch on a Boolean value, skipping unnecessary effects. *)
  val ifS : bool S.t -> 'a S.t -> 'a S.t -> 'a S.t

  (** Conditionally perform an effect. *)
  val whenS : bool S.t -> unit S.t -> unit S.t

  (** A lifted version of lazy Boolean OR. *)
  val ( <||> ) : bool S.t -> bool S.t -> bool S.t

  (** A lifted version of lazy Boolean AND. *)
  val ( <&&> ) : bool S.t -> bool S.t -> bool S.t
end
