open Base

module type Basic = sig
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

module type S = sig
  include Applicative.S

  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t

  (** An operator alias for [select], which is sometimes convenient. It tries
      to follow the notational convention for [Applicative] operators. The
      angle bracket pointing to the left means we always use the corresponding
      value. The value on the right, however, may be unnecessary, hence the
      question mark. *)
  val ( <*? ) : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t

  (** The [branch] function is a natural generalisation of [select]: instead of
      skipping an unnecessary effect, it chooses which of the two given
      effectful functions to apply to a given argument. *)
  val branch : ('a, 'b) Either.t t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t

  (** Branch on a Boolean value, skipping unnecessary effects. *)
  val ifS : bool t -> 'a t -> 'a t -> 'a t

  (** Conditionally perform an effect. *)
  val whenS : bool t -> unit t -> unit t

  (** A lifted version of lazy Boolean OR. *)
  val ( <||> ) : bool t -> bool t -> bool t

  (** A lifted version of lazy Boolean AND. *)
  val ( <&&> ) : bool t -> bool t -> bool t
end
