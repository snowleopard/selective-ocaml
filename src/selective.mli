(** Selective functors *)

module type Basic = Selective_intf.Basic

module type S = Selective_intf.S

module Make (S : Basic) : S with type 'a t = 'a S.t
