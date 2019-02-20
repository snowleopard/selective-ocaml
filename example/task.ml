(* Work in progress, prototyped in https://try.ocamlpro.com/. To be modified
   for Base. *)

open Base

module type Task = sig
  type k

  type v

  module Make (S : Selective.S) : sig
    val run : (k -> v S.t) -> v S.t
  end
end

module Example : Task with type k = string and type v = int = struct
  type k = string

  type v = int

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

module Const (M : Monoid) = struct
  type 'a t = Const of M.t

  let return _ = Const M.empty

  let apply (Const x) (Const y) = Const (M.append x y)

  let map = `Define_using_apply

  let select (Const x) (Const y) = Const (M.append x y)
end

module Dependencies (Task : Task) : sig
  val deps : Task.k list
end = struct
  module Ks = Const (struct
    type t = Task.k list

    let empty = []

    let append = List.append
  end)

  module M = Task.Make (Selective.Make (Ks))

  let deps =
    let (Ks.Const x) = M.run (fun k -> Ks.Const [k]) in
    x
end

let%expect_test "task" =
  let module D = Dependencies (Example) in
  List.iter D.deps ~f:Stdio.print_endline;
  [%expect {|
    condition
    zero
    non-zero
  |}]
