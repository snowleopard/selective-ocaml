(* Work in progress, prototyped in https://try.ocamlpro.com/.
To be modified for Base. *)

module Either = struct
  type ('f, 's) t =
    | First  of 'f
    | Second of 's
  let map t ~first ~second =
    match t with
    | First x -> First (first x)
    | Second x -> Second (second x)
  let first  x = First  x
  let second x = Second x
end

module type Applicative = sig
  type 'a t
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type Selective = sig
  include Applicative
  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
  val ifS : bool t -> 'a t -> 'a t -> 'a t
end

let listSelect fs = function
    | Either.First a  -> List.map (fun f -> f a) fs
    | Either.Second b -> [b]

(* ListSelective.select [Either.First 1; Either.Second 2] [(fun x -> x * 10); (fun x -> x * 20)]
- : int ListSelective.t = [10; 20; 2]
*)
module ListSelective : Selective with type 'a t = 'a list = struct
  type 'a t = 'a list
  let return x = [x]
  let apply fs xs = List.concat (List.map (fun x -> List.map (fun f -> f x) fs) xs)
  let map x ~f = List.map f x
  let select xs fs = List.concat (List.map (fun x -> listSelect fs x) xs)
  let ifS xs ts fs = List.concat (List.map (fun x -> if x then ts else fs) xs)
end

module type Task = sig
  type k
  type v
  module Make(S : Selective) : sig
    val run : (k -> v S.t) -> v S.t
  end
end

module Example : Task with type k := string and type v := int = struct
  module Make(S : Selective) = struct
    let run fetch =
        S.ifS (S.map (fetch "condition") ~f:(fun x -> x = 0))
              (fetch "zero")
              (fetch "non-zero")
  end
end


