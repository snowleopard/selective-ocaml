open Base
open! Expect_test_helpers_kernel

module Grammar = struct
  type t = entry list

  and entry =
    | Keyword of string
    | Atom
    | List of t
    | Repeat of t
  [@@deriving sexp]
end

module Parser : sig
  (** Type of a parser producing a value of type ['a] from an input composed of
      a sequence of s-expressions *)
  type 'a t

  include Selective.S with type 'a t := 'a t

  (** Expect the next element to be the following atom *)
  val keyword : string -> unit t

  (** Expect the next element to be an atom and return its contents *)
  val atom : string t

  (** [enter t] expects the next element to be a list and parse its contents
      with [t]. *)
  val enter : 'a t -> 'a t

  (** Consume all the input with the following parser *)
  val repeat : 'a t -> 'a list t

  (** Test whether the next element of the input is a list *)
  val is_list : bool t

  val parse : 'a t -> Sexp.t list -> 'a

  val grammar : _ t -> Grammar.t

  module Let_syntax : sig
    val return : 'a -> 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end = struct
  module M = struct
    type 'a t =
      | Return : 'a -> 'a t
      | Apply : ('a -> 'b) t * 'a t -> 'b t
      | Select : ('a, 'b) Either.t t * ('a -> 'b) t -> 'b t
      | Keyword : string -> unit t
      | Atom : string t
      | Enter : 'a t -> 'a t
      | Is_list : bool t
      | Repeat : 'a t -> 'a list t

    let return x = Return x

    let apply f t = Apply (f, t)

    let select t f = Select (t, f)

    let map = `Define_using_apply
  end

  open M
  include Selective.Make (M)

  module Let_syntax = struct
    let return = return

    let map = map

    let both = both
  end

  let keyword s = Keyword s

  let atom = Atom

  let enter s = Enter s

  let is_list = Is_list

  let repeat t = Repeat t

  let rec parse : type a. a t -> Sexp.t list -> a * Sexp.t list =
   fun t sexps ->
    match t with
    | Return x -> (x, sexps)
    | Apply (f, t) ->
        let f, sexps = parse f sexps in
        let t, sexps = parse t sexps in
        (f t, sexps)
    | Select (t, f) -> (
        let t, sexps = parse t sexps in
        match t with
        | First x ->
            let f, sexps = parse f sexps in
            (f x, sexps)
        | Second y -> (y, sexps) )
    | Keyword s -> (
      match sexps with
      | Atom x :: rest when String.equal x s -> ((), rest)
      | _ -> Printf.ksprintf failwith "keyword %s expected" s )
    | Enter t -> (
      match sexps with
      | List l :: rest ->
          let x, sexps = parse t l in
          if not (List.is_empty sexps) then
            failwith "remaninig elements at end of list";
          (x, rest)
      | _ -> raise_s [%message "list expected" ~sexps:(sexps : Sexp.t list)] )
    | Is_list -> (
      match sexps with List _ :: _ -> (true, sexps) | _ -> (false, sexps) )
    | Atom -> (
      match sexps with
      | Atom x :: rest -> (x, rest)
      | _ -> failwith "atom expected" )
    | Repeat t ->
        let rec loop acc = function
          | [] -> List.rev acc
          | sexps ->
              let x, sexps = parse t sexps in
              loop (x :: acc) sexps
        in
        (loop [] sexps, [])

  let parse t sexps =
    let x, sexps = parse t sexps in
    if not (List.is_empty sexps) then
      failwith "remaining elements at end of input";
    x

  let rec grammar : type a. a t -> Grammar.t = function
    | Return _ -> []
    | Apply (f, t) -> grammar f @ grammar t
    | Select (t, f) -> grammar t @ grammar f
    | Keyword s -> [Keyword s]
    | Enter t -> [List (grammar t)]
    | Is_list -> []
    | Atom -> [Atom]
    | Repeat t -> [Repeat (grammar t)]
end

module Test = struct
  open Parser

  type hello_to =
    | These of string list
    | World
  [@@deriving sexp]

  let expr =
    enter
      (let%map () = keyword "hello"
       and y =
         ifS is_list
           (let%map x = enter (repeat atom) in
            These x)
           (let%map () = keyword "world!" in
            World)
       in
       y)

  let%expect_test _ =
    let v = parse expr [List [Atom "hello"; Atom "world!"]] in
    print_s [%sexp (v : hello_to)];
    [%expect {| World |}]

  let%expect_test _ =
    let v =
      parse expr [List [Atom "hello"; List [Atom "bob"; Atom "alice"]]]
    in
    print_s [%sexp (v : hello_to)];
    [%expect {| (These (bob alice)) |}]

  let%expect_test _ =
    print_s [%sexp (grammar expr : Grammar.t)];
    [%expect
      {| ((List ((Keyword hello) (List ((Repeat (Atom)))) (Keyword world!))))
     |}]
end
