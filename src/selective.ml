open Base
include Selective_intf

module Make (S : Basic) = struct
  include S
  include Applicative.Make (S)

  let ( <*? ) x f = S.select x f

  let branch x l r =
    map x ~f:(Either.map ~first:Fn.id ~second:Either.first)
    <*? map l ~f:(Fn.compose Either.second)
    <*? r

  let ifS x t f =
    branch
      (map x ~f:(fun b -> if b then Either.First () else Either.Second ()))
      (map t ~f:Fn.const) (map f ~f:Fn.const)

  let whenS x act = ifS x act (return ())

  let ( <||> ) a b = ifS a (return true) b

  let ( <&&> ) a b = ifS a b (return false)
end
