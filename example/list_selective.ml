open Base

let listSelect fs = function
  | Either.First a -> List.map fs ~f:(fun f -> f a)
  | Either.Second b -> [b]

(* ListSelective.select [Either.First 1; Either.Second 2] [(fun x -> x * 10);
   (fun x -> x * 20)] - : int ListSelective.t = [10; 20; 2] *)
module ListSelective : Selective.S with type 'a t = 'a list =
Selective.Make (struct
  type 'a t = 'a list

  let return x = [x]

  let apply fs xs =
    List.concat_map xs ~f:(fun x -> List.map fs ~f:(fun f -> f x))

  let map = `Custom List.map

  let select xs fs = List.concat_map xs ~f:(fun x -> listSelect fs x)
end)
