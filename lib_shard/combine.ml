open Shards

module Make (Ord : Map.OrderedType) = struct
  module M = Map.Make (Ord)

  type ('k, 'v) t =
    { input : ('k * 'v) list
    ; combine : 'v list -> 'v
    }

  let run ?(num_domains = 4) ({ input; combine } : ('k, 'v) t) : ('k * 'v) Shards.t =
    let grouped =
      List.fold_left
        (fun m (k, v) ->
           let vs =
             match M.find_opt k m with
             | Some l -> v :: l
             | None -> [ v ]
           in
           M.add k vs m)
        M.empty
        input
    in
    let reduced = M.bindings grouped |> List.map (fun (k, vs) -> k, combine vs) in
    let n = max 1 num_domains in
    let buckets = Array.make n [] in
    List.iter
      (fun (k, v) ->
         let i = Stdlib.Hashtbl.hash k mod n in
         buckets.(i) <- (k, v) :: buckets.(i))
      reduced;
    Array.iteri (fun i l -> buckets.(i) <- List.rev l) buckets;
    { buckets; n }
  ;;
end
