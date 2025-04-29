open Shards

module Make (Ord : Map.OrderedType) = struct
  module M = Map.Make (Ord)

  type ('k, 'v) t = { input : ('k * 'v) list }

  let run ?(num_domains = 4) { input } : ('k * 'v list) Shards.t =
    let grouped =
      List.fold_left
        (fun m (k, v) ->
           let vs =
             match M.find_opt k m with
             | None -> []
             | Some l -> l
           in
           M.add k (v :: vs) m)
        M.empty
        input
    in
    let n = max 1 num_domains in
    let buckets = Array.make n [] in
    M.iter
      (fun k vs ->
         let i = Stdlib.Hashtbl.hash k mod n in
         buckets.(i) <- (k, vs) :: buckets.(i))
      grouped;
    Array.iteri (fun i l -> buckets.(i) <- List.rev l) buckets;
    { buckets; n }
  ;;
end
