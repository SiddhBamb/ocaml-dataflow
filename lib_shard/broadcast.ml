open Shards

(** the collection to replicate *)
type 'a t = { input : 'a list }

let run ?(num_domains = 4) ({ input } : 'a t) : 'a Shards.t =
  let n = max 1 num_domains in
  let buckets = Array.make n input in
  { buckets; n }
;;
