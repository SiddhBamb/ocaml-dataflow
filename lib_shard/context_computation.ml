open Domainslib
open Shards

(** A computation that needs an immutable context. *)
type ('a, 'b, 'c) t =
  { input : 'a list (** elements to process *)
  ; transform : 'a -> 'c -> 'b (** user-supplied transformation *)
  ; context : 'c (** immutable context *)
  }

let sequential node : 'b Shards.t =
  of_list ~n:1 (List.map (fun x -> node.transform x node.context) node.input)
;;

let parallel n_domains node : 'b Shards.t =
  let shards = of_list ~n:n_domains node.input in
  let pool = Task.setup_pool ~num_domains:n_domains () in
  let buckets =
    Task.run pool
    @@ fun () ->
    Array.init n_domains (fun i ->
      Task.await
        pool
        (Task.async pool (fun () ->
           List.map (fun x -> node.transform x node.context) shards.buckets.(i))))
  in
  Task.teardown_pool pool;
  { buckets; n = n_domains }
;;

let run ?(num_domains = 4) (node : ('a, 'b, 'c) t) : 'b Shards.t =
  if num_domains = 0 then sequential node else parallel num_domains node
;;
