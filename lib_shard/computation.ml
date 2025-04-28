open Domainslib
open Shards

type ('a, 'b) t =
  { input : 'a list
  ; transform : 'a -> 'b
  }

let sequential node = map node.transform (of_list ~n:1 node.input)

let parallel n_domains node =
  let shards = of_list ~n:n_domains node.input in
  let pool = Task.setup_pool ~num_domains:n_domains () in
  let buckets =
    Task.run pool
    @@ fun () ->
    Array.init n_domains (fun k ->
      Task.await
        pool
        (Task.async pool (fun () -> List.map node.transform shards.buckets.(k))))
  in
  Task.teardown_pool pool;
  { buckets; n = n_domains }
;;

let run ?(num_domains = 4) node =
  if num_domains = 0 then sequential node else parallel num_domains node
;;
