open Domainslib
open Dataflowlib

(** A polymorphic groupby node.
    The user supplies a generic list, the result is grouped by the first element of the tuple. *)
type ('a, 'b) groupby =
  { input : ('a * 'b) list (** Elements to group *)
  }

let shuffle_parallel (input : ('a * 'b) list) : ('a * 'b list) list =
  let num_domains = 4 in (* Or determine dynamically *)
  let partitions = Array.make num_domains [] in
  List.iter (fun (key, value) ->
    let index = (Hashtbl.hash key) mod num_domains in
    partitions.(index) <- (key, value) :: partitions.(index)
  ) input;

  let map = Concurrent_hashmap.ConcurrentHashMap.create ~expected_size:10 () in
  let n = Array.length partitions in
  let pool = Task.setup_pool ~num_domains () in

  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
      (* No need to destructure, element is already the list *)
      let inner_list = partitions.(i) in
      List.iter (fun (key, value) ->
        Concurrent_hashmap.ConcurrentHashMap.insert map key value
      ) inner_list
    )
  );

  Task.teardown_pool pool;

  (* ... rest of result collection ... *)
  List.map (fun key ->
    let values = Concurrent_hashmap.ConcurrentHashMap.read map key in
    (key, values)
  ) (Concurrent_hashmap.ConcurrentHashMap.keys map)

;;
  
let shuffle_sequential (input : ('a * 'b) list) : ('a * 'b list) list =
  let map = Concurrent_hashmap.ConcurrentHashMap.create ~expected_size:10 () in
  List.iter (fun (key, value) ->
    Concurrent_hashmap.ConcurrentHashMap.insert map key value
  ) input;
  let key_list = Concurrent_hashmap.ConcurrentHashMap.keys map in
  let unique_keys = List.sort_uniq compare key_list in
  List.map (fun key ->
    (key, Concurrent_hashmap.ConcurrentHashMap.read map key)
  ) unique_keys
;;

let run_groupby (node : ('a, 'b) groupby) : ('a * 'b list) list =
  shuffle_parallel node.input
;;
