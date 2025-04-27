open Domainslib

let shuffle_parallel (input : ('a * 'b) list list) : ('a * 'b list) list =
  let map = Concurrent_hashmap.ConcurrentHashMap.create ~expected_size:10 () in
  let input_array = Array.of_list input in
  let n = Array.length input_array in
  let num_domains = n in
  let pool = Task.setup_pool ~num_domains () in
  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
      let inner_list = input_array.(i) in
      List.iter
        (fun (key, value) -> Concurrent_hashmap.ConcurrentHashMap.insert map key value)
        inner_list));
  Task.teardown_pool pool;
  List.map
    (fun key ->
       let values = Concurrent_hashmap.ConcurrentHashMap.read map key in
       key, values)
    (Concurrent_hashmap.ConcurrentHashMap.keys map)
;;

let shuffle_sequential (input : ('a * 'b) list list) : ('a * 'b list) list =
  let map = Hashtbl.create 10 in
  List.iter
    (fun inner_list ->
       List.iter (fun (key, value) -> Hashtbl.add map key value) inner_list)
    input;
  let key_list = Hashtbl.fold (fun key _value acc -> key :: acc) map [] in
  let unique_keys = List.sort_uniq compare key_list in
  List.map (fun key -> key, Hashtbl.find_all map key) unique_keys
;;
