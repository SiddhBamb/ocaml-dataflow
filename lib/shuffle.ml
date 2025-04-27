open Domainslib

(*



(*
in this file, we will:
- test Domainslib.Hashtbl's concurrent operations by incrementing various counters in a map
in parallel and checking the results for consistency

*)

module ConcurrentWordMap = struct
  (* The value stored: a mutable list of counts and a mutex to protect access to that list *)
  type value_data = {
    mutable counts : int list;
    value_lock : Mutex.t;
  }

  (* The concurrent map structure: wraps the Hashtbl and adds a Mutex for table-level operations (adding new keys) *)
  type t = {
    table : (string, value_data) Hashtbl.t;
    table_lock : Mutex.t; (* Protects adding/finding NEW keys *)
  }

  (* Create a new concurrent map *)
  let create size : t =
    { table = Hashtbl.create size; table_lock = Mutex.create () }

  (* Safely add a count for a given word *)
  let add_count (map : t) (word : string) (count : int) : unit =
    (* First, try to find the key without the main table lock *)
    match Hashtbl.find_opt map.table word with
    | Some data ->
        (* Key exists: Use the specific lock for this value *)
        Mutex.lock data.value_lock;
        data.counts <- count :: data.counts; (* Prepend to the list *)
        Mutex.unlock data.value_lock
    | None ->
        (* Key might not exist. We need to lock the main table to safely check again and potentially add. *)
        Mutex.lock map.table_lock;
        (* DOUBLE CHECK: Check again inside the lock in case another thread added it *)
        match Hashtbl.find_opt map.table word with
        | Some data ->
            (* Another thread added it while we waited for the table_lock. Release table lock and use the value lock. *)
            Mutex.unlock map.table_lock;
            Mutex.lock data.value_lock;
            data.counts <- count :: data.counts;
            Mutex.unlock data.value_lock
        | None ->
            (* Still doesn't exist, and we hold the table lock. Safe to add now. *)
            let new_value_data = { counts = [ count ]; value_lock = Mutex.create () } in
            Hashtbl.add map.table word new_value_data;
            Mutex.unlock map.table_lock (* Release table lock *after* adding *)
  ;;

  (* Get a snapshot of the final counts. Assumes adds are finished or locks appropriately. *)
  let get_final_counts (map : t) : (string * int list) list =
     Mutex.lock map.table_lock; (* Lock table briefly to prevent structural changes during fold *)
     let result = Hashtbl.fold (fun word data acc ->
         (* We should lock the value_lock to safely read the list,
            especially if adds could still theoretically happen, though
            typically get_final_counts is called after adds are done. *)
         Mutex.lock data.value_lock;
         let current_counts = data.counts in
         Mutex.unlock data.value_lock;
         (word, current_counts) :: acc
       ) map.table []
     in
     Mutex.unlock map.table_lock;
     result
   ;;

end

let run_test () =
  let num_domains = 4 in
  let pool = Task.setup_pool ~num_domains () in
  let concurrent_map = ConcurrentWordMap.create 10 in
  let data_to_process = [ ("test", 0); ("test", 1); ("hello", 5); ("test", 2); ("hello", 3) ] in

  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(List.length data_to_process - 1) ~body:(fun i ->
      let word, value = List.nth data_to_process i in
      ConcurrentWordMap.add_count concurrent_map word value;
      Printf.printf "Domain %d processed item %d: (%s, %d)\n" (Domain.self () :> int) i word value;
      flush stdout;
    )
  );

  (* Get the results - should contain lists of counts added *)
  let final_counts = ConcurrentWordMap.get_final_counts concurrent_map in

  (* Optional: Aggregate the counts for printing *)
  let aggregated_results = List.map (fun (word, counts_list) ->
      (word, List.fold_left (+) 0 counts_list)
    ) final_counts in

  Printf.printf "\nTasks finished.\n";
  Printf.printf "Final raw counts:\n";
  List.iter (fun (k, v_list) -> Printf.printf "  %s: [%s]\n" k (String.concat "; " (List.map string_of_int v_list))) final_counts;
  Printf.printf "Final aggregated counts:\n";
  List.iter (fun (k, v) -> Printf.printf "  %s: %d\n" k v) aggregated_results;

  Task.teardown_pool pool
;; (* Ensure the file ends correctly *)

*)

(* shuffle stage in dataflow: input is a list of (key, value) pairs, output is a list of (key, list of values) pairs *)

let shuffle_parallel (input : (('a * 'b) list) list) : ('a * 'b list) list =
  let map = Concurrent_hashmap.ConcurrentHashMap.create ~expected_size:10 () in
  let input_array = Array.of_list input in
  let n = Array.length input_array in
  let num_domains = n in
  let pool = Task.setup_pool ~num_domains () in

  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
      let inner_list = input_array.(i) in
      List.iter (fun (key, value) ->
        Concurrent_hashmap.ConcurrentHashMap.insert map key value
      ) inner_list
    )
  );

  Task.teardown_pool pool;

  List.map (fun key ->
  let values = (Concurrent_hashmap.ConcurrentHashMap.read map key) in
  (key, values)
) (Concurrent_hashmap.ConcurrentHashMap.keys map)

;;

let shuffle_sequential (input : (('a * 'b) list) list) : ('a * 'b list) list =
  let map = Hashtbl.create 10 in
  List.iter (fun inner_list ->
    List.iter (fun (key, value) ->
      Hashtbl.add map key value
    ) inner_list
  ) input;
  let key_list = Hashtbl.fold (fun key _value acc -> key :: acc) map [] in
  let unique_keys = List.sort_uniq compare key_list in
  List.map (fun key ->
    (key, Hashtbl.find_all map key)
  ) unique_keys
;;
