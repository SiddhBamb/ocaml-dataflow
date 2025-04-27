open Dataflowlib.Concurrent_hashmap
open Domainslib

(* test 1: basic insertion and read *)
let test_basic_insert_read () =
  let map = ConcurrentHashMap.create () in
  ConcurrentHashMap.insert map "hello" [1];  (* Insert a list with one value *)
  match ConcurrentHashMap.read map "hello" with
  | [[1]] -> print_endline "PASS test_basic_insert_read passed"
  | _ -> failwith "FAIL test_basic_insert_read failed"

(* test 2: read missing key returns empty list *)
let test_read_missing_key () =
  let map = ConcurrentHashMap.create () in
  match ConcurrentHashMap.read map "world" with
  | [] -> print_endline "PASS test_read_missing_key passed"
  | _ -> failwith "FAIL test_read_missing_key failed"

(* test 3: insert then delete *)
let test_insert_delete () =
  let map = ConcurrentHashMap.create () in
  ConcurrentHashMap.insert map "key" 42;  (* Insert a list with one value *)
  ConcurrentHashMap.delete map "key";      (* Delete the key *)
  match ConcurrentHashMap.read map "key" with
  | [] -> print_endline "PASS test_insert_delete passed"
  | _ -> failwith "FAIL test_insert_delete failed"

(* test 4: appending to the same list *)
let test_overwrite_key () =
  let map = ConcurrentHashMap.create () in
  ConcurrentHashMap.insert map "dup" 1;  (* Insert initial value *)
  ConcurrentHashMap.insert map "dup" 2;  (* Insert new value *)
  match ConcurrentHashMap.read map "dup" with
  | [1; 2] -> print_endline "PASS test_overwrite_key passed"
  | e -> (List.iter(fun x -> print_int x; print_char ' ') e); print_endline ""; failwith "FAIL test_overwrite_key failed"

(* test 5: correctness of concurrent inserts *)
let test_concurrent_inserts () =
  let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
  let map = ConcurrentHashMap.create () in

  Domainslib.Task.run pool (fun _ ->
    let tasks = List.init 10 (fun i ->
      Domainslib.Task.async pool (fun () ->
        for _ = 1 to 1000 do
          ConcurrentHashMap.insert map (string_of_int i) [i]
        done
      )
    ) in

    (* join all threads *)
    List.iter (fun task -> ignore (Domainslib.Task.await pool task)) tasks
  );
  Domainslib.Task.teardown_pool pool;

  (* check for correctness *)
  let all_keys = ConcurrentHashMap.keys map in
  if (List.length all_keys = 10) && (List.for_all (fun key -> List.length (ConcurrentHashMap.read map key) = 1000) all_keys) then
    print_endline "PASS test_concurrent_inserts passed"
  else
    failwith "FAIL test_concurrent_inserts failed"

(* timer helper *)
let time f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let finish = Unix.gettimeofday () in
  (finish -. start, result)

(* test 6: speedup of ConcurrentHashMap vs regular Map *)
let test_speedup () =
  let num_tasks = 10 in
  let num_inserts_per_task = 100_000 in

  (* ConcurrentHashMap test *)
  let pool = Domainslib.Task.setup_pool ~num_domains:1 () in
  let cmap = ConcurrentHashMap.create ~expected_size:(1000) () in

  let concurrent_time, _ = time (fun () ->
    Domainslib.Task.run pool (fun _ ->
      let tasks = List.init num_tasks (fun i ->
        Domainslib.Task.async pool (fun () ->
          for j = 1 to num_inserts_per_task do
            ConcurrentHashMap.insert cmap (i * num_inserts_per_task + j) [i; j]
          done          
        )
      ) in
      List.iter (fun task -> ignore (Domainslib.Task.await pool task)) tasks
    )
  ) 
  in
  Domainslib.Task.teardown_pool pool;

  let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
  let module M = Map.Make(Int) in
  let seqmap = ref M.empty in
  let concurrent_with_regular_map_time, _ = time (fun () ->
    Domainslib.Task.run pool (fun _ ->
      let tasks = List.init num_tasks (fun i ->
        Domainslib.Task.async pool (fun () ->
          for j = 1 to num_inserts_per_task do
            seqmap := M.add (i * num_inserts_per_task + j) [i; j] !seqmap
          done
        )
      ) in
      List.iter (fun task -> ignore (Domainslib.Task.await pool task)) tasks
    )
  ) 
  in
  Domainslib.Task.teardown_pool pool;

  (* regular map test *)
  let module M = Map.Make(Int) in
  let map = ref M.empty in

  let regular_time, _ = time (fun () ->
    let tasks = List.init num_tasks (fun i ->
      for j = 1 to num_inserts_per_task do
        map := M.add (i * num_inserts_per_task + j) [i; j] !map
      done
    ) in
    ignore tasks
  ) 
  in

  (* report timing results *)
  Printf.printf "\n===== Timing Results =====\n";
  Printf.printf "ConcurrentHashMap insert time: %.4f seconds\n" concurrent_time;
  Printf.printf "Regular Map insert time (using threads): %.4f seconds\n" concurrent_with_regular_map_time;
  Printf.printf "Regular Map insert time: %.4f seconds\n" regular_time;
  if concurrent_time < regular_time then
    Printf.printf "PASS ConcurrentHashMap was faster by %.2fx\n" (regular_time /. concurrent_time)
  else
    Printf.printf "⚠️  ConcurrentHashMap was slower by %.2fx\n" (concurrent_time /. regular_time)




let test_compare () =
  let num_tasks             = 8 in
  let num_inserts_per_task  = 1_000_000 in
  let total                 = num_tasks * num_inserts_per_task in

  (* time single-threaded ConcurrentHashMap *)
  let chm_single_time, () = time (fun () ->
    let cmap = ConcurrentHashMap.create ~expected_size:total () in
    for i = 1 to total do
      ConcurrentHashMap.insert cmap i i
    done
  ) in

  (* time single-threaded hashtable *)
  let ht_single_time, () = time (fun () ->
    let ht = Hashtbl.create total in
    for i = 1 to total do
      Hashtbl.add ht i i
    done
  ) in

  (* time multi-threaded ConcurrentHashMap *)
  let pool = Task.setup_pool ~num_domains:8 () in
  let chm_multi_time, () = time (fun () ->
    let cmap = ConcurrentHashMap.create ~expected_size:total () in
    Task.run pool (fun _ ->
      let tasks = List.init num_tasks (fun t ->
        Task.async pool (fun () ->
          let base = (t-1) * num_inserts_per_task in
          for j = 1 to num_inserts_per_task do
            let key = base + j in
            ConcurrentHashMap.insert cmap key key
          done
        )
      ) in
      List.iter (fun tsk -> ignore (Task.await pool tsk)) tasks
    )
  ) in
  Task.teardown_pool pool;


  (* report timing results *)
  Printf.printf "\n===== Timing Results =====\n";
  Printf.printf "1) Single-thread CHM:     %.4f s\n" chm_single_time;
  Printf.printf "2) Single-thread Hashtbl: %.4f s\n" ht_single_time;
  Printf.printf "3) 4-threaded CHM:        %.4f s\n" chm_multi_time;
  Printf.printf "\nSpeed-up vs Hashtbl (CHM1): %.2fx\n"
    (ht_single_time /. chm_single_time);
  Printf.printf "Speed-up vs CHM1 (CHM4):     %.4fx\n"
    (chm_single_time /. chm_multi_time);
  Printf.printf "\n%!"    
    

(* run all tests *)
let () =
  test_basic_insert_read ();
  test_read_missing_key ();
  test_insert_delete ();
  test_overwrite_key ();
  test_concurrent_inserts ();
  test_speedup ();
  test_compare ();
