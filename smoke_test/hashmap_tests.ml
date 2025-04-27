open Dataflowlib.Concurrent_hashmap

(* Test 1: Basic insertion and read *)
let test_basic_insert_read () =
  let map = ConcurrentHashMap.create () in
  ConcurrentHashMap.insert map "hello" [1];  (* Insert a list with one value *)
  match ConcurrentHashMap.read map "hello" with
  | [[1]] -> print_endline "✅ test_basic_insert_read passed"
  | _ -> failwith "❌ test_basic_insert_read failed"

(* Test 2: Read missing key returns None or default *)
let test_read_missing_key () =
  let map = ConcurrentHashMap.create () in
  match ConcurrentHashMap.read map "world" with
  | [] -> print_endline "✅ test_read_missing_key passed"
  | _ -> failwith "❌ test_read_missing_key failed"

(* Test 3: Insert then delete *)
let test_insert_delete () =
  let map = ConcurrentHashMap.create () in
  ConcurrentHashMap.insert map "key" 42;  (* Insert a list with one value *)
  ConcurrentHashMap.delete map "key";      (* Delete the key *)
  match ConcurrentHashMap.read map "key" with
  | [] -> print_endline "✅ test_insert_delete passed"
  | _ -> failwith "❌ test_insert_delete failed"

(* Test 4: Overwrite existing key with a new list *)
let test_overwrite_key () =
  let map = ConcurrentHashMap.create () in
  ConcurrentHashMap.insert map "dup" 1;  (* Insert initial value *)
  ConcurrentHashMap.insert map "dup" 2;  (* Insert new value *)
  match ConcurrentHashMap.read map "dup" with
  | [1; 2] -> print_endline "✅ test_overwrite_key passed"
  | e -> (List.iter(fun x -> print_int x; print_char ' ') e); print_endline ""; failwith "❌ test_overwrite_key failed"

(* Test 5: Concurrent inserts *)
let test_concurrent_inserts () =
  let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
  let map = ConcurrentHashMap.create () in

  (* Run everything inside Task.run *)
  Domainslib.Task.run pool (fun _ ->
    (* Create tasks *)
    let tasks = List.init 10 (fun i ->
      Domainslib.Task.async pool (fun () ->
        for _ = 1 to 1000 do
          ConcurrentHashMap.insert map (string_of_int i) [i]
        done
      )
    ) in

    (* Wait for all tasks to complete *)
    List.iter (fun task -> ignore (Domainslib.Task.await pool task)) tasks
  );

  (* After run block is done, safe to teardown *)
  Domainslib.Task.teardown_pool pool;

  (* Check if all keys are inserted *)
  let all_keys = ConcurrentHashMap.keys map in
  if List.length all_keys = 10 then
    print_endline "✅ test_concurrent_inserts passed"
  else
    failwith "❌ test_concurrent_inserts failed"

(* timer helper *)
let time f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let finish = Unix.gettimeofday () in
  (finish -. start, result)

(* Test 6: Speedup of ConcurrentHashMap vs regular Map *)
let test_speedup () =
  let num_tasks = 10 in
  let num_inserts_per_task = 1_000_000 in

  (* ConcurrentHashMap test *)
  let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
  let cmap = ConcurrentHashMap.create () in

  let concurrent_time, _ = time (fun () ->
    Domainslib.Task.run pool (fun _ ->
      let tasks = List.init num_tasks (fun i ->
        Domainslib.Task.async pool (fun () ->
          for j = 1 to num_inserts_per_task do
            ConcurrentHashMap.insert cmap (Printf.sprintf "%d-%d" i j) [i; j]
          done
        )
      ) in
      List.iter (fun task -> ignore (Domainslib.Task.await pool task)) tasks
    )
  ) 
  in
  Domainslib.Task.teardown_pool pool;

  (* Regular Map test *)
  let module M = Map.Make(String) in  (* This should be outside the 'let' block *)
  let map = ref M.empty in

  let regular_time, _ = time (fun () ->
    let tasks = List.init num_tasks (fun i ->
      (* Just sequential loops, no real parallelism *)
      for j = 1 to num_inserts_per_task do
        map := M.add (Printf.sprintf "%d-%d" i j) [i; j] !map
      done
    ) in
    ignore tasks
  ) 
  in

  (* Report results *)
  Printf.printf "\n===== Speed Comparison =====\n";
  Printf.printf "ConcurrentHashMap insert time: %.4f seconds\n" concurrent_time;
  Printf.printf "Regular Map insert time: %.4f seconds\n" regular_time;
  if concurrent_time < regular_time then
    Printf.printf "✅ ConcurrentHashMap was faster by %.2fx\n" (regular_time /. concurrent_time)
  else
    Printf.printf "⚠️  ConcurrentHashMap was slower by %.2fx\n" (concurrent_time /. regular_time)


    

(* Run all tests *)
let () =
  test_basic_insert_read ();
  test_read_missing_key ();
  test_insert_delete ();
  test_overwrite_key ();
  test_concurrent_inserts ();
  test_speedup ();
