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

  (* Create tasks *)
  let tasks = List.init 10 (fun i ->
    Domainslib.Task.async pool (fun () ->
      for _ = 1 to 1000 do
        ConcurrentHashMap.insert map (string_of_int i) [i]  (* Insert list for each key *)
      done
    )
  ) in

  (* Wait for all tasks to complete using List.iter *)
  List.iter (fun task -> ignore (Domainslib.Task.await pool task)) tasks;

  (* Teardown the pool *)
  Domainslib.Task.teardown_pool pool;

  (* Check if all keys are inserted *)
  let all_keys = ConcurrentHashMap.keys map in
  if List.length all_keys = 10 then
    print_endline "✅ test_concurrent_inserts passed"
  else
    failwith "❌ test_concurrent_inserts failed"


(* Run all tests *)
let () =
  test_basic_insert_read ();
  test_read_missing_key ();
  test_insert_delete ();
  test_overwrite_key ();
  test_concurrent_inserts ();
