open Yojson.Basic.Util

(* file utils *)
let read_file file_path =
  let ch = open_in file_path in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let write_json_file file_path json =
  let ch = open_out file_path in
  Yojson.Basic.to_channel ch json;
  close_out ch

let read_json_file file_path =
  let ch = open_in file_path in
  let json = Yojson.Basic.from_channel ch in
  close_in ch;
  json

(* create tiny sample for testing *)
let create_tiny_sample file_path =
  let content = read_file file_path in
  let lines = String.split_on_char '\n' content 
              |> List.filter (fun s -> s <> "") in
  (* Just take the first 50 lines *)
  let sample_lines = 
    let rec take n lst acc =
      if n = 0 || lst = [] then List.rev acc
      else take (n-1) (List.tl lst) (List.hd lst :: acc)
    in
    take 50 lines []
  in
  String.concat "\n" sample_lines

(* read input file and format it *)
let prepare_wordcount_input file_path =
  let content = read_file file_path in
  let lines = String.split_on_char '\n' content in
  let chunks = 
    lines |> List.map (String.split_on_char ' ') 
          |> List.filter (fun chunk -> chunk <> [""])
          |> List.map (List.filter (fun w -> w <> ""))
  in
  `List (List.map (fun chunk -> 
    `List (List.map (fun word -> `String word) chunk)
  ) chunks)

(* create wordcount map (MAP function) *)
let wordcount_map json_input =
  let word_lists = match json_input with
    | `List chunks -> 
        List.map (function 
          | `List words -> List.map to_string words
          | _ -> []) chunks
    | _ -> []
  in
  
  (* mapping function calculates counts per chunk (different than other example) *)
  let word_prefix = "word" in
  
  let word_counts = List.fold_left (fun acc chunk ->
    List.fold_left (fun acc word ->
      if String.length word >= String.length word_prefix && 
         String.sub word 0 (String.length word_prefix) = word_prefix then
        let count = try acc |> List.assoc word |> to_int with Not_found -> 0 in
        let filtered = List.filter (fun (w, _) -> w <> word) acc in
        (word, `Int (count + 1)) :: filtered
      else
        acc
    ) acc chunk
  ) [] word_lists in
  
  `Assoc word_counts

(* REDUCE function, fold with sum *)
let wordcount_reduce _key values =
  let total = List.fold_left (fun acc value ->
    acc + to_int value
  ) 0 values in
  `Int total

(* print first 10 lexicographical words and their counts *)
let verify_wordcount_output output =
  match output with
  | `Assoc items ->
    let total_words = List.length items in
    Printf.printf "Word count results (showing first 10 of %d words):\n" total_words;
    (* Sort items alphabetically by word *)
    let sorted_items = List.sort (fun (word1, _) (word2, _) -> String.compare word1 word2) items in
    let sample = if total_words > 10 then List.take 10 sorted_items else sorted_items in
    List.iter (fun (word, count) ->
      Printf.printf "  %s: %d\n" word (to_int count)
    ) sample;
    Printf.printf "Total unique words: %d\n" total_words
  | _ -> Printf.printf "Output verification failed: invalid format\n"

(* demo *)
let run_realistic_fault_tolerance_demo () =
  let data_file = "data/wordcountdata_medium.txt" in
  Printf.printf "Processing file: %s\n" data_file;
  
  let input_data = prepare_wordcount_input data_file in
  Printf.printf "Input data prepared successfully\n";
  
  (* set up master, 500ms timeout *)
  let master = Master.create_master_state 0.5 in
  
  (* start monitor, checks every 200ms *)
  let _ = Master.start_background_monitor master 0.2 in
  Printf.printf "Started background monitoring (checks every 0.2 seconds)\n";
  
  Printf.printf "\n=== Running MapReduce with Simulated Worker Failure ===\n";
  
  (* run map and simulate map failure *)
  Printf.printf "Starting map phase with simulated worker failure...\n";
  let map_task = ("map", Master.MapReduceTask {
    phase = "map";
    input = input_data;
    map_func = Some wordcount_map;
    reduce_func = None;
  }) in
  
  (* queue map *)
  Master.queue_task master map_task;
  
  (* simulate map failure *)
  master.simulate_worker_failure <- true;
  Thread.delay 0.5; (* Give time for the task to start *)
  
  (* wait for automatic map retry *)
  while not (Hashtbl.mem master.completed_tasks "map") do
    Thread.delay 0.1;
  done;
  
  (* map done *)
  let map_output = Hashtbl.find master.completed_tasks "map" in
  Printf.printf "Map task completed: ";
  (match map_output with
   | `Assoc items -> Printf.printf "%d words counted\n" (List.length items)
   | _ -> Printf.printf "unexpected format\n");
  
  (* run shuffle and simulate shuffle failure *)
  Printf.printf "\nStarting shuffle phase with simulated worker failure...\n";
  let shuffle_task = ("shuffle", Master.MapReduceTask {
    phase = "shuffle";
    input = `List [map_output];
    map_func = None;
    reduce_func = None;
  }) in
  
  (* queue shuffle *)
  Master.queue_task master shuffle_task;
  
  (* simulate shuffle failure *)
  master.simulate_worker_failure <- true;
  Thread.delay 0.5;
  
  (* wait for shuffle automatic retry *)
  while not (Hashtbl.mem master.completed_tasks "shuffle") do
    Thread.delay 0.1;
  done;
  
  (* shuffle done *)
  let shuffle_output = Hashtbl.find master.completed_tasks "shuffle" in
  Printf.printf "Shuffle task completed: ";
  (match shuffle_output with
   | `Assoc items -> Printf.printf "%d keys shuffled\n" (List.length items)
   | _ -> Printf.printf "unexpected format\n");
  
  (* run reduce and simulate reduce failure *)
  Printf.printf "\nStarting reduce phase with simulated worker failure...\n";
  let reduce_task = ("reduce", Master.MapReduceTask {
    phase = "reduce";
    input = shuffle_output;
    map_func = None;
    reduce_func = Some wordcount_reduce;
  }) in
  
  (* queue reduce *)
  Master.queue_task master reduce_task;
  
  (* simulate reduce failure *)
  master.simulate_worker_failure <- true;
  Thread.delay 0.5;
  
  (* wait for reduce automatic retry *)
  while not (Hashtbl.mem master.completed_tasks "reduce") do
    Thread.delay 0.1;
  done;
  
  (* verify and print output *)
  let reduce_output = Hashtbl.find master.completed_tasks "reduce" in
  Printf.printf "Reduce task completed\n\nFinal Output:\n";
  verify_wordcount_output reduce_output;
  
  (* stop monitor before exit *)
  let _ = Master.stop_background_monitor master in
  
  Printf.printf "\nMapReduce with fault tolerance completed successfully!\n"




(* DATAFLOW EXAMPLE IMPLEMENTATION WITH FAULT TOLERANCE BELOW *)




(* computation node for dataflow *)
let wordcount_transform json_word =
  let word = match json_word with
    | `String w -> w
    | _ -> Yojson.Basic.to_string json_word
  in
  `List [`String word; `Int 1]

(* filter node for dataflow *)
let wordcount_filter json_item =
  match json_item with
  | `List [`String word; `Int _] -> String.length word > 0
  | _ -> false

(* run realistic fault tolerance demo with dataflow *)
let run_realistic_dataflow_demo () =
  let data_file = "data/wordcountdata_medium.txt" in
  Printf.printf "Processing file: %s for dataflow\n" data_file;
  
  let input_data = prepare_wordcount_input data_file in
  Printf.printf "Input data prepared successfully\n";
  
  (* create master 500ms timeout for heartbeat ping *)
  let master = Master.create_master_state 0.5 in
  
  (* start monitor, checks every 200ms *)
  let _ = Master.start_background_monitor master 0.2 in
  Printf.printf "Started background monitoring (checks every 0.2 seconds)\n";
  
  Printf.printf "\n=== Running Dataflow with Simulated Worker Failure ===\n";
  
  (* run computation and simulate computation failure *)
  Printf.printf "Starting computation task with simulated worker failure...\n";
  let computation_task = ("computation", Master.DataflowTask {
    node_type = "computation";
    input = input_data;
    transform = Some wordcount_transform;
    pred = None;
  }) in
  
  (* queue computation *)
  Master.queue_task master computation_task;
  
  (* simulate computation failure *)
  master.simulate_worker_failure <- true;
  Thread.delay 0.5; (* Give time for the task to start *)
  
  (* wait for automatic retry in computation *)
  while not (Hashtbl.mem master.completed_tasks "computation") do
    Thread.delay 0.1;
  done;
  
  let computation_output = Hashtbl.find master.completed_tasks "computation" in
  Printf.printf "Computation task completed\n";
  
  (* run filter and simulate filter failure *)
  Printf.printf "\nStarting filter task with simulated worker failure...\n";
  let filter_task = ("filter", Master.DataflowTask {
    node_type = "filter";
    input = computation_output;
    transform = None;
    pred = Some wordcount_filter;
  }) in
  
  (* queue filter *)
  Master.queue_task master filter_task;
  
  (* simulate filter failure *)
  master.simulate_worker_failure <- true;
  Thread.delay 0.5; (* Give time for the task to start *)
  
  (* wait for automatic retry in filter *)
  while not (Hashtbl.mem master.completed_tasks "filter") do
    Thread.delay 0.1;
  done;
  
  let filter_output = Hashtbl.find master.completed_tasks "filter" in
  Printf.printf "Filter task completed\n";
  
  (* run groupby and simulate groupby failure *)
  Printf.printf "\nStarting groupby task with simulated worker failure...\n";
  let groupby_task = ("groupby", Master.DataflowTask {
    node_type = "groupby";
    input = filter_output;
    transform = None;
    pred = None;
  }) in
  
  (* queue groupby *)
  Master.queue_task master groupby_task;
  
  (* simulate groupby failure *)
  master.simulate_worker_failure <- true;
  Thread.delay 0.5; (* Give time for the task to start *)
  
  (* wait for automatic retry in groupby *)
  while not (Hashtbl.mem master.completed_tasks "groupby") do
    Thread.delay 0.1;
  done;
  
  (* verify and print output *)
  let groupby_output = Hashtbl.find master.completed_tasks "groupby" in
  Printf.printf "Groupby task completed\n\nFinal Output:\n";
  let final_output = match groupby_output with
    | `Assoc bindings ->
        `Assoc (List.map (fun (key, values) ->
          let count = match values with
            | `List items -> List.length items
            | _ -> 0
          in
          (key, `Int count)
        ) bindings)
    | other -> other
  in
  
  verify_wordcount_output final_output;
  
  (* stop monitor thread before exit *)
  let _ = Master.stop_background_monitor master in
  
  Printf.printf "\nDataflow with fault tolerance completed successfully!\n"

(* main.ml wraps this part *)
let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "worker" then
    Printf.printf "running worker\n%!"
  else
    (* only master/demo here *)
    let () =
      Random.self_init ();
      Printf.printf "===== Running Realistic Fault Tolerance Demo =====\n%!"
    in
    run_realistic_fault_tolerance_demo ();
    Printf.printf "===== Running Realistic Dataflow Demo =====\n%!";
    run_realistic_dataflow_demo ()
