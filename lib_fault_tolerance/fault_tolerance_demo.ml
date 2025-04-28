open Domainslib
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

(* wordcount mapreduce funcs *)
let prepare_wordcount_input file_path =
  let content = read_file file_path in
  let lines = String.split_on_char '\n' content in
  let chunks = 
    lines |> List.map (String.split_on_char ' ') 
          |> List.filter (fun chunk -> chunk <> [""])
  in
  `List (List.map (fun chunk -> 
    `List (List.map (fun word -> `String word) chunk)
  ) chunks)

(* wordcount map func *)
let wordcount_map json_input =
  let word_lists = match json_input with
    | `List chunks -> 
        List.map (function 
          | `List words -> List.map to_string words
          | _ -> []) chunks
    | _ -> []
  in
  
  let word_counts = List.fold_left (fun acc chunk ->
    List.fold_left (fun acc word ->
      let count = try acc |> List.assoc word |> to_int with Not_found -> 0 in
      let filtered = List.filter (fun (w, _) -> w <> word) acc in
      (word, `Int (count + 1)) :: filtered
    ) acc chunk
  ) [] word_lists in
  
  `Assoc word_counts

(* wordcount reduce func *)
let wordcount_reduce _key values =
  let total = List.fold_left (fun acc value ->
    acc + to_int value
  ) 0 values in
  `Int total

(* dataflow wordcount funcs *)
let prepare_wordcount_dataflow_input file_path =
  let content = read_file file_path in
  let lines = String.split_on_char '\n' content in
  let words = List.concat_map (String.split_on_char ' ') lines |> 
              List.filter (fun w -> w <> "") in
  `List (List.map (fun word -> `String word) words)

(* computation node *)
let wordcount_transform json_word =
  let word = to_string json_word in
  (* For dataflow, return [key, value] pairs *)
  `List [`String word; `Int 1]

(* filter node *)
let wordcount_filter json_item =
  match json_item with
  | `List [`String word; `Int _] -> String.length word > 0
  | _ -> false

(* result display *)
let verify_wordcount_output output =
  match output with
  | `Assoc items ->
    Printf.printf "Word count results (sample of up to 10 words):\n";
    (* Sort items alphabetically by word *)
    let sorted_items = List.sort (fun (word1, _) (word2, _) -> String.compare word1 word2) items in
    let sample = if List.length sorted_items > 10 then List.take 10 sorted_items else sorted_items in
    List.iter (fun (word, count) ->
      Printf.printf "  %s: %d\n" word (to_int count)
    ) sample;
    Printf.printf "Total unique words: %d\n" (List.length items)
  | _ -> Printf.printf "Output verification failed: invalid format\n"

(* master gonna run forever, suppress warnings about that (not very ocaml-like but thats okay) *)
[@@@ocaml.warning "-21"]

let run_mapreduce_demo () =
  let data_file = "data/wordcountdata_medium.txt" in
  Printf.printf "Processing file: %s\n" data_file;
  let input_data = prepare_wordcount_input data_file in
  
  (* set up master *)
  let state = Master.create_master_state 5.0 in
  
  (* run map task *)
  Printf.printf "Starting map task...\n";
  let map_task = ("map", Master.MapReduceTask {
    phase = "map";
    input = input_data;
    map_func = Some wordcount_map;
    reduce_func = None;
  }) in
  
  Queue.push map_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Map task completed\n";
  
  (* run shuffle with map output *)
  Printf.printf "Starting shuffle task...\n";
  let map_output = Hashtbl.find state.completed_tasks "map" in
  
  let shuffle_task = ("shuffle", Master.MapReduceTask {
    phase = "shuffle";
    input = `List [map_output];
    map_func = None;
    reduce_func = None;
  }) in
  
  Queue.push shuffle_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Shuffle task completed\n";
  
  (* run reduce with shuffle output *)
  Printf.printf "Starting reduce task...\n";
  let shuffle_output = Hashtbl.find state.completed_tasks "shuffle" in
  let reduce_task = ("reduce", Master.MapReduceTask {
    phase = "reduce";
    input = shuffle_output;
    map_func = None;
    reduce_func = Some wordcount_reduce;
  }) in
  
  Queue.push reduce_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Reduce task completed\n";
  
  (* verify output *)
  let final_output = Hashtbl.find state.completed_tasks "reduce" in
  verify_wordcount_output final_output;
  
  (* cleanup *)
  Task.teardown_pool state.pool

let run_dataflow_demo () =
  let data_file = "data/wordcountdata_medium.txt" in
  Printf.printf "Processing file: %s\n" data_file;
  let input_data = prepare_wordcount_dataflow_input data_file in
  
  (* set up master *)
  let state = Master.create_master_state 5.0 in
  
  (* run computation task (map) *)
  Printf.printf "Starting computation task...\n";
  let computation_task = ("computation", Master.DataflowTask {
    node_type = "computation";
    input = input_data;
    transform = Some wordcount_transform;
    pred = None;
  }) in
  
  Queue.push computation_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Computation task completed\n";
  
  (* run filter with computation output *)
  Printf.printf "Starting filter task...\n";
  let computation_output = Hashtbl.find state.completed_tasks "computation" in
  
  let filter_task = ("filter", Master.DataflowTask {
    node_type = "filter";
    input = computation_output;
    transform = None;
    pred = Some wordcount_filter;
  }) in
  
  Queue.push filter_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Filter task completed\n";
  
  (* run groupby with filter output *)
  Printf.printf "Starting groupby task...\n";
  let filter_output = Hashtbl.find state.completed_tasks "filter" in
  
  let groupby_task = ("groupby", Master.DataflowTask {
    node_type = "groupby";
    input = filter_output;
    transform = None;
    pred = None;
  }) in
  
  Queue.push groupby_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Groupby task completed\n";
  
  (* format output for verification *)
  let groupby_output = Hashtbl.find state.completed_tasks "groupby" in
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
  
  (* cleanup *)
  Task.teardown_pool state.pool

let run_fault_tolerance_demo () =
  let data_file = "data/wordcountdata_medium.txt" in
  Printf.printf "Processing file: %s\n" data_file;
  let input_data = prepare_wordcount_input data_file in
  
  (* set up the master *)
  let state = Master.create_master_state 5.0 in
  
  (* run map task *)
  Printf.printf "Starting map task...\n";
  let map_task = ("map", Master.MapReduceTask {
    phase = "map";
    input = input_data;
    map_func = Some wordcount_map;
    reduce_func = None;
  }) in
  
  Queue.push map_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Map task completed\n";
  
  (* run shuffle with failure sim *)
  Printf.printf "Starting shuffle task with fault simulation...\n";
  let map_output = Hashtbl.find state.completed_tasks "map" in
  
  let shuffle_task = ("shuffle", Master.MapReduceTask {
    phase = "shuffle";
    input = `List [map_output];
    map_func = None;
    reduce_func = None;
  }) in
  
  (* queue task and sim failure *)
  Queue.push shuffle_task state.task_queue;
  
  (* start worker then fail it *)
  state.simulate_worker_failure <- true;
  
  (* monitor will requeue the task *)
  Master.monitor_workers state;
  Printf.printf "Shuffle task failed and was requeued\n";
  
  (* reset flag and run again *)
  state.simulate_worker_failure <- false;
  Master.monitor_workers state;
  Printf.printf "Shuffle task completed after recovery\n";
  
  (* run reduce with shuffle output *)
  Printf.printf "Starting reduce task...\n";
  let shuffle_output = Hashtbl.find state.completed_tasks "shuffle" in
  let reduce_task = ("reduce", Master.MapReduceTask {
    phase = "reduce";
    input = shuffle_output;
    map_func = None;
    reduce_func = Some wordcount_reduce;
  }) in
  
  Queue.push reduce_task state.task_queue;
  Master.monitor_workers state;
  Printf.printf "Reduce task completed\n";
  
  (* verify output *)
  let final_output = Hashtbl.find state.completed_tasks "reduce" in
  verify_wordcount_output final_output;
  
  (* cleanup *)
  Task.teardown_pool state.pool

(* main removed - run from bin/main.ml *) 