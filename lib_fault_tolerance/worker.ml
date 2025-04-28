open Dataflowlib
open Domainslib

module StringMapReduce = Mapreduce_parallel.MakeParallelMapReduce(String)
module Computation = Nodes.Computation
module Filter = Nodes.Filter
module Groupby = Nodes.Groupby

type worker_config = {
  task_type: string;  (* mapreduce or dataflow *)
  phase: string;      (* for mapreduce: map, shuffle, reduce *)
  node_type: string;  (* for dataflow: computation, filter, groupby *)
  task_id: string;
  input_file: string;
  output_file: string;
  master_pid: int;
}

let read_config () =
  if Array.length Sys.argv < 7 then
    failwith "Usage: worker <task_type> <phase/node_type> <task_id> <input_file> <output_file> <master_pid>"
  else {
    task_type = Sys.argv.(1);
    phase = Sys.argv.(2);
    node_type = Sys.argv.(2);
    task_id = Sys.argv.(3);
    input_file = Sys.argv.(4);
    output_file = Sys.argv.(5);
    master_pid = int_of_string Sys.argv.(6);
  }

let send_heartbeat master_pid =
  let pipe_name = Printf.sprintf "/tmp/worker_%d" master_pid in
  let oc = open_out pipe_name in
  output_string oc (string_of_int (Unix.getpid ()));
  close_out oc;
  Unix.kill master_pid Sys.sigusr1

let send_completion master_pid output_file =
  let pipe_name = Printf.sprintf "/tmp/worker_%d" master_pid in
  let oc = open_out pipe_name in
  output_string oc (string_of_int (Unix.getpid ()));
  output_string oc "\n";
  output_string oc output_file;
  close_out oc;
  Unix.kill master_pid Sys.sigusr2

let process_mapreduce_task config =
  let input = Yojson.Basic.from_file config.input_file in
  let pool = Task.setup_pool ~num_domains:4 () in
  let output = match config.phase with
    | "map" -> 
        (* convert input to list if it's a json list *)
        let input_list = match input with
          | `List items -> items
          | _ -> failwith "Map expects a JSON list input"
        in
        let result = StringMapReduce.parallel_map pool (fun x -> x) input_list in
        `List result
    | "shuffle" -> 
        let maps = match input with
          | `List maps -> List.map (fun m -> match m with `Assoc m -> StringMapReduce.KMap.of_seq (List.to_seq m) | _ -> failwith "Invalid map format") maps
          | _ -> failwith "Invalid input format for shuffle"
        in
        let result = StringMapReduce.shuffle_task_runner maps in
        (* convert map values to proper json lists *)
        let json_assoc = List.map (fun (k, vs) -> 
          (k, `List vs)
        ) (List.of_seq (StringMapReduce.KMap.to_seq result)) in
        `Assoc json_assoc
    | "reduce" -> 
        (* simplified reduce impl *)
        `List (match input with | `List l -> l | _ -> failwith "Reduce expects a JSON list input")
    | _ -> failwith "Invalid MapReduce phase"
  in
  Task.teardown_pool pool;
  Yojson.Basic.to_file config.output_file output

let process_dataflow_task config =
  let input = Yojson.Basic.from_file config.input_file in
  let output = match config.node_type with
    | "computation" -> 
        (* convert json input to computation record *)
        let items = match input with
          | `List items -> items
          | _ -> failwith "Computation expects a JSON list input"
        in
        let node : ('a, 'b) Computation.t = {
          input = items;
          transform = (fun x -> x) (* identity transform as simple example *)
        } in
        let result = Computation.run_computation node in
        `List result
    | "filter" -> 
        (* convert json input to filter record *)
        let items = match input with
          | `List items -> items
          | _ -> failwith "Filter expects a JSON list input"
        in
        let node : 'a Filter.filter = {
          input = items;
          pred = (fun _ -> true) (* accept all as simple example *)
        } in
        let result = Filter.run_filter node in
        `List result
    | "groupby" ->
        (* convert json input to groupby record *)
        let items = match input with
          | `List items -> 
              (* convert each item to key-value pair *)
              List.map (fun item ->
                match item with
                | `List [key; value] -> (key, value)
                | _ -> failwith "Expected [key, value] pairs"
              ) items
          | _ -> failwith "Groupby expects a JSON list of pairs"
        in
        let node : ('a, 'b) Groupby.groupby = {
          input = items
        } in
        let result = Groupby.run_groupby node in
        (* convert back to json *)
        `List (List.map (fun (key, values) -> 
          `List [key; `List values]
        ) result)
    | _ -> failwith "Invalid dataflow node type"
  in
  Yojson.Basic.to_file config.output_file output

let run_worker () =
  let config = read_config () in
  let pool = Task.setup_pool ~num_domains:4 () in
  
  (* setup periodic heartbeat with stop flag *)
  let stop_heartbeat = ref false in
  ignore (Task.async pool (fun () ->
    let rec loop () =
      if not !stop_heartbeat then (
        send_heartbeat config.master_pid;
        Unix.sleepf 1.0;
        loop ()
      )
    in
    loop ()
  ));
  
  try
    (* process the task *)
    if config.task_type = "mapreduce" then
      process_mapreduce_task config
    else
      process_dataflow_task config;
    
    (* send completion signal *)
    send_completion config.master_pid config.output_file;
    
    (* stop heartbeat thread *)
    stop_heartbeat := true;
    Task.teardown_pool pool
  with e ->
    (* stop heartbeat thread *)
    stop_heartbeat := true;
    Task.teardown_pool pool;
    raise e

let () = run_worker () 