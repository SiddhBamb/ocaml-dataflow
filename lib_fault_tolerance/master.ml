open Domainslib

module Mapreduce_parallel = Dataflowlib.Mapreduce_parallel

type task_type =
  | MapReduceTask of {
      phase: string;  (* map/shuffle/reduce for mapreduce *)
      input: Yojson.Basic.t;  (* serialized json input object *)
      map_func: (Yojson.Basic.t -> Yojson.Basic.t) option;
      reduce_func: (Yojson.Basic.t -> Yojson.Basic.t list -> Yojson.Basic.t) option;
    }
  | DataflowTask of {
      node_type: string;  (* computation/filter/groupby for dataflow *)
      input: Yojson.Basic.t;
      transform: (Yojson.Basic.t -> Yojson.Basic.t) option;
      pred: (Yojson.Basic.t -> bool) option;
    }

type worker_status =
  | Running
  | Failed
  | Completed

(* stores info about what each worker is doing, uses above structs *)
type worker_info = {
  task_id: string;
  status: worker_status;
  start_time: float;
  last_heartbeat: float;
  task_type: task_type;
  output: Yojson.Basic.t option;
  pid: int option;  (* worker process id *)
}

type master_state = {
  workers: (string, worker_info) Hashtbl.t;  (* worker id -> worker_info *)
  task_queue: (string * task_type) Queue.t;  (* (worker_id, task_type) queue *)
  completed_tasks: (string, Yojson.Basic.t) Hashtbl.t;  (* task_id -> output *)
  heartbeat_timeout: float;
  pool: Task.pool;
  mutable simulate_worker_failure: bool;  (* flag to sim worker failure *)
  mutable monitor_running: bool;          (* flag to control background monitoring *)
  mutable monitoring_thread: Thread.t option; (* background monitoring thread *)
}

let create_master_state heartbeat_timeout =
  let pool = Task.setup_pool ~num_domains:4 () in
  {
    workers = Hashtbl.create 10;
    task_queue = Queue.create ();
    completed_tasks = Hashtbl.create 10;
    heartbeat_timeout;
    pool;
    simulate_worker_failure = false;
    monitor_running = false;
    monitoring_thread = None;
  }

let check_worker_health state worker_info =
  let now = Unix.gettimeofday () in
  if now -. worker_info.last_heartbeat > state.heartbeat_timeout then (
    Hashtbl.remove state.workers worker_info.task_id;
    Queue.push (worker_info.task_id, worker_info.task_type) state.task_queue;
    true
  ) else
    false

let handle_heartbeat state task_id =
  match Hashtbl.find_opt state.workers task_id with
  | Some worker_info ->
    let now = Unix.gettimeofday () in
    let updated_info = { worker_info with last_heartbeat = now } in
    Hashtbl.replace state.workers task_id updated_info;
    true
  | None -> false

let handle_task_completion state task_id output =
  match Hashtbl.find_opt state.workers task_id with
  | Some _ ->
    Hashtbl.add state.completed_tasks task_id output;
    Hashtbl.remove state.workers task_id;
    true
  | None -> false

(* wraps mapreduce node connections *)
let run_mapreduce_task state task_id task =
  match task with
  | MapReduceTask { phase; input; map_func; reduce_func } ->
    let module PMR = Mapreduce_parallel.MakeParallelMapReduce (String) in
    (match phase with
    | "map" ->
      let map_func = Option.get map_func in
      let result = map_func input in
      handle_task_completion state task_id result
    | "shuffle" ->
      let maps = match input with
        | `List maps -> List.map (fun m -> match m with `Assoc m -> PMR.KMap.of_seq (List.to_seq m) | _ -> failwith "Invalid map format") maps
        | _ -> failwith "Invalid input format for shuffle"
      in
      let result = PMR.shuffle_task_runner maps in
      let json_result = `Assoc (List.map (fun (k, vs) -> 
        (k, `List vs)
      ) (List.of_seq (PMR.KMap.to_seq result))) in
      handle_task_completion state task_id json_result
    | "reduce" ->
      let reduce_func = Option.get reduce_func in
      let bindings = match input with
        | `Assoc bindings -> bindings
        | _ -> failwith "Invalid input format for reduce"
      in
      let result = List.map (fun (k, v) ->
        match v with
        | `List lst -> k, reduce_func (`String k) lst
        | _ -> failwith "Invalid value format for reduce"
      ) bindings in
      let json_result = `Assoc result in
      handle_task_completion state task_id json_result
    | _ -> failwith "Invalid MapReduce phase")
  | DataflowTask _ ->
      failwith "Expected MapReduce task, got DataflowTask"

(* wraps dataflow node connections *)
let run_dataflow_task state task_id task =
  match task with
  | DataflowTask { node_type; input; transform; pred } ->
    (match node_type with
    | "computation" ->
      let transform = Option.get transform in
      let items = match input with
        | `List chunks -> 
            List.flatten (List.map (fun chunk ->
              match chunk with
              | `List words -> words
              | other -> [other]
            ) chunks)
        | _ -> failwith "Invalid input format for computation"
      in
      (* Use parallel computation from the nodes library *)
      let results = Nodes.Computation.run_computation ~num_domains:4 
        { input = items; transform } in
      handle_task_completion state task_id (`List results)
    | "filter" ->
      let pred = Option.get pred in
      let items = match input with
        | `List items -> items
        | _ -> failwith "Invalid input format for filter"
      in
      (* Use parallel filter from the nodes library *)
      let filtered = Nodes.Filter.run_filter ~num_domains:4
        { input = items; pred } in
      handle_task_completion state task_id (`List filtered)
    | "groupby" ->
      let items = match input with
        | `List items -> 
            (* Prepare items for groupby - should be (key, value) pairs *)
            List.map (fun item ->
              match item with
              | `List [key; value] -> (key, value)
              | _ -> failwith "Expected [key, value] pairs for groupby"
            ) items
        | _ -> failwith "Invalid input format for groupby"
      in
      (* Use parallel groupby from the nodes library *)
      let result = Nodes.Groupby.run_groupby ~num_domains:4 { input = items } in
      
      (* Convert to json with string keys *)
      let json_result = `Assoc (
        List.map (fun (key, values) ->
          let key_str = match key with
            | `String s -> s
            | _ -> Yojson.Basic.to_string key
          in
          (key_str, `List values)
        ) result
      ) in
      
      handle_task_completion state task_id json_result
    | _ -> failwith "Invalid dataflow node type")
  | MapReduceTask _ -> 
      failwith "Expected DataflowTask, got MapReduceTask"

let monitor_workers state =
  let now = Unix.gettimeofday () in
  let dead_workers = ref [] in
  
  (* check all workers *)
  Hashtbl.iter (fun task_id worker_info ->
    if check_worker_health state worker_info then
      dead_workers := task_id :: !dead_workers
  ) state.workers;
  
  (* remove dead workers *)
  List.iter (fun task_id -> Hashtbl.remove state.workers task_id) !dead_workers;
  
  (* process queued tasks *)
  while not (Queue.is_empty state.task_queue) && 
        Hashtbl.length state.workers < 4 (* max concurrent workers *) do
    let task_id, task_type = Queue.pop state.task_queue in
    let worker_info = {
      task_id;
      status = Running;
      start_time = now;
      last_heartbeat = now;
      task_type;
      output = None;
      pid = None;  (* simulation doesn't use real processes *)
    } in
    Hashtbl.add state.workers task_id worker_info;
    
    (* execute task directly *)
    print_endline ("Executing task: " ^ task_id);
    
    (* simulate worker failure if flag is set *)
    if state.simulate_worker_failure then begin
      print_endline ("Simulating worker failure for task: " ^ task_id);
      state.simulate_worker_failure <- false;  (* reset flag after using once *)
      Queue.push (task_id, task_type) state.task_queue;  (* requeue task *)
      Hashtbl.remove state.workers task_id;
    end else begin
      let success = match task_type with
      | MapReduceTask _ -> run_mapreduce_task state task_id task_type
      | DataflowTask _ -> run_dataflow_task state task_id task_type
      in
      
      if success then
        print_endline ("Task completed: " ^ task_id)
      else
        print_endline ("Task failed: " ^ task_id);
      
      (* remove worker after completion *)
      Hashtbl.remove state.workers task_id;
    end
  done

(* realistic monitor that actually spawns worker processes *)
let monitor_workers_realistic state =
  let now = Unix.gettimeofday () in
  let dead_workers = ref [] in
  
  (* check for heartbeat timeouts *)
  Hashtbl.iter (fun task_id worker_info ->
    if now -. worker_info.last_heartbeat > state.heartbeat_timeout then (
      Printf.printf "Worker timeout detected for task %s (last heartbeat: %.1f seconds ago)\n"
        task_id (now -. worker_info.last_heartbeat);
      
      (* kill worker process if still running *)
      (match worker_info.pid with
      | Some pid -> 
          (try Unix.kill pid Sys.sigkill with _ -> ());
          Printf.printf "Killed worker process %d for task %s\n" pid task_id
      | None -> ());
      
      (* mark for removal and requeue *)
      dead_workers := task_id :: !dead_workers;
      Queue.push (task_id, worker_info.task_type) state.task_queue;
    )
  ) state.workers;
  
  (* remove dead workers *)
  List.iter (fun task_id -> Hashtbl.remove state.workers task_id) !dead_workers;
  
  (* process queued tasks - spawn worker threads *)
  while not (Queue.is_empty state.task_queue) && 
        Hashtbl.length state.workers < 4 (* max concurrent workers *) do
    let task_id, task_type = Queue.pop state.task_queue in
    
    (* create temp files for input/output & fast recovery *)
    let temp_dir = Sys.getcwd() in
    let input_file = Filename.concat temp_dir (Printf.sprintf "input_%s.json" task_id) in
    let output_file = Filename.concat temp_dir (Printf.sprintf "output_%s.json" task_id) in
    
    (* serialize into infile *)
    let input = match task_type with
      | MapReduceTask {input; _} -> input
      | DataflowTask {input; _} -> input
    in
    let oc = open_out input_file in
    Yojson.Basic.to_channel oc input;
    close_out oc;
    
    (* spawn worker process *)
    let args = [|
      "worker"; 
      (match task_type with 
       | MapReduceTask _ -> "mapreduce" 
       | DataflowTask _ -> "dataflow");
      (match task_type with 
       | MapReduceTask {phase; _} -> phase 
       | DataflowTask {node_type; _} -> node_type);
      task_id;
      input_file;
      output_file;
      string_of_int (Unix.getpid())
    |] in
    
    let child_pid = 
      try 
        let worker_path = Sys.executable_name in  (* use the same executable *)
        let pid = Unix.create_process worker_path args Unix.stdin Unix.stdout Unix.stderr in
        Some pid
      with e -> 
        Printf.printf "Failed to spawn worker: %s\n" (Printexc.to_string e);
        None
    in
    
    match child_pid with
    | Some pid ->
        let worker_info = {
          task_id;
          status = Running;
          start_time = now;
          last_heartbeat = now;
          task_type;
          output = None;
          pid = Some pid;
        } in
        Printf.printf "Spawned worker process %d for task %s\n" pid task_id;
        Hashtbl.add state.workers task_id worker_info
    | None ->
        (* requeue the task if we failed to spawn a worker *)
        Queue.push (task_id, task_type) state.task_queue
  done

(* start monitor thread *)
let start_background_monitor state check_interval =
  if not state.monitor_running then begin
    state.monitor_running <- true;
    
    (* start monitoring thread, checks if workers are alive *)
    let monitoring_thread = Thread.create (fun () ->
      while state.monitor_running do
        (* monitor_workers_realistic state; *)
        monitor_workers state;
        
        (* sleep until next check *)
        Thread.delay check_interval;
      done;
    ) () in
    
    state.monitoring_thread <- Some monitoring_thread;
    true
  end else
    false

(* stop monitor thread if it is running *)
let stop_background_monitor state =
  if state.monitor_running then begin
    state.monitor_running <- false;
    
    (* exit monitor thread *)
    (match state.monitoring_thread with
    | Some t -> Thread.join t
    | None -> ());
    
    state.monitoring_thread <- None;
    true
  end else
    false

(* await all tasks *)
let wait_for_completion state =
  (* queue empty and all workers inactive *)
  while (not (Queue.is_empty state.task_queue)) || (Hashtbl.length state.workers > 0) do
    Thread.delay 0.1;
  done

(* queue a task *)
let queue_task state task =
  Queue.push task state.task_queue

(* capture task output *)
let get_task_output state task_id =
  Hashtbl.find_opt state.completed_tasks task_id

(* suppress warnings about non-terminating process since we need the master to run without stopping *)
[@@@ocaml.warning "-21"]

let run_master () =
  let state = create_master_state 1.0 in (* heartbeat timeout is 1s *)
  monitor_workers state;
  Task.teardown_pool state.pool