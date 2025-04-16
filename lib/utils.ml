type node_type =
  | FileIO
  | Computation

type node_config = {
  path : string option;
  mode : string option;
  function_name : string option;
  threads : int;
}

type node = {
  id : string;
  ntype : node_type;
  config : node_config;
}

type edge = {
  from_id : string;
  to_id : string;
}

type graph = {
  job_name : string;
  nodes : node list;
  edges : edge list;
}

(* kahn's algo impl for topo sort *)
(* extract stage-level parallelism from dag *)
let topological_sort graph =
  (* set up indegree and node id maps *)
  let node_map = Hashtbl.create 10 in
  List.iter (fun node -> Hashtbl.add node_map node.id node) graph.nodes;
  let in_degree = Hashtbl.create 10 in
  List.iter (fun node -> Hashtbl.add in_degree node.id 0) graph.nodes;
  let adj_list = Hashtbl.create 10 in
  List.iter (fun node -> Hashtbl.add adj_list node.id []) graph.nodes;
  List.iter (fun edge ->
    let count = Hashtbl.find in_degree edge.to_id in
    Hashtbl.replace in_degree edge.to_id (count + 1);
    Hashtbl.add adj_list edge.from_id (edge.to_id :: Hashtbl.find adj_list edge.from_id)
  ) graph.edges;

  (* process nodes with 0 indegree first *)
  let queue = Queue.create () in
  Hashtbl.iter (fun id deg ->
    if deg = 0 then Queue.add id queue
  ) in_degree;

  (* topological sort *)
  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let id = Queue.take queue in
    sorted := id :: !sorted;
    (* decrement indegree when processing dependency, add to queue if it hits 0 *)
    List.iter (fun to_id ->
      let deg = Hashtbl.find in_degree to_id in
      let deg' = deg - 1 in
      Hashtbl.replace in_degree to_id deg';
      if deg' = 0 then Queue.add to_id queue
    ) (Hashtbl.find adj_list id)
  done;
  (* if not all nodes were processed, there's a cycle, can't proceed *)
  if List.length !sorted <> List.length graph.nodes then
    failwith "Cycle detected or graph is not a DAG"
  else List.rev !sorted

(* Helper function to convert node_type to string *)
let node_type_to_string = function
  | FileIO -> "FileIO"
  | Computation -> "Computation"

(* Helper function to print option types cleanly *)
let print_option printer = function
  | Some v -> printer v
  | None -> print_string "None"

(* Function to print the graph details *)
let print_graph graph = 
  Printf.printf "Job Name: %s\n" graph.job_name;
  print_endline "Nodes:";
  List.iter (fun node ->
    Printf.printf "  Node ID: %s\n" node.id;
    Printf.printf "    Type: %s\n" (node_type_to_string node.ntype);
    Printf.printf "    Config:\n";
    Printf.printf "      Path: "; print_option print_string node.config.path; print_newline ();
    Printf.printf "      Mode: "; print_option print_string node.config.mode; print_newline ();
    Printf.printf "      Function Name: "; print_option print_string node.config.function_name; print_newline ();
    Printf.printf "      Threads: %d\n" node.config.threads
  ) graph.nodes;
  print_endline "Edges:";
  List.iter (fun edge ->
    Printf.printf "  %s -> %s\n" edge.from_id edge.to_id
  ) graph.edges



