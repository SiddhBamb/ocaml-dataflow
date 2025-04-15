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
    List.iter (fun { from_id; to_id } ->
        let count = Hashtbl.find in_degree to_id in
        Hashtbl.replace in_degree to_id (count + 1)
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
        List.iter (fun { from_id; to_id } ->
            if from_id = id then (
                let deg = Hashtbl.find in_degree to_id in
                let deg' = deg - 1 in
                Hashtbl.replace in_degree to_id deg';
                if deg' = 0 then Queue.add to_id queue
            )
        ) graph.edges
    done;
    (* if not all nodes were processed, there's a cycle, can't proceed *)
    if List.length !sorted <> List.length graph.nodes then
        failwith "Cycle detected or graph is not a DAG"
    else List.rev !sorted



