type node_type =
  | FileIO
  | Computation

type node_config =
  { path : string option
  ; mode : string option
  ; function_name : string option
  ; threads : int
  }

type node =
  { id : string
  ; ntype : node_type
  ; config : node_config
  }

type edge =
  { from_id : string
  ; to_id : string
  }

type graph =
  { job_name : string
  ; nodes : node list
  ; edges : edge list
  }

(** Performs topological sort on the graph.
    Returns Ok of node ID list in topological order,
    or Error "Cycle detected" if the graph is not a DAG. *)
let topological_sort graph : (string list, string) result =
  (* Map storing indegrees *)
  let in_degree = Hashtbl.create 10 in
  List.iter (fun node -> Hashtbl.add in_degree node.id 0) graph.nodes;
  (* Map storing adjacency lists *)
  let adj_list = Hashtbl.create 10 in
  List.iter (fun node -> Hashtbl.add adj_list node.id []) graph.nodes;
  (* Count indegrees *)
  List.iter
    (fun edge ->
       let count = Hashtbl.find in_degree edge.to_id in
       Hashtbl.replace in_degree edge.to_id (count + 1);
       Hashtbl.add adj_list edge.from_id (edge.to_id :: Hashtbl.find adj_list edge.from_id))
    graph.edges;
  (* Process nodes with 0 indegree first *)
  let queue = Queue.create () in
  Hashtbl.iter (fun id deg -> if deg = 0 then Queue.add id queue) in_degree;
  (* Topological sort *)
  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let id = Queue.take queue in
    sorted := id :: !sorted;
    (* Decrement indegree when processing dependency, add to queue if it hits 0 *)
    List.iter
      (fun to_id ->
         let deg = Hashtbl.find in_degree to_id in
         let deg' = deg - 1 in
         Hashtbl.replace in_degree to_id deg';
         if deg' = 0 then Queue.add to_id queue)
      (Hashtbl.find adj_list id)
  done;
  (* if not all nodes were processed, there's a cycle, can't proceed *)
  if List.length !sorted <> List.length graph.nodes then
    failwith "Cycle detected or graph is not a DAG"
  else 
    let sorted_nodes = List.map (fun id -> Hashtbl.find node_map id) !sorted in
    List.rev sorted_nodes

(* Helper function to convert node_type to string. *)
let node_type_to_string = function
  | FileIO -> "FileIO"
  | Computation -> "Computation"
;;

(* Helper function to print option types cleanly. *)
let print_option printer = function
  | Some v -> printer v
  | None -> print_string "None"
;;

(* Function to print the graph details. *)
let print_graph graph =
  Printf.printf "Job Name: %s\n" graph.job_name;
  print_endline "Nodes:";
  List.iter
    (fun node ->
       Printf.printf "  Node ID: %s\n" node.id;
       Printf.printf "    Type: %s\n" (node_type_to_string node.ntype);
       Printf.printf "    Config:\n";
       Printf.printf "      Path: ";
       print_option print_string node.config.path;
       print_newline ();
       Printf.printf "      Mode: ";
       print_option print_string node.config.mode;
       print_newline ();
       Printf.printf "      Function Name: ";
       print_option print_string node.config.function_name;
       print_newline ();
       Printf.printf "      Threads: %d\n" node.config.threads)
    graph.nodes;
  print_endline "Edges:";
  List.iter (fun edge ->
    Printf.printf "  %s -> %s\n" edge.from_id edge.to_id
  ) graph.edges

(* the potential output types from different nodes 
we need this cause nodes can have different types of outputs *)
type generic_type = 
  | NoOutput (* For nodes with no specific output or side effects only *)
  | IntList of int list
  | StringList of string list
  | FloatList of float list
  | StringIntList of (string * int) list
  | StringIntListList of (string * (int list)) list
  (* Add other possible output types here *)

let create_output_map () : (string, generic_type) Hashtbl.t =
  Hashtbl.create 10 (* Adjust initial size as needed *)

(* read input file: each input file is one line of space separated strings*)
let read_input_file (path : string) : string list =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  String.split_on_char ' ' s

let write_output_file (path : string) (inputs : generic_type list) =
  let ch = open_out path in
  List.iter (fun input ->
    match input with
    | StringList str_list ->
      List.iter (fun str -> output_string ch str) str_list
    | IntList int_list ->
      List.iter (fun int -> output_string ch (string_of_int int)) int_list
    | FloatList float_list ->
      List.iter (fun float -> output_string ch (string_of_float float)) float_list
    | StringIntList str_int_list ->
      List.iter (fun (str, int) -> output_string ch (str ^ " " ^ string_of_int int)) str_int_list
    | StringIntListList str_int_list_list ->
      List.iter (fun (str, int_list) ->
        List.iter (fun int -> output_string ch (str ^ " " ^ string_of_int int)) int_list
      ) str_int_list_list
    | _ -> failwith "Unsupported output type"
  ) inputs;
  close_out ch
