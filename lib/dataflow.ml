type 'a input_chunk = 'a list

(* like template in c++, we can make mapreduce for any key type *)
module MakeMapReduce (Key : Map.OrderedType) = struct
  module KMap = Map.Make (Key)

    (* map: takes list of input chunks, makes it into a map for each chunk *)
    (* in word count example: takes list of lines, makes map from word to count *)
    let map_task_runner (input_chunks: 'a input_chunk list) (map_func: 'a input_chunk -> 'v KMap.t) : 'v KMap.t list =
        List.map map_func input_chunks
    ;;

  (** Takes all the different maps from each map task, merges into single map
      from the list of values w that map to each key.
      In word count example: takes map from word to count for each line, merges
      into single map from word to list of counts. *)
  let shuffle_task_runner (maps : 'v KMap.t list) : 'v list KMap.t =
    let init_map : 'v list KMap.t = KMap.empty in
    let merge_fun (_key : Key.t) (list_opt : 'v list option) (v_opt : 'v option)
      : 'v list option
      =
      match list_opt, v_opt with
      | Some list_val, None -> Some list_val (* Key only in acc, keep list *)
      | None, Some v_val -> Some [ v_val ] (* Key only in current, create list *)
      | Some list_val, Some v_val -> Some (v_val :: list_val) (* Key in both, prepend *)
      | None, None -> None (* Should not happen *)
    in
    List.fold_left
      (fun acc_map current_map -> KMap.merge merge_fun acc_map current_map)
      init_map
      maps
  ;;

  (** Takes map from key to list of values, reduces to map from key to single value.
      In word count example: takes map from word to list of counts, reduces to
      map from word to total count. *)
  let reduce_task_runner (map : 'v list KMap.t) (reduce_func : Key.t -> 'v list -> 'r)
    : 'r KMap.t
    =
    KMap.mapi reduce_func map
  ;;

  (* Runs the whole pipeline. *)
  let run
        (input_chunks : 'a input_chunk list)
        ~(map_func : 'a input_chunk -> 'v KMap.t)
        ~(reduce_func : Key.t -> 'v list -> 'r)
    : 'r KMap.t
    =
    let map_outputs = map_task_runner input_chunks map_func in
    let shuffled_map = shuffle_task_runner map_outputs in
    reduce_task_runner shuffled_map reduce_func
  ;;
end

(*
planning sequential dataflow execution on the graph

1. get graph from json, and topo sort: both functions in utils.ml
2. execute nodes in topological order
    - need to figure out how to pass data between nodes
    - maybe have a global state that all nodes can read from and write to?
    - or maybe have a list of inputs and outputs for each node, and pass data that way?
2a. gonna communicate through global map node -> output
*)

(*
create global map from node id to output
*)

type function_registry = (string, (Utils.generic_type -> Utils.generic_type)) Hashtbl.t

let accumulate_same_type_generic_inputs (inputs : Utils.generic_type list) : Utils.generic_type =
  match inputs with
  | [] -> Utils.NoOutput (* Sensible default for empty input *)
  | (Utils.StringList _ :: _) ->
      let all_string_lists = List.filter_map (function Utils.StringList sl -> Some sl | _ -> None) inputs in
      Utils.StringList (List.concat all_string_lists)
  | (Utils.IntList _ :: _) ->
      let all_int_lists = List.filter_map (function Utils.IntList il -> Some il | _ -> None) inputs in
      Utils.IntList (List.concat all_int_lists)
  | (Utils.FloatList _ :: _) ->
      let all_float_lists = List.filter_map (function Utils.FloatList fl -> Some fl | _ -> None) inputs in
      Utils.FloatList (List.concat all_float_lists)
  | (Utils.StringIntList _ :: _) ->
      let all_si_lists = List.filter_map (function Utils.StringIntList sil -> Some sil | _ -> None) inputs in
      Utils.StringIntList (List.concat all_si_lists)
  | (Utils.StringIntListList _ :: _) ->
      let all_si_list_lists = List.filter_map (function Utils.StringIntListList sil -> Some sil | _ -> None) inputs in
      Utils.StringIntListList (List.concat all_si_list_lists)
  (* Add cases for any other list types in Utils.generic_type *)
  | (Utils.NoOutput :: _) -> Utils.NoOutput (* If inputs are NoOutput, result is NoOutput *)

let get_option_exn field_name opt =
  match opt with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Configuration error: Missing required field '%s' in node config" field_name)

module Dataflow = struct
  let run (graph_json_path : string) (functions : function_registry) =
    let graph = Jsonparser.json_to_graph graph_json_path in
    let sorted_nodes = Utils.topological_sort graph in (* list of nodes *)

    let global_output_map = Utils.create_output_map () in (* map from node id to output *)

    let execute_node (node : Utils.node) =
        match node.ntype with
        | Utils.FileIO ->
            let mode = get_option_exn "mode" node.config.mode in
            let path = get_option_exn "path" node.config.path in (* Get path for both read/write *)
            if mode = "read" then
                let input = Utils.read_input_file path in
                Hashtbl.add global_output_map node.id (Utils.StringList input)
            else if mode = "write" then
                let relevant_edges = List.filter (fun (edge : Utils.edge) -> edge.to_id = node.id) graph.edges in
                let inputs = List.map (fun (edge : Utils.edge) -> Hashtbl.find global_output_map edge.from_id) relevant_edges in
                Utils.write_output_file path inputs
            (* Optional: Add an else case to handle unexpected modes *)
            else
                failwith ("Unknown FileIO mode: " ^ mode)
        | Utils.Computation ->
            (* iterate through all edges, collect inputs from global_output_map where edge.dst = node.id *)
            let relevant_edges = List.filter (fun (edge : Utils.edge) -> edge.to_id = node.id) graph.edges in
            (*list of generic_type inputs*)
            let inputs = List.map (fun (edge : Utils.edge) -> Hashtbl.find global_output_map edge.from_id) relevant_edges in
            (* accumulate inputs into a single list*)
            let accumulated_inputs = accumulate_same_type_generic_inputs inputs in
            (* TODO: parallelize. get num threads, split inputs into num threads lists, apply function to each list in parallel *)
            let func_name = get_option_exn "function_name" node.config.function_name in
            (* apply function *)
            let func = Hashtbl.find functions func_name in
            let output = func accumulated_inputs in
            (* add output to global_output_map *)
            Hashtbl.add global_output_map node.id output
    in
    (* execute all nodes in topological order *)
    List.iter execute_node sorted_nodes
end