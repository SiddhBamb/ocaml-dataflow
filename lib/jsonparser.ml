(* parse json file into a Yojson.Basic.t *)
let parse_json_file (filename: string) : Yojson.Basic.t =
    let ch = open_in filename in
    let json_str = really_input_string ch (in_channel_length ch) in
    close_in ch;
    Yojson.Basic.from_string json_str

let json_config_to_node_config (json_config: Yojson.Basic.t) : Utils.node_config =
    let path_json = Yojson.Basic.Util.member "path" json_config in
    let path = match path_json with
      | `Null -> None
      | _ -> Some (Yojson.Basic.Util.to_string path_json)
    in
    let mode_json = Yojson.Basic.Util.member "mode" json_config in
    let mode = match mode_json with
      | `Null -> None
      | _ -> Some (Yojson.Basic.Util.to_string mode_json)
    in
    let function_name_json = Yojson.Basic.Util.member "function_name" json_config in
    let function_name = match function_name_json with
      | `Null -> None
      | _ -> Some (Yojson.Basic.Util.to_string function_name_json)
    in
    let threads = Yojson.Basic.Util.member "threads" json_config |> Yojson.Basic.Util.to_int in
    {path = path; mode = mode; function_name = function_name; threads = threads}

let json_node_type_to_node_type (json_node_type: Yojson.Basic.t) : Utils.node_type =
    let node_type_str = Yojson.Basic.Util.to_string json_node_type in
    match node_type_str with
      | "FileIO" -> Utils.FileIO
      | "Computation" -> Utils.Computation
      | _ -> failwith ("Invalid node type: " ^ node_type_str)

let json_node_to_node (json_node: Yojson.Basic.t) : Utils.node =
    let id = Yojson.Basic.Util.member "id" json_node |> Yojson.Basic.Util.to_string in
    let ntype = json_node_type_to_node_type (Yojson.Basic.Util.member "type" json_node) in
    let config = Yojson.Basic.Util.member "config" json_node in
    let node_config = json_config_to_node_config config in
    {id = id; ntype = ntype; config = node_config}

let json_edge_to_edge (json_edge: Yojson.Basic.t) : Utils.edge =
    let from_id = Yojson.Basic.Util.member "from" json_edge |> Yojson.Basic.Util.to_string in
    let to_id = Yojson.Basic.Util.member "to" json_edge |> Yojson.Basic.Util.to_string in
    {from_id = from_id; to_id = to_id}

(* convert json_str to graph*)
let json_to_graph (filename: string) : Utils.graph =
    let json_str = parse_json_file filename in
    let job_name = Yojson.Basic.Util.member "job_name" json_str |> Yojson.Basic.Util.to_string in
    let nodes = Yojson.Basic.Util.member "nodes" json_str in
    let edges = Yojson.Basic.Util.member "edges" json_str in
    let nodes_list = Yojson.Basic.Util.to_list nodes in
    let edges_list = Yojson.Basic.Util.to_list edges in
    let nodes = List.map json_node_to_node nodes_list in
    let edges = List.map json_edge_to_edge edges_list in
    {job_name = job_name; nodes = nodes; edges = edges}