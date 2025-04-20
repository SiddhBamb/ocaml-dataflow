let () =
  let graph = Dataflowlib.Jsonparser.json_to_graph "examples/example_cfg.json" in
  Dataflowlib.Utils.print_graph graph
;;
