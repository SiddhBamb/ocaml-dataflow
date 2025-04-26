(*

let map_word_count (input : Dataflowlib.Utils.generic_type) : Dataflowlib.Utils.generic_type =
    let input_strings = match input with 
    | Dataflowlib.Utils.StringList sl -> sl
    | _ -> failwith "Input is not a string list"
    in

    let word_counts = Hashtbl.create 100 in
    List.iter (fun word ->
        let count = Hashtbl.find_opt word_counts word in
        match count with
        | Some c -> Hashtbl.replace word_counts word (c + 1)
        | None -> Hashtbl.add word_counts word 1
    ) input_strings;

    let word_counts_list = Hashtbl.fold (fun word count acc ->
        (word, count) :: acc
    ) word_counts [] in

    Dataflowlib.Utils.StringIntList word_counts_list

*)

let () = 
    let graph = Dataflowlib.Jsonparser.json_to_graph "examples/example_cfg.json" in
    Dataflowlib.Utils.print_graph graph