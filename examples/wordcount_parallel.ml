(* Instantiate the functor for string keys *)
module StringPMR = Dataflowlib.Dataflow_parallel.MakeParallelMapReduce (String)

(* Read the input file into memory *)
let file_content =
  let ch = open_in "data/wordcountdata.txt" in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

(* Split into chunks: one list‑of‑words per line *)
let lines = String.split_on_char '\n' file_content
let words_list_list = List.map (String.split_on_char ' ') lines

(* Map and Reduce functions (identical to sequential version) *)
let map (chunk : string list) : int StringPMR.KMap.t =
  List.fold_left
    (fun acc word ->
       let cur =
         match StringPMR.KMap.find_opt word acc with
         | None -> 0
         | Some count -> count
       in
       StringPMR.KMap.add word (cur + 1) acc)
    StringPMR.KMap.empty
    chunk
;;

let reduce (_ : string) (values : int list) : int = List.fold_left ( + ) 0 values

(* Run the parallel pipeline and print the result *)
let () =
  let result =
    StringPMR.run_parallel
      ~num_domains:3
      words_list_list
      ~map_func:map
      ~reduce_func:reduce
  in
  let binding_to_string (k, v) = Printf.sprintf "%s: %d" k v in
  result
  |> StringPMR.KMap.bindings
  |> List.map binding_to_string
  |> String.concat "\n"
  |> print_endline
;;
