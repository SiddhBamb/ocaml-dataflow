module StringMapReduce = Dataflowlib.Dataflow.MakeMapReduce (String)

let file_content =
  let ch = open_in "data/wordcountdata.txt" in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let lines = String.split_on_char '\n' file_content
let words = List.map (fun line -> String.split_on_char ' ' line) lines

let map (chunk : string list) : int StringMapReduce.KMap.t =
  List.fold_left
    (fun acc line -> StringMapReduce.KMap.add line 1 acc)
    StringMapReduce.KMap.empty
    chunk
;;

let reduce (_ : string) (values : int list) : int = List.fold_left ( + ) 0 values

let () =
  let result = StringMapReduce.run words ~map_func:map ~reduce_func:reduce in
  let print_binding (k, v) = Printf.sprintf "%s: %d" k v in
  let bindings_str = List.map print_binding (StringMapReduce.KMap.bindings result) in
  print_endline (String.concat "\n" bindings_str)
;;
