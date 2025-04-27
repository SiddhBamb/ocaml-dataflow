let read_input file_path =
  let file_content =
    let ch = open_in file_path in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let lines = String.split_on_char '\n' file_content in
  let words_list_list = List.map (String.split_on_char ' ') lines in
  List.concat words_list_list
;;

let map (input : string) : (string * int) = 
  (input, 1)
;;

let reduce (input : string * int list) : string * int = 
  (fst input, List.fold_left (+) 0 (snd input))
;;

let run_wordcount_new (file_path) = 
  let input = read_input file_path in
  let mapped = Nodes.Computation.run_computation { input = input; transform = map } in
  let shuffled = Nodes.Groupby.run_groupby { input = mapped } in
  let reduced = Nodes.Computation.run_computation { input = shuffled; transform = reduce } in
  reduced
;;

let get_average_time =
  let result = Dataflowlib.Benchmark.run ~repeat:1 "wordcount new" (fun () -> run_wordcount_new "data/wordcountdata_large.txt") in
  List.hd result
;;

let () =
  let time = get_average_time in
  Printf.printf "Time taken: %f seconds\n" time
;;
