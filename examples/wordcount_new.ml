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

let map (input : string) : string * int = input, 1

let reduce (input : string * int list) : string * int =
  fst input, List.fold_left ( + ) 0 (snd input)
;;

let run_wordcount_new (num_domains : int) (file_path : string) =
  let input = read_input file_path in
  let mapped =
    Nodes.Computation.run_computation ~num_domains { input; transform = map }
  in
  let shuffled = Nodes.Groupby.run_groupby ~num_domains { input = mapped } in
  let reduced =
    Nodes.Computation.run_computation
      ~num_domains
      { input = shuffled; transform = reduce }
  in
  reduced
;;

let sequential_average_time =
  let result =
    Dataflowlib.Benchmark.run ~repeat:3 "[sequential] wordcount new" (fun () ->
      run_wordcount_new 0 "data/wordcountdata_medium.txt")
  in
  List.hd result
;;

let parallel_average_time =
  let result =
    Dataflowlib.Benchmark.run ~repeat:3 "[parallel] wordcount new" (fun () ->
      run_wordcount_new 8 "data/wordcountdata_medium.txt")
  in
  List.hd result
;;

let () = Printf.printf "Speedup: %f x\n" (sequential_average_time /. parallel_average_time)
