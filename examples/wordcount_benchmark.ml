module StringSequential = Dataflowlib.Mapreduce.MakeMapReduce (String)
module StringParallel = Dataflowlib.Mapreduce_parallel.MakeParallelMapReduce (String)

let file_content =
  let ch = open_in "data/wordcountdata_large.txt" in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let lines = String.split_on_char '\n' file_content
let words_list_list = List.map (String.split_on_char ' ') lines

let map (chunk : string list) : int StringSequential.KMap.t =
  List.fold_left
    (fun acc w ->
       let cur =
         match StringSequential.KMap.find_opt w acc with
         | None -> 0
         | Some c -> c
       in
       StringSequential.KMap.add w (cur + 1) acc)
    StringSequential.KMap.empty
    chunk
;;

let reduce _key (values : int list) = List.fold_left ( + ) 0 values

(* Benchmark *)
let () =
  (* Sequential timing *)
  let sequential_time =
    List.hd
      (Dataflowlib.Benchmark.run ~repeat:5 "sequential wordcount" (fun () ->
         StringSequential.run words_list_list ~map_func:map ~reduce_func:reduce))
  in
  (* Parallel timing *)
  let parallel_time =
    List.hd
      (Dataflowlib.Benchmark.run ~repeat:5 "parallel wordcount" (fun () ->
         StringParallel.run_parallel
           ~num_domains:8
           words_list_list
           ~map_func:map
           ~reduce_func:reduce))
  in
  Printf.printf "speed-up: %.2fx\n%!" (sequential_time /. parallel_time)
;;
