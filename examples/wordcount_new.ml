open Nodes
module GB = Groupby.Make (String)

(* ---------- helpers -------------------------------------------------- *)
let read_input file_path =
  let ic = open_in file_path in
  let txt = really_input_string ic (in_channel_length ic) in
  close_in ic;
  txt |> String.split_on_char '\n' |> List.concat_map (String.split_on_char ' ')
;;

let map w = w, 1
let reduce (w, ns) = w, List.fold_left ( + ) 0 ns

(* ---------- shard-aware pipeline ------------------------------------- *)
let run_wordcount_new ~num_domains file =
  let n_dom = if num_domains = 0 then 1 else num_domains in
  let words = read_input file in
  let mapped =
    Computation.run ~num_domains:n_dom { input = words; transform = map }
    (* returns shards *)
  in
  let grouped =
    GB.run ~num_domains:n_dom { input = Shards.concat mapped }
    (* expects list  *)
  in
  let reduced =
    Computation.run
      ~num_domains:n_dom
      { input = Shards.concat grouped (* flatten here *); transform = reduce }
    |> Shards.concat (* final list *)
  in
  reduced
;;

(* ---------- quick benchmark ------------------------------------------ *)
let seq_time =
  Dataflowlib.Benchmark.run ~repeat:3 "[seq] wc" (fun () ->
    run_wordcount_new ~num_domains:0 "data/wordcountdata_large.txt")
  |> List.hd
;;

let par_time =
  Dataflowlib.Benchmark.run ~repeat:3 "[par] wc" (fun () ->
    run_wordcount_new ~num_domains:8 "data/wordcountdata_large.txt")
  |> List.hd
;;

let () = Printf.printf "speed-up: %.2fx\n" (seq_time /. par_time)

(* let () =
  let result = run_wordcount_new ~num_domains:8 "data/wordcountdata_medium.txt" in
  let sorted =
    List.sort (fun (word1, _) (word2, _) -> String.compare word1 word2) result
  in
  List.iter (fun (word, count) -> Printf.printf "%s: %d\n" word count) sorted
;; *)
