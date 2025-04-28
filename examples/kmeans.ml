open Nodes (* Computation, Context_computation, Groupby *)
open Shards

(* ───────── unchanged helpers ───────── *)
let read_input path =
  let ch = open_in path in
  let txt = really_input_string ch (in_channel_length ch) in
  close_in ch;
  match String.split_on_char '\n' txt with
  | header :: rows ->
    let k = header |> String.split_on_char ' ' |> List.nth 2 |> int_of_string in
    let points =
      List.map (fun l -> String.split_on_char ' ' l |> List.map float_of_string) rows
    in
    k, points
  | [] -> failwith "empty file"
;;

let rec take k = function
  | ([] | _) when k = 0 -> []
  | h :: t -> h :: take (k - 1) t
;;

let map_point p centroids =
  let _, nearest =
    List.fold_left
      (fun (best_d, best_c) c ->
         let d = List.fold_left2 (fun s x y -> s +. ((x -. y) ** 2.)) 0. p c in
         if d < best_d then d, c else best_d, best_c)
      (Float.infinity, [])
      centroids
  in
  nearest, p
;;

let reduce_cluster (centroid, pts) =
  match pts with
  | [] -> centroid
  | _ ->
    let dim = List.length centroid in
    let sums =
      List.fold_left
        (fun acc pt -> Array.mapi (fun i s -> s +. List.nth pt i) acc)
        (Array.make dim 0.)
        pts
    in
    Array.to_list (Array.map (fun s -> s /. float (List.length pts)) sums)
;;

(* ───────── shard-aware k-means ───────── *)
let run_kmeans ~num_domains (* 0 = sequential *) path max_iter : float list list =
  (* 0 ▸ read data + single partition pass *)
  let k, points = read_input path in
  let n_dom = if num_domains = 0 then 1 else num_domains in
  let points_shards = Shards.of_list ~n:n_dom points in
  let init_centroids = take k points in
  let rec loop iter centroids =
    if iter = max_iter
    then centroids
    else (
      (* 1 ▸ assign points → nearest centroid *)
      let assigned =
        Nodes.Context_computation.run_computation_with_context
          ~num_domains:n_dom
          { input = Shards.concat points_shards (* API expects list *)
          ; transform = map_point
          ; context = centroids
          }
      in
      (* shards returned *)
      (* 2 ▸ group by centroid key *)
      let grouped =
        Nodes.Groupby.run ~num_domains:n_dom { input = Shards.concat assigned }
        (* Groupby expects list *)
      in
      (* shards returned *)
      (* 3 ▸ recompute centroids *)
      let new_centroids =
        Nodes.Computation.run
          ~num_domains:n_dom
          { input = grouped (* shards *); transform = reduce_cluster }
        |> Shards.concat (* flatten for next iter *)
      in
      loop (iter + 1) new_centroids)
  in
  loop 0 init_centroids
;;

(* ───────── simple benchmark ───────── *)
let () =
  let path = "data/kmeansdata_medium.txt" in
  let max_iter = 100 in
  let seq_time =
    Dataflowlib.Benchmark.run ~repeat:1 "[seq] kmeans" (fun () ->
      run_kmeans ~num_domains:0 path max_iter)
    |> List.hd
  and par_time =
    Dataflowlib.Benchmark.run ~repeat:1 "[par] kmeans" (fun () ->
      run_kmeans ~num_domains:8 path max_iter)
    |> List.hd
  in
  Printf.printf "seq  : %.3fs\n" seq_time;
  Printf.printf "par  : %.3fs\n" par_time;
  Printf.printf "speed-up: %.2fx\n" (seq_time /. par_time)
;;
