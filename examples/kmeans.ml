let read_input file_path =
  let file_content =
    let ch = open_in file_path in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let lines = String.split_on_char '\n' file_content in
  let metadata = String.split_on_char ' ' (List.hd lines) in
  let n = int_of_string (List.hd metadata) in
  let d = int_of_string (List.hd (List.tl metadata)) in
  let k = int_of_string (List.hd (List.tl (List.tl metadata))) in
  let point_strings = List.map (String.split_on_char ' ') (List.tl lines) in
  let points = List.map (fun line -> List.map float_of_string line) point_strings in
  n, d, k, points
;;

(* Helper to take first k elements *)
let rec take k lst =
  match lst with
  | [] -> []
  | _ when k <= 0 -> []
  | h :: t -> h :: take (k - 1) t
;;

(* input: list of points, context: list of centroids, return: centroid index to the point *)
let map (point : float list) (context : float list list) : float list * float list =
  (* iterate through points and find closest centroid*)
  let _, closest_centroid =
    List.fold_left
      (fun (min_d, min_c) centroid ->
         let current_dist =
           List.fold_left2 (fun acc x y -> acc +. ((x -. y) ** 2.0)) 0.0 point centroid
         in
         if current_dist < min_d then current_dist, centroid else min_d, min_c)
      (infinity, ([] : float list))
      context
  in
  closest_centroid, point
;;

let reduce (input : float list * float list list) : float list =
  let centroid, points = input in
  let num_points = List.length points in
  if num_points = 0
  then centroid (* Handle case with no points for a centroid *)
  else (
    let d = List.length centroid in
    (* Dimension *)
    let sum_points_arr : float array =
      List.fold_left
        (fun acc_arr point_list ->
           Array.mapi (fun i acc_val -> acc_val +. List.nth point_list i) acc_arr)
        (Array.make d 0.0)
        points
    in
    (* Calculate the new centroid by dividing sums by count *)
    let new_centroid_arr =
      Array.map (fun sum -> sum /. float_of_int num_points) sum_points_arr
    in
    Array.to_list new_centroid_arr)
;;

let run_kmeans (num_domains : int) (file_path : string) (max_iterations : int)
  : float list list
  =
  let _, _, k, points = read_input file_path in
  let initial_centroids = take k points in
  let rec convergence_loop current_iteration current_centroids =
    if current_iteration >= max_iterations
    then current_centroids
    else (
      let mapped =
        Nodes.Context_computation.run_computation_with_context
          ~num_domains
          { input = points; transform = map; context = current_centroids }
      in
      let grouped = Nodes.Groupby.run_groupby ~num_domains { input = mapped } in
      let new_centroids =
        Nodes.Computation.run_computation
          ~num_domains
          { input = grouped; transform = reduce }
      in
      convergence_loop (current_iteration + 1) new_centroids)
  in
  convergence_loop 0 initial_centroids
;;

let max_iters = 10

let sequential_average_time =
  let result =
    Dataflowlib.Benchmark.run ~repeat:1 "[sequential] kmeans" (fun () ->
      run_kmeans 0 "data/kmeansdata_large.txt" max_iters)
  in
  List.hd result
;;

let parallel_average_time =
  let result =
    Dataflowlib.Benchmark.run ~repeat:1 "[parallel] kmeans" (fun () ->
      run_kmeans 8 "data/kmeansdata_large.txt" max_iters)
  in
  List.hd result
;;

let () =
  Printf.printf "Sequential average time: %f\n" sequential_average_time;
  Printf.printf "Parallel average time: %f\n" parallel_average_time;
  Printf.printf "Speedup: %f\n" (sequential_average_time /. parallel_average_time)
;;
