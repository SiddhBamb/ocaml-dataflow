let read_input file_path =
  let file_content =
    let ch = open_in file_path in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let lines = String.split_on_char '\n' file_content in
  let data_lines = List.tl lines in
  let records =
    List.mapi
      (fun line_idx data_line_str ->
         match String.split_on_char ' ' data_line_str with
         | [ user_str; item_str; rating_str ] ->
           (try Some (user_str, (int_of_string item_str, int_of_string rating_str)) with
            | Failure _ ->
              failwith
                (Printf.sprintf
                   "Invalid number format on line %d: %s"
                   (line_idx + 2)
                   data_line_str))
         | [] when line_idx = List.length data_lines - 1 && data_line_str = "" -> None
         | _ ->
           failwith
             (Printf.sprintf
                "Invalid data line format on line %d: expected user item rating, got: \
                 \"%s\""
                (line_idx + 2)
                data_line_str))
      data_lines
    |> List.filter_map (fun x -> x)
  in
  records
;;

(*
   stages:
shuffle by user: [user, list of (item, rating)]
normalize by user: [user, list of (item, rating)]
compute corating of pairs: [(i, j), (ri * rj, ri^2, rj^2)]
shuffle by pairs: [(i, j), list of (ri * rj, ri^2, rj^2)]
compute similarity of pairs: [(i, j), similarity]
filter by positive similarity: [(i, j), similarity]
shuffle by item: [i, list of (j, similarity)]
get top k: [i, list of (j, similarity)]
*)

let normalize_ratings (input : string * (int * int) list) : (int * int) list =
  let _, ratings = input in
  let average_rating =
    List.fold_left (fun acc (_, rating) -> acc + rating) 0 ratings / List.length ratings
  in
  List.map (fun (item, rating) -> item, rating - average_rating) ratings
;;

let compute_corating (input : (int * int) list) : ((int * int) * (int * int * int)) list =
  (* for each pair of items, compute r1 * r2, r1^2, r2^2 *)
  let similarity =
    List.fold_left
      (fun acc (item1, rating1) ->
         List.fold_left
           (fun acc (item2, rating2) ->
              if item1 < item2
              then
                ((item1, item2), (rating1 * rating2, rating1 * rating1, rating2 * rating2))
                :: acc
              else acc)
           acc
           input)
      []
      input
  in
  similarity
;;

let compute_similarity (input : (int * int) * (int * int * int) list)
  : int * (int * float)
  =
  let (i, j), coratings = input in
  let total_dot_product = List.fold_left (fun acc (dot, _, _) -> acc + dot) 0 coratings in
  let total_magI =
    sqrt (float_of_int (List.fold_left (fun acc (_, magI, _) -> acc + magI) 0 coratings))
  in
  let total_magJ =
    sqrt (float_of_int (List.fold_left (fun acc (_, _, magJ) -> acc + magJ) 0 coratings))
  in
  let similarity = float_of_int total_dot_product /. (total_magI *. total_magJ) in
  i, (j, similarity)
;;

let filter_similarity (input : int * (int * float)) : bool =
  let _, (_, similarity) = input in
  similarity > 0.0
;;

let rec take k lst =
  match lst with
  | [] -> []
  | _ when k <= 0 -> []
  | h :: t -> h :: take (k - 1) t
;;

let get_top_k (input : int * (int * float) list) : int * (int * float) list =
  let i, similarities = input in
  let sorted_similarities = List.sort (fun (_, a) (_, b) -> compare b a) similarities in
  let top_k = take 10 sorted_similarities in
  i, top_k
;;

let run_matrix_factorization (num_domains : int) (file_path : string)
  : (int * (int * float) list) list
  =
  let records = read_input file_path in
  let grouped_by_user = Nodes.Groupby.run_groupby ~num_domains { input = records } in
  let normalized_ratings =
    Nodes.Computation.run_computation
      ~num_domains
      { input = grouped_by_user; transform = normalize_ratings }
  in
  let corating =
    List.concat
      (Nodes.Computation.run_computation
         ~num_domains
         { input = normalized_ratings; transform = compute_corating })
  in
  let grouped_by_pairs = Nodes.Groupby.run_groupby ~num_domains { input = corating } in
  let similarity =
    Nodes.Computation.run_computation
      ~num_domains
      { input = grouped_by_pairs; transform = compute_similarity }
  in
  let filtered_similarity =
    Nodes.Filter.run_filter ~num_domains { input = similarity; pred = filter_similarity }
  in
  let grouped_by_item =
    Nodes.Groupby.run_groupby ~num_domains { input = filtered_similarity }
  in
  let top_k =
    Nodes.Computation.run_computation
      ~num_domains
      { input = grouped_by_item; transform = get_top_k }
  in
  top_k
;;

let sequential_average_time =
  let result =
    Dataflowlib.Benchmark.run ~repeat:1 "[sequential] matrix factorization" (fun () ->
      run_matrix_factorization 1 "data/ratings_upper_medium.txt")
  in
  List.hd result
;;

let parallel_average_time =
  let result =
    Dataflowlib.Benchmark.run ~repeat:1 "[parallel] matrix factorization" (fun () ->
      run_matrix_factorization 8 "data/ratings_upper_medium.txt")
  in
  List.hd result
;;

let () =
  Printf.printf "Sequential average time: %f\n" sequential_average_time;
  Printf.printf "Parallel average time: %f\n" parallel_average_time;
  Printf.printf "Speedup: %f\n" (sequential_average_time /. parallel_average_time)
;;
