open Domainslib

module MakeParallelMapReduce (Key : Map.OrderedType) = struct
  module KMap = Map.Make (Key)

  type 'a input_chunk = 'a list

  let parallel_map ?(chunk_size = 32) (pool : Task.pool) (f : 'a -> 'b) (xs : 'a list)
    : 'b list
    =
    let in_arr = Array.of_list xs in
    let n = Array.length in_arr in
    let out_arr = Array.make n (Obj.magic ()) in
    Task.run pool
    @@ fun () ->
    Task.parallel_for pool ~chunk_size ~start:0 ~finish:(n - 1) ~body:(fun i ->
      out_arr.(i) <- f in_arr.(i));
    Array.to_list out_arr
  ;;

  let shuffle_task_runner (maps : 'v KMap.t list) : 'v list KMap.t =
    let init = KMap.empty in
    let merge _key list_opt v_opt =
      match list_opt, v_opt with
      | Some l, None -> Some l
      | None, Some v -> Some [ v ]
      | Some l, Some v -> Some (v :: l)
      | None, None -> None
    in
    List.fold_left (fun acc m -> KMap.merge merge acc m) init maps
  ;;

  let run_parallel
        ~(num_domains : int)
        (input_chunks : 'a input_chunk list)
        ~(map_func : 'a input_chunk -> 'v KMap.t)
        ~(reduce_func : Key.t -> 'v list -> 'r)
    : 'r KMap.t
    =
    let pool = Task.setup_pool ~num_domains () in
    (* Map *)
    let map_outputs = parallel_map pool map_func input_chunks in
    (* Shuffle *)
    let shuffled = shuffle_task_runner map_outputs in
    (* Reduce *)
    let bindings = KMap.bindings shuffled in
    let reduced_bindings =
      parallel_map pool (fun (k, lst) -> k, reduce_func k lst) bindings
    in
    (* Collect *)
    let result =
      List.fold_left (fun acc (k, v) -> KMap.add k v acc) KMap.empty reduced_bindings
    in
    Task.teardown_pool pool;
    result
  ;;
end
