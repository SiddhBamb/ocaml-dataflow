(* like template in c++, we can make mapreduce for any key type *)
module MakeMapReduce (Key : Map.OrderedType) = struct
  module KMap = Map.Make (Key)

  type 'a input_chunk = 'a list

  (* map: takes list of input chunks, makes it into a map for each chunk *)
  (* in word count example: takes list of lines, makes map from word to count *)
  let map_task_runner
        (input_chunks : 'a input_chunk list)
        (map_func : 'a input_chunk -> 'v KMap.t)
    : 'v KMap.t list
    =
    List.map map_func input_chunks
  ;;

  (* takes all the different maps from each map task, merges into single map from key 
    list of values w that key *)
  (* in word count example: takes map from word to count for each line, merges into single map from word to list of counts *)
  let shuffle_task_runner (maps : 'v KMap.t list) : 'v list KMap.t =
    let init_map : 'v list KMap.t = KMap.empty in
    let merge_fun (_key : Key.t) (list_opt : 'v list option) (v_opt : 'v option)
      : 'v list option
      =
      match list_opt, v_opt with
      | Some list_val, None -> Some list_val (* Key only in acc, keep list *)
      | None, Some v_val -> Some [ v_val ] (* Key only in current, create list *)
      | Some list_val, Some v_val -> Some (v_val :: list_val) (* Key in both, prepend *)
      | None, None -> None
      (* Should not happen in Map.merge logic, but handle defensively *)
    in
    List.fold_left
      (fun acc_map current_map -> KMap.merge merge_fun acc_map current_map)
      init_map
      maps
  ;;

  (* reduce: takes map from key to list of values w/ that key, reduces to 
     map from key to single value *)
  (* in word count example: takes map from word to list of counts, reduces to 
     map from word to total count *)
  let reduce_task_runner (map : 'v list KMap.t) (reduce_func : Key.t -> 'v list -> 'r)
    : 'r KMap.t
    =
    KMap.mapi reduce_func map
  ;;

  (* runs the whole pipeline *)
  let run
        (input_chunks : 'a input_chunk list)
        ~(map_func : 'a input_chunk -> 'v KMap.t)
        ~(reduce_func : Key.t -> 'v list -> 'r)
    : 'r KMap.t
    =
    let map_outputs = map_task_runner input_chunks map_func in
    let shuffled_map = shuffle_task_runner map_outputs in
    reduce_task_runner shuffled_map reduce_func
  ;;
end
