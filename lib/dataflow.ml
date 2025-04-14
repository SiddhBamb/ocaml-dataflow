open Core

(* generic comparable type*)
module type ORDERED_TYPE = sig
    type t
    val compare : t -> t -> int
end

(* like template in c++, we can make mapreduce for any key type *)
module MakeMapReduce (Key : ORDERED_TYPE) = struct
    module KMap = Map.Make(Key) (* Use the provided Key *)

    type 'a input_chunk = 'a list

    (* map: takes list of input chunks, makes it into a map for each chunk *)
    (* in word count example: takes list of lines, makes map from word to count *)
    let map_task_runner (input_chunks: 'a input_chunk list) (map_func: 'a input_chunk -> 'v KMap.t) : 'v KMap.t list =
        List.map input_chunks ~f:map_func
    ;;

    (* takes all the different maps from each map task, merges into single map from key 
    list of values w that key *)
    (* in word count example: takes map from word to count for each line, merges into single map from word to list of counts *)
    let shuffle_task_runner (maps: 'v KMap.t list) : 'v list KMap.t =
        let init_map : 'v list KMap.t = KMap.empty in
        List.fold_left maps ~init:init_map ~f:(fun acc_map current_map ->
            KMap.merge acc_map current_map ~f:(fun ~key:_ merge_result ->
                match merge_result with
                | `Left list_val -> Some list_val (* Key only in acc, keep list *)
                | `Right v_val -> Some [v_val]   (* Key only in current, create list *)
                | `Both (list_val, v_val) -> Some (v_val :: list_val) (* Key in both, prepend *)
            )
        )
    ;;

    (* reduce: takes map from key to list of values w/ that key, reduces to 
     map from key to single value *)
    (* in word count example: takes map from word to list of counts, reduces to 
     map from word to total count *)
    let reduce_task_runner (map: 'v list KMap.t) (reduce_func: key:Key.t -> data:'v list -> 'r) : 'r KMap.t =
        KMap.mapi map ~f:reduce_func
    ;;

    (* runs the whole pipeline *)
    let run (input_chunks: 'a input_chunk list)
            ~(map_func: 'a input_chunk -> 'v KMap.t)
            ~(reduce_func: key:Key.t -> data:'v list -> 'r) : 'r KMap.t =
        let map_outputs = map_task_runner input_chunks map_func in
        let shuffled_map = shuffle_task_runner map_outputs in
        reduce_task_runner shuffled_map reduce_func
    ;;
end
