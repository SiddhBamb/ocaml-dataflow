(** A polymorphic computation node.
    Users specifies a function to apply on each element of a list. *)
    type ('a, 'b, 'c) t =
    { input : 'a list (** Elements to process *)
    ; transform : 'a -> 'c -> 'b (** User-supplied transformation *)
    ; context : 'c (** Context for the transformation *)
    }
  
  let computation_sequential (node : ('a, 'b, 'c) t) : 'b list =
    List.map (fun element -> node.transform element node.context) node.input
  ;;
  
  let computation_parallel (num_domains : int) (node : ('a, 'b, 'c) t) : 'b list =
    (* Make an array of empty lists - one partition per domain *)
    let partitions : 'a list array = Array.make num_domains [] in
    (* Round-robin place each element in its partition *)
    List.iteri
      (fun i element ->
         let k = i mod num_domains in
         partitions.(k) <- element :: partitions.(k))
      node.input;
    (* Run the user’s transform in parallel *)
    let pool = Domainslib.Task.setup_pool ~num_domains () in
    let mapped_list_list =
      Domainslib.Task.run pool
      @@ fun () ->
      (* Launch one async per partition *)
      let futures =
        Array.map
          (fun partition ->
             Domainslib.Task.async pool (fun () ->
               List.map (fun element -> node.transform element node.context) partition))
          partitions
      in
      (* Wait and collect *)
      let mapped_list_array = Array.map (Domainslib.Task.await pool) futures in
      Array.to_list mapped_list_array
    in
    Domainslib.Task.teardown_pool pool;
    (* Flatten *)
    List.concat mapped_list_list
  ;;
  
  (** Run the node and get the outputs. *)
  let run_computation_with_context ?(num_domains : int = 4) (node : ('a, 'b, 'c) t)
    : 'b list
    =
    if num_domains = 0
    then computation_sequential node
    else computation_parallel num_domains node
  ;;