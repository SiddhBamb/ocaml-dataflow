(** A polymorphic filter node.
    The user supplies a predicate; only elements that satisfy it are kept. *)
    type 'a filter =
    { input : 'a list (** Elements to test *)
    ; pred : 'a -> bool (** Keep the element if pred x = true *)
    }
  
  let filter_sequential (node : 'a filter) = List.filter node.pred node.input
  
  let filter_parallel (num_domains : int) (node : 'a filter) =
    (* Round-robin partition into one list per domain *)
    let partitions : 'a list array = Array.make num_domains [] in
    List.iteri
      (fun i element ->
         let k = i mod num_domains in
         partitions.(k) <- element :: partitions.(k))
      node.input;
    (* Run List.filter concurrently on each partition *)
    let pool = Domainslib.Task.setup_pool ~num_domains () in
    let filtered_list_list =
      Domainslib.Task.run pool
      @@ fun () ->
      Array.init num_domains (fun k ->
        Domainslib.Task.await
          pool
          (Domainslib.Task.async pool (fun () -> List.filter node.pred partitions.(k))))
      |> Array.to_list
    in
    Domainslib.Task.teardown_pool pool;
    (* Flatten *)
    List.concat filtered_list_list
  ;;
  
  (** Execute the filter node and return the surviving elements. *)
  let run_filter ?(num_domains : int = 4) (node : 'a filter) : 'a list =
    if num_domains = 0 then filter_sequential node else filter_parallel num_domains node
  ;;