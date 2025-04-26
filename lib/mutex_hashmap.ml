module ConcurrentHashMap = struct
  type 'a bucket = {
    mutex : Mutex.t;
    mutable values : 'a list;
  }

  type 'a t = {
    mutable table : (int, 'a bucket) Hashtbl.t;
    mutable size : int;
    mutable capacity : int;
    map_mutex : Mutex.t;
  }

  (* create map *)
  let create () =
    { table = Hashtbl.create 16;
      size = 0;
      capacity = 16;
      map_mutex = Mutex.create () }

  (* double capacity if full/load factor *)
  let resize map =
    Mutex.lock map.map_mutex;
    let new_capacity = map.capacity * 2 in
    let new_table = Hashtbl.create new_capacity in
    (* migrate entries with rehash *)
    Hashtbl.iter (fun key bucket ->
      Mutex.lock bucket.mutex;
      List.iter (fun value ->
        let index = Hashtbl.hash key mod new_capacity in
        let new_bucket = 
          try Hashtbl.find new_table index
          with Not_found -> { mutex = Mutex.create (); values = [] }
        in
        new_bucket.values <- value :: new_bucket.values;
        Hashtbl.replace new_table index new_bucket
      ) bucket.values;
      Mutex.unlock bucket.mutex
    ) map.table;
    map.table <- new_table;
    map.capacity <- new_capacity;
    Mutex.unlock map.map_mutex

  (* insert *)
  let insert map key value =
    (* resize at load factor 0.75 *)
    if float_of_int map.size /. float_of_int map.capacity > 0.75 then
      resize map;
    
    let index = Hashtbl.hash key mod map.capacity in
    let bucket =
      try Hashtbl.find map.table index
      with Not_found ->
        let b = { mutex = Mutex.create (); values = [] } in
        Hashtbl.add map.table index b;
        b
    in
    Mutex.lock bucket.mutex;
    bucket.values <- value :: bucket.values;
    map.size <- map.size + 1;
    Mutex.unlock bucket.mutex

  (* read *)
  let read map key =
    let index = Hashtbl.hash key mod map.capacity in
    try
      let bucket = Hashtbl.find map.table index in
      Mutex.lock bucket.mutex;
      let values = bucket.values in
      Mutex.unlock bucket.mutex;
      Some (List.rev values)
    with Not_found -> None

  (* delete key *)
  let delete map key =
    let index = Hashtbl.hash key mod map.capacity in
    try
      let bucket = Hashtbl.find map.table index in
      Mutex.lock bucket.mutex;
      bucket.values <- List.filter (fun _ -> false) bucket.values; (* Clear the list *)
      map.size <- map.size - 1;
      Mutex.unlock bucket.mutex;
    with Not_found -> ()

  (* clear map *)
  let clear map =
    Mutex.lock map.map_mutex;
    Hashtbl.clear map.table;
    map.size <- 0;
    map.capacity <- 16;
    Mutex.unlock map.map_mutex
end
