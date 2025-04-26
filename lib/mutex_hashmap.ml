module ConcurrentHashMap = struct
  (* BEGIN READ-WRITE LOCK IMPLEMENTATION **)

  module Rw_lock = struct
    type t = {
      mutex   : Mutex.t;
      cond    : Condition.t;
      mutable readers : int;
      mutable writer  : bool;
    }

    let create () =
      { mutex   = Mutex.create ();
        cond    = Condition.create ();
        readers = 0;
        writer  = false }

    let read_lock l =
      Mutex.lock l.mutex;
      while l.writer do
        Condition.wait l.cond l.mutex
      done;
      l.readers <- l.readers + 1;
      Mutex.unlock l.mutex

    let read_unlock l =
      Mutex.lock l.mutex;
      l.readers <- l.readers - 1;
      if l.readers = 0 then Condition.signal l.cond;
      Mutex.unlock l.mutex

    let write_lock l =
      Mutex.lock l.mutex;
      while l.writer || l.readers > 0 do
        Condition.wait l.cond l.mutex
      done;
      l.writer <- true;
      Mutex.unlock l.mutex

    let write_unlock l =
      Mutex.lock l.mutex;
      l.writer <- false;
      Condition.broadcast l.cond;
      Mutex.unlock l.mutex
  end

  (* BEGIN ACTUAL HASHMAP IMPLEMENTATION *)

  type 'a bucket = {
    mutex  : Mutex.t;
    mutable values : 'a list;
  }

  type 'a t = {
    mutable table    : (int, 'a bucket) Hashtbl.t;
    mutable size     : int;
    mutable capacity : int;
    rwlock           : Rw_lock.t;
  }

  let create () =
    { table    = Hashtbl.create 16;
      size     = 0;
      capacity = 16;
      rwlock   = Rw_lock.create () }

  (* resize, assume global write lock held *)
  let resize_body map =
    let new_capacity = map.capacity * 2 in
    let new_table = Hashtbl.create new_capacity in
    Hashtbl.iter (fun key bucket ->
      (* can directly read bucket values since write lock held *)
      List.iter (fun value ->
        let idx = Hashtbl.hash key mod new_capacity in
        let b =
          try Hashtbl.find new_table idx
          with Not_found ->
            let b = { mutex = Mutex.create (); values = [] } in
            Hashtbl.add new_table idx b;
            b
        in
        b.values <- value :: b.values
      ) bucket.values
    ) map.table;
    map.table    <- new_table;
    map.capacity <- new_capacity

  let resize map =
    Rw_lock.write_lock map.rwlock;
    resize_body map;
    Rw_lock.write_unlock map.rwlock

  let insert map key value =
    (* read locking until we need map-wide change *)
    Rw_lock.read_lock map.rwlock;
    (* check if load exceeds threshold of 0.75 --> resize if needed *)
    if float_of_int map.size /. float_of_int map.capacity > 0.75 then begin
      (* upgrade to write-lock to update whole map *)
      Rw_lock.read_unlock map.rwlock;
      Rw_lock.write_lock map.rwlock;
      resize_body map;
      (* downgrade lock to read-lock *)
      Rw_lock.write_unlock map.rwlock;
      Rw_lock.read_lock map.rwlock;
    end;
    (* now look at buckets and actually insert *)
    let idx = Hashtbl.hash key mod map.capacity in
    let bucket =
      try Hashtbl.find map.table idx
      with Not_found ->
        (* if making new bucket need exclusive lock on map *)
        Rw_lock.read_unlock map.rwlock;
        Rw_lock.write_lock map.rwlock;
        let b = { mutex = Mutex.create (); values = [] } in
        Hashtbl.add map.table idx b;
        Rw_lock.write_unlock map.rwlock;
        Rw_lock.read_lock map.rwlock;
        b
    in
    (* lock the actual bucket's write mutex - allows fine grained concurrency *)
    Mutex.lock bucket.mutex;
    bucket.values <- value :: bucket.values;
    map.size <- map.size + 1;
    Mutex.unlock bucket.mutex;

    (* release all locks *)
    Rw_lock.read_unlock map.rwlock

  let read map key =
    Rw_lock.read_lock map.rwlock;
    let res =
      let idx = Hashtbl.hash key mod map.capacity in
      try
        let bucket = Hashtbl.find map.table idx in
        Mutex.lock bucket.mutex;
        let vs = List.rev bucket.values in
        Mutex.unlock bucket.mutex;
        Some vs
      with Not_found -> None
    in
    Rw_lock.read_unlock map.rwlock;
    res

  let delete map key =
    Rw_lock.read_lock map.rwlock;
    let idx = Hashtbl.hash key mod map.capacity in
    (match Hashtbl.find_opt map.table idx with
     | Some bucket ->
       Mutex.lock bucket.mutex;
       bucket.values <- [];
       map.size <- max 0 (map.size - 1);
       Mutex.unlock bucket.mutex
     | None -> ());
    Rw_lock.read_unlock map.rwlock

  let clear map =
    Rw_lock.write_lock map.rwlock;
    Hashtbl.clear map.table;
    map.size     <- 0;
    map.capacity <- 16;
    Rw_lock.write_unlock map.rwlock
end
