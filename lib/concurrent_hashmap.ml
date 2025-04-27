(* lock-free concurrent hashmap implementation using CAS for insertion and deletion *)
module ConcurrentHashMap : sig
  type ('k,'v) t
  val create : ?expected_size:int -> unit -> ('k,'v) t
  val insert : ('k,'v) t -> 'k -> 'v -> unit
  val read   : ('k,'v) t -> 'k -> 'v list
  val delete : ('k,'v) t -> 'k -> unit
  val clear  : ('k,'v) t -> unit
  val keys   : ('k,'v) t -> 'k list
end = struct
  type ('k,'v) node = {
    key   : 'k;
    value : 'v;
    next  : ('k,'v) node option;
  }

  type ('k,'v) t = {
    buckets : (('k,'v) node option) Atomic.t array;
    mask    : int;  (* power-of-two capacity - 1 *)
  }

  (* make sizes always a power of 2 (since amortization stuff is 2xing cap) *)
  let round_pow2 x =
    let x = x - 1 in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    x + 1

  let create ?(expected_size=1000) () =
    let cap = round_pow2 (max 16 expected_size) in
    let buckets = Array.init cap (fun _ -> Atomic.make None) in
    { buckets; mask = cap - 1 }

  
  (* one node per insertion; immutable once built, if editing needed just make a new node *)
  let insert map key value =
    let idx = Hashtbl.hash key land map.mask in
    let cell = map.buckets.(idx) in
    (* keep trying to insert if the cell changes using CAS *)
    let rec loop () =
      let old = Atomic.get cell in
      let node = { key; value; next = old } in
      if not (Atomic.compare_and_set cell old (Some node)) then
        loop ()
    in
    loop ()

  let read map key =
    (* return a list of values for the key *)
    let idx = Hashtbl.hash key land map.mask in
    let rec traverse acc = function
      | None -> acc
      | Some n ->
        let acc = if n.key = key then n.value :: acc else acc in
        traverse acc n.next
    in
    traverse [] (Atomic.get map.buckets.(idx))

  let delete map key =
    let idx  = Hashtbl.hash key land map.mask in
    let cell = map.buckets.(idx) in
    let rec loop () =
      let old = Atomic.get cell in
      (* bail if not present *)
      let rec exists = function
        | None -> false
        | Some n when n.key = key -> true
        | Some n -> exists n.next
      in
      if not (exists old) then () else begin
        let rec filter = function
          | None -> None
          | Some n ->
            let nxt = filter n.next in
            if n.key = key then nxt else Some { n with next = nxt }
        in
        let new_head = filter old in
        if not (Atomic.compare_and_set cell old new_head) then
          loop ()
      end
    in
    loop ()

  let clear map =
    Array.iter (fun cell -> Atomic.set cell None) map.buckets

  let keys map =
    let seen = Hashtbl.create (Array.length map.buckets) in
    let acc  = ref [] in
    Array.iter (fun cell ->
      let rec trav = function
        | None -> ()
        | Some n ->
          if not (Hashtbl.mem seen n.key) then begin
            Hashtbl.add seen n.key ();
            acc := n.key :: !acc
          end;
          trav n.next
      in
      trav (Atomic.get cell)
    ) map.buckets;
    !acc
end





(* This one is kind of slow *)
(* Doesn't use CAS, instead is lock-based *)
module CUSTOM_ConcurrentHashMap = struct
  (* A single bucket: a mutex + a mutable chain of (key * vs list) *)
  type ('k, 'v) bucket = {
    mutex   : Mutex.t;
    mutable chain : ('k * 'v list) list;
  }

  type ('k, 'v) t = {
    buckets     : ('k, 'v) bucket array Atomic.t;
    size        : int Atomic.t;
    resize_lock : Mutex.t;
    load_factor : float;
  }

  (* Create with an initial capacity large enough to hold ~expected_size
     at the given load_factor, so you never have to resize. *)
  let create ?(expected_size=1000) ?(load_factor=0.9) () =
    let cap = 
      max 16
        (int_of_float (float expected_size /. load_factor))
    in
    let make_bucket () = { mutex = Mutex.create (); chain = [] } in
    {
      buckets     = Atomic.make (Array.init cap (fun _ -> make_bucket ()));
      size        = Atomic.make 0;
      resize_lock = Mutex.create ();
      load_factor;
    }

  (* Helpers to grab the current bucket-array and its length *)
  let get_buckets map = Atomic.get map.buckets
  let capacity map  = Array.length (get_buckets map)

  (* Double-checked resize: allocate new array, migrate under per-bucket locks,
     then atomically swap it in under resize_lock. *)
  let resize_if_needed map =
    let sz  = Atomic.get map.size in
    let cap = capacity map in
    if float sz /. float cap > map.load_factor then begin
      Mutex.lock map.resize_lock;
      (* Re-check to avoid redundant grows *)
      let sz2  = Atomic.get map.size in
      let cap2 = capacity map in
      if cap2 = cap && float sz2 /. float cap2 > map.load_factor then begin
        let old = get_buckets map in
        let new_cap = cap2 * 2 in
        let make_bucket () = { mutex = Mutex.create (); chain = [] } in
        let newb = Array.init new_cap (fun _ -> make_bucket ()) in
        (* Migrate each old bucket under its own lock *)
        Array.iter (fun bkt ->
          Mutex.lock bkt.mutex;
          List.iter (fun (k,vs) ->
            let idx = Hashtbl.hash k mod new_cap in
            let nb  = newb.(idx) in
            nb.chain <- (k,vs)::nb.chain
          ) bkt.chain;
          Mutex.unlock bkt.mutex
        ) old;
        (* Swap buckets in one atomic write *)
        Atomic.set map.buckets newb
      end;
      Mutex.unlock map.resize_lock
    end

  let insert map key value =
    let buckets = get_buckets map in
    let idx     = Hashtbl.hash key mod Array.length buckets in
    let bkt     = buckets.(idx) in
    Mutex.lock bkt.mutex;
    (* rebuild chain with new value at head of this key’s list *)
    let rec rebuild acc = function
      | [] -> List.rev_append acc [ key, [value] ]
      | (k,vs)::tl when k = key ->
          List.rev_append acc ((k, value::vs)::tl)
      | hd::tl -> rebuild (hd::acc) tl
    in
    bkt.chain <- rebuild [] bkt.chain;
    Mutex.unlock bkt.mutex;
    ignore (Atomic.fetch_and_add map.size 1);
    resize_if_needed map

  let read map key =
    let buckets = get_buckets map in
    let idx     = Hashtbl.hash key mod Array.length buckets in
    let bkt     = buckets.(idx) in
    Mutex.lock bkt.mutex;
    let vs = match List.assoc_opt key bkt.chain with
      | Some lst -> List.rev lst
      | None     -> []
    in
    Mutex.unlock bkt.mutex;
    vs

  let delete map key =
    let buckets = get_buckets map in
    let idx     = Hashtbl.hash key mod Array.length buckets in
    let bkt     = buckets.(idx) in
    Mutex.lock bkt.mutex;
    let removed =
      match List.assoc_opt key bkt.chain with
      | Some lst -> List.length lst
      | None     -> 0
    in
    bkt.chain <- List.remove_assoc key bkt.chain;
    Mutex.unlock bkt.mutex;
    if removed > 0 then ignore (Atomic.fetch_and_add map.size (-removed))

  let clear map =
    Mutex.lock map.resize_lock;
    let buckets = get_buckets map in
    Array.iter (fun bkt ->
      Mutex.lock bkt.mutex;
      bkt.chain <- [];
      Mutex.unlock bkt.mutex
    ) buckets;
    Atomic.set map.size 0;
    Mutex.unlock map.resize_lock

  let keys map =
    let acc = ref [] in
    let buckets = get_buckets map in
    Array.iter (fun bkt ->
      Mutex.lock bkt.mutex;
      List.iter (fun (k, _) -> acc := k :: !acc) bkt.chain;
      Mutex.unlock bkt.mutex
    ) buckets;
    !acc
end



(* OLD 2 (faster but still bad) *)


(* module ConcurrentHashMap = struct
  type ('k,'v) bucket = {
    mutex   : Mutex.t;
    mutable chain : ('k * 'v list) list;
  }

  type ('k,'v) t = {
    mutable buckets     : ('k,'v) bucket array;
    size        : int Atomic.t;       (* atomic counter *)
    resize_lock : Mutex.t;            (* only for resizing *)
  }

  let create () =
    let initial = 1000 in
    let make_bucket () = { mutex = Mutex.create (); chain = [] } in
    {
      buckets     = Array.init initial (fun _ -> make_bucket ());
      size        = Atomic.make 0;
      resize_lock = Mutex.create ();
    }

  let resize_body map =
    let old = map.buckets in
    let new_cap = Array.length old * 2 in
    let make_bucket () = { mutex = Mutex.create (); chain = [] } in
    let newb = Array.init new_cap (fun _ -> make_bucket ()) in
    (* migrate entries *)
    Array.iter (fun bkt ->
      Mutex.lock bkt.mutex;
      List.iter (fun (k, vs) ->
        let idx = Hashtbl.hash k mod new_cap in
        let nb = newb.(idx) in
        nb.chain <- (k,vs)::nb.chain
      ) bkt.chain;
      Mutex.unlock bkt.mutex
    ) old;
    map.buckets <- newb

  let maybe_resize map =
    let sz    = Atomic.get map.size in
    let cap   = Array.length map.buckets in
    if sz > cap * 3 / 4 then begin
      Mutex.lock map.resize_lock;
      (* recheck under lock in case someone else already grew it *)
      let sz2  = Atomic.get map.size in
      let cap2 = Array.length map.buckets in
      if sz2 > cap2 * 3 / 4 then
        resize_body map;
      Mutex.unlock map.resize_lock
    end

  let insert map key value =
    let idx    = Hashtbl.hash key mod Array.length map.buckets in
    let bucket = map.buckets.(idx) in
    (* only bucket‐lock for the chain update *)
    Mutex.lock bucket.mutex;
    let rec aux acc = function
      | [] -> List.rev_append acc [ key, [value] ]
      | (k,vs)::tl when k = key ->
         List.rev_append acc ((k, value::vs)::tl)
      | hd::tl -> aux (hd::acc) tl
    in
    bucket.chain <- aux [] bucket.chain;
    Mutex.unlock bucket.mutex;
    (* bump size lock‐free *)
    ignore (Atomic.fetch_and_add map.size 1);
    (* occasionally resize—but only one thread at a time *)
    maybe_resize map

  let read map key =
    let idx    = Hashtbl.hash key mod Array.length map.buckets in
    let bucket = map.buckets.(idx) in
    Mutex.lock bucket.mutex;
    let vs = match List.assoc_opt key bucket.chain with
      | Some lst -> List.rev lst
      | None     -> []
    in
    Mutex.unlock bucket.mutex;
    vs

  (* delete all values for a given key *)
  let delete map key =
    let idx    = Hashtbl.hash key mod Array.length map.buckets in
    let bucket = map.buckets.(idx) in
    Mutex.lock bucket.mutex;
    (* count how many we’re about to remove *)
    let removed =
      match List.assoc_opt key bucket.chain with
      | Some lst -> List.length lst
      | None     -> 0
    in
    bucket.chain <- List.remove_assoc key bucket.chain;
    Mutex.unlock bucket.mutex;
    (* adjust size atomically *)
    if removed > 0 then ignore (Atomic.fetch_and_add map.size (-removed))

  (* clear the entire map *)
  let clear map =
    (* prevent concurrent resizing or inserts from racing the clear *)
    Mutex.lock map.resize_lock;
    Array.iter (fun bucket ->
      Mutex.lock bucket.mutex;
      bucket.chain <- [];
      Mutex.unlock bucket.mutex
    ) map.buckets;
    Atomic.set map.size 0;
    Mutex.unlock map.resize_lock

  (* return a list of all keys *)
  let keys map =
    let acc = ref [] in
    Array.iter (fun bucket ->
      Mutex.lock bucket.mutex;
      (* collect just the keys from this bucket *)
      List.iter (fun (k, _) -> acc := k :: !acc) bucket.chain;
      Mutex.unlock bucket.mutex
    ) map.buckets;
    !acc
end *)


(* OLD HASHMAP BELOW *)


(* module ConcurrentHashMap = struct
  (* BEGIN READER-WRITER LOCK IMPLEMENTATION *)
  module Rw_lock = struct
    type t = {
      mutex   : Mutex.t;        (* protects internal state *)
      cond    : Condition.t;    (* for waiting threads *)
      mutable readers : int;     (* number of active readers *)
      mutable writer  : bool;    (* writer active flag *)
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

  (* BEGIN HASHMAP IMPLEMENTATION *)
  type ('k,'v) bucket = {
    mutex  : Mutex.t;
    mutable chain : ('k * 'v list) list;
  }

  type ('k,'v) t = {
    mutable buckets  : ('k,'v) bucket array;
    mutable size     : int;
    mutable capacity : int;
    rwlock           : Rw_lock.t;
  }

  let create () =
    let initial = 16 in
    let make_bucket () = { mutex = Mutex.create (); chain = [] } in
    { buckets = Array.init initial (fun _ -> make_bucket ());
      size = 0;
      capacity = initial;
      rwlock = Rw_lock.create () }

  (* resize (internal method), assumes you are holding global write lock *)
  let resize_body map =
    let new_cap = map.capacity * 2 in
    let make_bucket () = { mutex = Mutex.create (); chain = [] } in
    let new_buckets = Array.init new_cap (fun _ -> make_bucket ()) in
    (* Move each bucket's entries into new buckets *)
    Array.iter (fun bkt ->
      Mutex.lock bkt.mutex;
      List.iter (fun (k,vs) ->
        let idx = Hashtbl.hash k mod new_cap in
        let nb = new_buckets.(idx) in
        nb.chain <- (k, vs) :: nb.chain
      ) bkt.chain;
      Mutex.unlock bkt.mutex
    ) map.buckets;
    map.buckets <- new_buckets;
    map.capacity <- new_cap

  (* resize *)
  let resize map =
    Rw_lock.write_lock map.rwlock;
    resize_body map;
    Rw_lock.write_unlock map.rwlock

  (* insert a value at end of list for key *)
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
    let bucket = map.buckets.(idx) in
    Mutex.lock bucket.mutex;
    (* create new entry or append to end *)
    let rec aux acc = function
      | [] -> List.rev_append acc [(key, [value])]
      | (k,vs)::tl when k = key -> List.rev_append acc ((k, value::vs)::tl)
      | hd::tl -> aux (hd::acc) tl
    in
    bucket.chain <- aux [] bucket.chain;
    map.size <- map.size + 1;
    Mutex.unlock bucket.mutex;
    (* release locks (shared read lock) *)
    Rw_lock.read_unlock map.rwlock

  (* returns list of all values for a key *)
  let read map key =
    Rw_lock.read_lock map.rwlock;
    let idx = Hashtbl.hash key mod map.capacity in
    let bucket = map.buckets.(idx) in
    Mutex.lock bucket.mutex;
    let vs =
      match List.assoc_opt key bucket.chain with
      | Some lst -> List.rev lst  (* preserve insertion order *)
      | None -> []
    in
    Mutex.unlock bucket.mutex;
    Rw_lock.read_unlock map.rwlock;
    vs

  (* deletes list of all values for a key *)
  let delete map key =
    Rw_lock.read_lock map.rwlock;
    let idx = Hashtbl.hash key mod map.capacity in
    let bucket = map.buckets.(idx) in
    Mutex.lock bucket.mutex;
    let removed =
      match List.assoc_opt key bucket.chain with
      | Some lst -> List.length lst
      | None -> 0
    in
    bucket.chain <- List.remove_assoc key bucket.chain;
    map.size <- map.size - removed;
    Mutex.unlock bucket.mutex;
    Rw_lock.read_unlock map.rwlock

  (* clears all buckets *)
  let clear map =
    Rw_lock.write_lock map.rwlock;
    Array.iter (fun bkt ->
      Mutex.lock bkt.mutex;
      bkt.chain <- [];
      Mutex.unlock bkt.mutex
    ) map.buckets;
    map.size <- 0;
    Rw_lock.write_unlock map.rwlock

  (* returns list of all keys *)
  let keys map =
    Rw_lock.read_lock map.rwlock;
    let acc =
      Array.fold_left (fun ks bkt ->
        Mutex.lock bkt.mutex;
        let bucket_keys = List.map fst bkt.chain in
        Mutex.unlock bkt.mutex;
        ks @ bucket_keys
      ) [] map.buckets
    in
    Rw_lock.read_unlock map.rwlock;
    acc
end *)








(* OLD CODE, BAD *)



(* 
module ConcurrentHashMap = struct
  (* rw *)
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
      while l.writer do Condition.wait l.cond l.mutex done;
      l.readers <- l.readers + 1;
      Mutex.unlock l.mutex

    let read_unlock l =
      Mutex.lock l.mutex;
      l.readers <- l.readers - 1;
      if l.readers = 0 then Condition.signal l.cond;
      Mutex.unlock l.mutex

    let write_lock l =
      Mutex.lock l.mutex;
      while l.writer || l.readers > 0 do Condition.wait l.cond l.mutex done;
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

  (* resize, assuming write-lock held *)
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

  (* insert value into list for key *)
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
      try Hashtbl.find map.table idx with
      | Not_found ->
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
  ;;

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
      with
      | Not_found -> None
    in
    Rw_lock.read_unlock map.rwlock;
    res
  ;;

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
  ;;

  let clear map =
    Rw_lock.write_lock map.rwlock;
    Hashtbl.clear map.table;
    map.size <- 0;
    map.capacity <- 16;
    Rw_lock.write_unlock map.rwlock
  ;;
end
end *)
