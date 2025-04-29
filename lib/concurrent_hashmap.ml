(* lock-free concurrent hashmap implementation using CAS for insertion and deletion *)
module ConcurrentHashMap : sig
  type ('k, 'v) t

  val create : ?expected_size:int -> unit -> ('k, 'v) t
  val insert : ('k, 'v) t -> 'k -> 'v -> unit
  val read : ('k, 'v) t -> 'k -> 'v list
  val delete : ('k, 'v) t -> 'k -> unit
  val clear : ('k, 'v) t -> unit
  val keys : ('k, 'v) t -> 'k list
  val iter : ('k -> 'v list -> unit) -> ('k, 'v) t -> unit
end = struct
  type ('k, 'v) node =
    { key : 'k
    ; value : 'v
    ; next : ('k, 'v) node option
    }

  type ('k, 'v) t =
    { buckets : ('k, 'v) node option Atomic.t array
    ; mask : int (* power-of-two capacity - 1 *)
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
  ;;

  let create ?(expected_size = 1000) () =
    let cap = round_pow2 (max 16 expected_size) in
    let buckets = Array.init cap (fun _ -> Atomic.make None) in
    { buckets; mask = cap - 1 }
  ;;

  (* one node per insertion; immutable once built, if editing needed just make a new node *)
  let insert map key value =
    let idx = Hashtbl.hash key land map.mask in
    let cell = map.buckets.(idx) in
    (* keep trying to insert if the cell changes using CAS *)
    let rec loop () =
      let old = Atomic.get cell in
      let node = { key; value; next = old } in
      if not (Atomic.compare_and_set cell old (Some node)) then loop ()
    in
    loop ()
  ;;

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
  ;;

  let delete map key =
    let idx = Hashtbl.hash key land map.mask in
    let cell = map.buckets.(idx) in
    let rec loop () =
      let old = Atomic.get cell in
      (* bail if not present *)
      let rec exists = function
        | None -> false
        | Some n when n.key = key -> true
        | Some n -> exists n.next
      in
      if not (exists old)
      then ()
      else (
        let rec filter = function
          | None -> None
          | Some n ->
            let nxt = filter n.next in
            if n.key = key then nxt else Some { n with next = nxt }
        in
        let new_head = filter old in
        if not (Atomic.compare_and_set cell old new_head) then loop ())
    in
    loop ()
  ;;

  let clear map = Array.iter (fun cell -> Atomic.set cell None) map.buckets

  let keys map =
    let seen = Hashtbl.create (Array.length map.buckets) in
    let acc = ref [] in
    Array.iter
      (fun cell ->
         let rec trav = function
           | None -> ()
           | Some n ->
             if not (Hashtbl.mem seen n.key)
             then (
               Hashtbl.add seen n.key ();
               acc := n.key :: !acc);
             trav n.next
         in
         trav (Atomic.get cell))
      map.buckets;
    !acc
  ;;

  let iter f map =
    Array.iter
      (fun cell ->
         let rec traverse = function
           | None -> ()
           | Some n ->
             f n.key [ n.value ];
             traverse n.next
         in
         traverse (Atomic.get cell))
      map.buckets
  ;;
end

(* This one is kind of slow *)
(* Doesn't use CAS, instead is lock-based *)
module CUSTOM_ConcurrentHashMap = struct
  (* A single bucket: a mutex + a mutable chain of (key * vs list) *)
  type ('k, 'v) bucket =
    { mutex : Mutex.t
    ; mutable chain : ('k * 'v list) list
    }

  type ('k, 'v) t =
    { buckets : ('k, 'v) bucket array Atomic.t
    ; size : int Atomic.t
    ; resize_lock : Mutex.t
    ; load_factor : float
    }

  (* Create with an initial capacity large enough to hold ~expected_size
     at the given load_factor, so you never have to resize. *)
  let create ?(expected_size = 1000) ?(load_factor = 0.9) () =
    let cap = max 16 (int_of_float (float expected_size /. load_factor)) in
    let make_bucket () = { mutex = Mutex.create (); chain = [] } in
    { buckets = Atomic.make (Array.init cap (fun _ -> make_bucket ()))
    ; size = Atomic.make 0
    ; resize_lock = Mutex.create ()
    ; load_factor
    }
  ;;

  (* bucket array and its length helpers *)
  let get_buckets map = Atomic.get map.buckets
  let capacity map = Array.length (get_buckets map)

  (* reallocate array to double size *)
  let resize_if_needed map =
    let sz = Atomic.get map.size in
    let cap = capacity map in
    if float sz /. float cap > map.load_factor
    then (
      Mutex.lock map.resize_lock;
      (* Re-check to avoid redundant grows *)
      let sz2 = Atomic.get map.size in
      let cap2 = capacity map in
      if cap2 = cap && float sz2 /. float cap2 > map.load_factor
      then (
        let old = get_buckets map in
        let new_cap = cap2 * 2 in
        let make_bucket () = { mutex = Mutex.create (); chain = [] } in
        let newb = Array.init new_cap (fun _ -> make_bucket ()) in
        (* Migrate each old bucket under its own lock *)
        Array.iter
          (fun bkt ->
             Mutex.lock bkt.mutex;
             List.iter
               (fun (k, vs) ->
                  let idx = Hashtbl.hash k mod new_cap in
                  let nb = newb.(idx) in
                  nb.chain <- (k, vs) :: nb.chain)
               bkt.chain;
             Mutex.unlock bkt.mutex)
          old;
        (* Swap buckets in one atomic write *)
        Atomic.set map.buckets newb);
      Mutex.unlock map.resize_lock)
  ;;

  let insert map key value =
    let buckets = get_buckets map in
    let idx = Hashtbl.hash key mod Array.length buckets in
    let bkt = buckets.(idx) in
    Mutex.lock bkt.mutex;
    (* rebuild chain with new value at head of this key’s list *)
    let rec rebuild acc = function
      | [] -> List.rev_append acc [ key, [ value ] ]
      | (k, vs) :: tl when k = key -> List.rev_append acc ((k, value :: vs) :: tl)
      | hd :: tl -> rebuild (hd :: acc) tl
    in
    bkt.chain <- rebuild [] bkt.chain;
    Mutex.unlock bkt.mutex;
    ignore (Atomic.fetch_and_add map.size 1);
    resize_if_needed map
  ;;

  let read map key =
    let buckets = get_buckets map in
    let idx = Hashtbl.hash key mod Array.length buckets in
    let bkt = buckets.(idx) in
    Mutex.lock bkt.mutex;
    let vs =
      match List.assoc_opt key bkt.chain with
      | Some lst -> List.rev lst
      | None -> []
    in
    Mutex.unlock bkt.mutex;
    vs
  ;;

  let delete map key =
    let buckets = get_buckets map in
    let idx = Hashtbl.hash key mod Array.length buckets in
    let bkt = buckets.(idx) in
    Mutex.lock bkt.mutex;
    let removed =
      match List.assoc_opt key bkt.chain with
      | Some lst -> List.length lst
      | None -> 0
    in
    bkt.chain <- List.remove_assoc key bkt.chain;
    Mutex.unlock bkt.mutex;
    if removed > 0 then ignore (Atomic.fetch_and_add map.size (-removed))
  ;;

  let clear map =
    Mutex.lock map.resize_lock;
    let buckets = get_buckets map in
    Array.iter
      (fun bkt ->
         Mutex.lock bkt.mutex;
         bkt.chain <- [];
         Mutex.unlock bkt.mutex)
      buckets;
    Atomic.set map.size 0;
    Mutex.unlock map.resize_lock
  ;;

  let keys map =
    let acc = ref [] in
    let buckets = get_buckets map in
    Array.iter
      (fun bkt ->
         Mutex.lock bkt.mutex;
         List.iter (fun (k, _) -> acc := k :: !acc) bkt.chain;
         Mutex.unlock bkt.mutex)
      buckets;
    !acc
  ;;
end
