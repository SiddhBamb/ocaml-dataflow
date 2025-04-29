(** Partitioned collection: one bucket per worker. *)
type 'a t =
  { buckets : 'a list array (** length = n *)
  ; n : int (** cached length *)
  }

(* Build from a flat list using round-robin *)
let of_list ~n lst : 'a t =
  let buckets = Array.make n [] in
  List.iteri
    (fun i x ->
       let k = i mod n in
       buckets.(k) <- x :: buckets.(k))
    lst;
  Array.iteri (fun i l -> buckets.(i) <- List.rev l) buckets;
  { buckets; n }
;;

(* Keep the partitioning, just transform the contents *)
let map f s = { s with buckets = Array.map (List.map f) s.buckets }
let filter p s = { s with buckets = Array.map (List.filter p) s.buckets }
let concat s = List.concat (Array.to_list s.buckets)

(* Re-bucket when the next stage wants a different degree of parallelism *)
let rebucket ~n_new ({ buckets; _ } as s) =
  if n_new = s.n
  then s
  else (
    let dst = Array.make n_new [] in
    Array.iter
      (List.iter (fun x ->
         dst.(Stdlib.Hashtbl.hash x mod n_new)
         <- x :: dst.(Stdlib.Hashtbl.hash x mod n_new)))
      buckets;
    Array.iteri (fun i l -> dst.(i) <- List.rev l) dst;
    { buckets = dst; n = n_new })
;;
