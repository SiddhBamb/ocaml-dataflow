let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (* run the user function *)
  let t1 = Unix.gettimeofday () in
  t1 -. t0, r
;;

let run ?(repeat = 5) (label : string) (f : unit -> 'a) : float list =
  let rec loop i acc =
    if i = 0
    then acc
    else (
      let dt, _ = time f in
      loop (i - 1) (dt :: acc))
  in
  let samples = loop repeat [] in
  let mean = List.fold_left ( +. ) 0.0 samples /. float repeat in
  Printf.printf "%s - mean over %d runs: %.4f s\n%!" label repeat mean;
  samples
;;
