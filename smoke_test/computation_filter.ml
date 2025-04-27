open Nodes.Computation
open Nodes.Filter

(* Filter function *)
let even x = x mod 2 = 0

(* Computation function *)
let square x = x * x

let () =
  List.iter
    (Printf.printf "%d ")
    (let even_list = run_filter { input = [ 1; 2; 3; 4; 5; 6 ]; pred = even } in
     run { input = even_list; transform = square });
  print_newline ()
;;
