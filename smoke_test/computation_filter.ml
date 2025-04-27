open Nodes.Computation
open Nodes.Filter

(* Filter for even numbers *)
let even x = x mod 2 = 0

(* Compute the square of a number *)
let square x = x * x

let () =
  let even_numbers_list = run_filter { input = [ 1; 2; 3; 4; 5; 6 ]; pred = even } in
  let squared_list = run_computation { input = even_numbers_list; transform = square } in
  List.iter (Printf.printf "%d ") squared_list;
  print_newline ()
;;
