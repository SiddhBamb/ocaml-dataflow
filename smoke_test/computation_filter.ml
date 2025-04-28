open Nodes

(* keep only even numbers *)
let even x = x mod 2 = 0

(* square a number *)
let square x = x * x

let () =
  let shards_even =
    Filter.run ~num_domains:4 { input = [ 1; 2; 3; 4; 5; 6 ]; pred = even }
  in
  let shards_squared = Shards.map square shards_even in
  let result = Shards.concat shards_squared in
  List.iter (Printf.printf "%d ") result;
  print_newline ()
;;
