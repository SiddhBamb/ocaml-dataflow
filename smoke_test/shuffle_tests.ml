(* test Dataflowlib.Shuffle.shuffle_parallel *)

let () =
  (* create input list size 100 *)
  let input = [
    [("a", 1); ("b", 2); ("c", 3)];
    [("a", 4); ("b", 5); ("c", 6)];
    [("a", 7); ("b", 8); ("c", 9)];
    [("a", 10); ("b", 11); ("c", 12)];
    [("a", 13); ("b", 14); ("c", 15)];
    [("a", 16); ("b", 17); ("c", 18)];
    [("a", 19); ("b", 20); ("c", 21)];
    [("a", 22); ("b", 23); ("c", 24)];
    [("a", 25); ("b", 26); ("c", 27)];
    [("a", 28); ("b", 29); ("c", 30)];
    [("a", 31); ("b", 32); ("c", 33)];
    [("a", 34); ("b", 35); ("c", 36)];
    [("a", 37); ("b", 38); ("c", 39)];
    [("a", 40); ("b", 41); ("c", 42)];
    [("a", 43); ("b", 44); ("c", 45)];
    [("a", 46); ("b", 47); ("c", 48)];
    [("a", 49); ("b", 50); ("c", 51)];
    [("a", 52); ("b", 53); ("c", 54)];
    [("a", 55); ("b", 56); ("c", 57)];
    [("a", 58); ("b", 59); ("c", 60)];
    [("a", 61); ("b", 62); ("c", 63)];
    [("a", 64); ("b", 65); ("c", 66)];
    [("a", 67); ("b", 68); ("c", 69)];
    [("a", 70); ("b", 71); ("c", 72)];
    [("a", 73); ("b", 74); ("c", 75)];
    [("a", 76); ("b", 77); ("c", 78)];
    [("a", 79); ("b", 80); ("c", 81)];
    [("a", 82); ("b", 83); ("c", 84)];
    [("a", 85); ("b", 86); ("c", 87)];
    [("a", 88); ("b", 89); ("c", 90)];
    [("a", 91); ("b", 92); ("c", 93)];
    [("a", 94); ("b", 95); ("c", 96)];
    [("a", 97); ("b", 98); ("c", 99)];
    [("a", 100); ("b", 101); ("c", 102)];
  ] in
  let result = Dataflowlib.Shuffle.shuffle_parallel input in
  Printf.printf "Result: %s\n" (String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s: %s" k (String.concat ", " (List.map string_of_int v))) result));
  let sequential_result = Dataflowlib.Shuffle.shuffle_sequential input in
  Printf.printf "Sequential Result: %s\n" (String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s: %s" k (String.concat ", " (List.map string_of_int v))) sequential_result));
;;
