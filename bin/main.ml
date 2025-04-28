let () = 
  print_endline "Starting Word Count Demos";
  
  (* check if realistic demo was requested *)
  let run_realistic = 
    Array.length Sys.argv > 1 && Sys.argv.(1) = "--realistic"
  in
  
  if run_realistic then begin
    Printf.printf "\n===== Running Realistic Fault Tolerance Demo =====\n";
    Fault_tolerance.Fault_tolerance_demo_realistic.run_realistic_fault_tolerance_demo ()
  end else begin
    (* run only dataflow demo for testing the parallel implementation *)
    Printf.printf "\n===== Running Dataflow Demo =====\n";
    Fault_tolerance.Fault_tolerance_demo.run_dataflow_demo ();
    
    print_endline "\nDataflow demo completed successfully!"
  end
