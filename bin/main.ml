let () =
  let len = Array.length Sys.argv in
  if len > 2 then (
    print_endline "Usage: olox [script]";
    exit 64
  )
  else if len = 2 then Olox.run_file Sys.argv.(1)
  (* else Olox.run_prompt () *)
