let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let run source =
  let result = Scanner.scan_tokens source in
  match result with
  | Ok _ -> false
  | Error _ -> true (* TODO: print returned errors? *)

let run_file path =
  let content = read_file path in
  if run content = true then exit 65

let run_prompt () =
  while true do
    print_string "> ";
    run (read_line ()) |> ignore;
  done

let main args =
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
