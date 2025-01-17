let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let run source = Lexer.scan_tokens source
(*
  TODO:
  1. Initialize scanner
  2. Scan tokens
  3. Print each token one-by-one
*)

(* To make things better, I'll probably use Result type returned from scanner later on. This is needed to properly replace "hadError" *)
(* TODO: implement a better error handling *)
let report line where message = print_endline ("[line " ^ line ^ "] Error" ^ where ^ ": " ^ message)

let error line where message = report line where message

(*
  This function is executed when we start olox directly from command-line
  TODO:
  1. Take filename (maybe rename to path?)
  2. Create a proper path from it
  3. Read the whole content (later could be improved to read line-by-line)
  4. Run the interpreter logic (could be a "mock" for now)
*)
let run_file path =
  let content = read_file path in
  print_endline (run content)
  (* if encountered an error during lexing -> exit 65 *)

let run_prompt () =
  while true do
    print_string "> ";
    run (read_line ()) |> ignore
  done

let main args =
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
