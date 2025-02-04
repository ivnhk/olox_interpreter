type state = {
  source : string;
  start : int;
  current : int;
  line : int
}
type token = { t: Token.t; text : string; line : int }

let default source = { source; start = 0; current = 0; line = 1}
let is_at_end state = state.current >= String.length state.source

let report line where message = print_endline ("[line " ^ line ^ "] Error " ^ where ^ ": " ^ message)
let error line where message = report (string_of_int line) (string_of_int where) message

let advance state = { state with current = state.current + 1}

let get_current_char state =
  if is_at_end state
  then '\x00'
  else state.source.[state.current]

let emit_token state =
  let current_state = { state with start = state.current } in
  let future_state = advance current_state in
  let open Token in
  match is_at_end current_state with
  | true -> (Ok Eof, future_state)
  | false -> begin
      match get_current_char current_state with
        | '(' -> (Ok Left_paren, future_state)
        | _ -> (Error (error current_state.line current_state.current "Unexpected character"), future_state)
    end

let scan_tokens source =
  let s = default source in
  let rec scan state tokens errors =
    match is_at_end state with
    | true -> begin
      match errors with
        | [] -> Ok (List.rev tokens)
        | _ -> Error (List.rev errors)
      end
    | false -> begin
        let result, state' = emit_token state in
        let scan' = scan state' in
        match result with
          | Ok t -> begin
            print_endline "OK t";
            print_string "[ ";
            List.iter (fun t -> Printf.printf "%s " (Token.to_string t)) tokens;
            print_string " ]";
            scan' (t :: tokens) errors
          end
          | Error e -> scan' tokens (e :: errors)
      end
  in
  scan s [] []
