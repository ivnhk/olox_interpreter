type state = {
  source : string;
  mutable start : int;
  mutable current : int;
  mutable line : int
}
type token = { t: Token.t; text : string; line : int }

let default source = { source; start = 0; current = 0; line = 1}
let is_at_end state = state.current >= String.length state.source

(* TODO: implement a better error handling *)
let report line where message = print_endline ("[line " ^ line ^ "] Error " ^ where ^ ": " ^ message)
let error line where message = report (string_of_int line) (string_of_int where) message

let advance state =
    let c = String.get state.source state.current in
    state.current <- state.current + 1;
    c

let add_token tokens token =
  Dynarray.add_last tokens token

let get_text state = String.sub state.source state.start (state.current - state.start)
(* TODO: do I really need to emit a more complex token? *)
let emit_token state t = Ok ({ t; text = get_text state; line = state.line })

let match_char state expected =
  if is_at_end state then
    false
  else
    let c = state.source.[state.current] in
    if c <> expected then
      false
    else begin
      state.current <- state.current + 1;
      true
    end

let peek state =
  if is_at_end state then
    '\255'
  else
    state.source.[state.current]

(* FIXME: Fix pos and length by moving through source code file correctly *)
let trim_quotes state =
  String.sub state.source (state.start + 2) (state.current - state.start - 3)

let parse_string state =
  while peek state <> '"' && not (is_at_end state) do
    if peek state = '\n' then
      state.line <- state.line + 1;
    ignore (advance state);
  done;

  if is_at_end state then
    error state.line state.current "Unterminated string.";
    ();

  (* The closing '"'. *)
  ignore (advance state);
  trim_quotes state


let rec scan_token state =
  let c = advance state in
  match c with
    | '(' -> emit_token state Token.Left_paren
    | ')' -> emit_token state Token.Right_paren
    | '{' -> emit_token state Token.Left_brace
    | '}' -> emit_token state Token.Right_brace
    | ',' -> emit_token state Token.Comma
    | '.' -> emit_token state Token.Dot
    | '-' -> emit_token state Token.Minus
    | '+' -> emit_token state Token.Plus
    | ';' -> emit_token state Token.Semicolon
    | '*' -> emit_token state Token.Star
    | '!' -> if match_char state '=' then emit_token state Token.Bang_equal else emit_token state Token.Bang
    | '=' -> if match_char state '=' then emit_token state Token.Equal_equal else emit_token state Token.Equal
    | '<' -> if match_char state '=' then emit_token state Token.Less_equal else emit_token state Token.Less
    | '>' -> if match_char state '=' then emit_token state Token.Greater_equal else emit_token state Token.Greater
    (* TODO: fix comment parsing. ATM, the in-line comment is parsed incorrectly *)
    | '/' ->
      if match_char state '/' then begin
        while peek state <> '\n' && not (is_at_end state) do
          ignore (advance state)
        done;
        scan_token state
      end else
        emit_token state Token.Slash
    | ' ' | '\r' | '\t' -> scan_token state
    | '\n' -> begin
        state.line <- state.line + 1;
        scan_token state
      end
    | '"' -> emit_token state (Token.String (parse_string state))
    | _ -> Error (error state.line state.current "Unexpected characther")

let scan_tokens source =
  let state = default source in
  let tokens = Dynarray.create () in
  let errors = Dynarray.create () in
  while not (is_at_end state) do
    state.start <- state.current;
    let result = scan_token state in
    match result with
    | Ok token -> add_token tokens token
    | Error e -> Dynarray.add_last errors e
  done;
  add_token tokens (emit_token state Token.Eof |> Result.get_ok);

  Dynarray.iter (fun t -> Printf.printf "%s " (Token.to_string t.t)) tokens;

  if Dynarray.length errors > 0
  then Error errors
  else Ok tokens
