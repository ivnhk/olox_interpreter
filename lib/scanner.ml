(* WHERE AND HOW to store tokens *)
(* Parse tokens - how? *)

(* HOW to determine if I reached the end of the source file *)
type state = {
  source : string;
  mutable start : int;
  mutable current : int;
  line : int
}
type token = { t: Token.t; text : string; line : int }

let default source = { source; start = 0; current = 0; line = 1}
let is_at_end state = state.current >= String.length state.source

let add_token state _ _ =
  print_endline ("source:" ^ state.source ^ "start: " ^ string_of_int state.start ^ "current: " ^ string_of_int state.current)
  (* let text = String.sub state.source state.start state.current in
  print_endline text *)
  (* let token = { t; text; line : state.line } in
  Dynarray.add_last tokens token *)

let emit_token tokens t = add_token tokens t

let advance state =
  print_endline ("source:" ^ state.source ^ "start: " ^ string_of_int state.start ^ "current: " ^ string_of_int state.current);

  if not (is_at_end state)
  then begin
    state.current <- state.current + 1;
    state.source.[state.current]
  end
  else ' '


let add_token tokens token =
  Dynarray.add_last tokens token

(* How to make "default" case redundant? *)
let scan_token tokens state =
  let c = advance state in
  print_char c;
  match c with
    | '(' -> emit_token state tokens Token.Right_paren
    (* | ')' -> add_token tokens Token.Right_paren
    | '{' -> add_token tokens Token.Left_brace
    | '}' -> add_token tokens Token.Right_brace
    | ',' -> add_token tokens Token.Comma
    | '.' -> add_token tokens Token.Dot
    | '-' -> add_token tokens Token.Minus
    | '+' -> add_token tokens Token.Plus
    | ';' -> add_token tokens Token.Semicolon
    | '*' -> add_token tokens Token.Star *)
    | _ -> print_endline "lol"

(* Should return the list of tokens *)
(* HOW TO PARSE PROPERLY *)
let scan_tokens source =
  let s = default source in
  let tokens = Dynarray.create () in
  while not (is_at_end s) do
    s.start <- s.current;
    scan_token tokens s;
  done;
  Dynarray.add_last tokens Token.Eof;
  tokens
