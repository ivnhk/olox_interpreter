(* WHERE AND HOW to store tokens *)
(* Parse tokens - how? *)

(* HOW to determine if I reached the end of the source file *)
type state = {
  source : string;
  mutable start : int;
  mutable current : int;
  mutable line : int
}
type token = { t: Token.t; text : string; line : int }

let default source = { source; start = 0; current = 0; line = 1}
let is_at_end state = state.current >= String.length state.source

(* To make things better, I'll probably use Result type returned from scanner later on. This is needed to properly replace "hadError" *)
(* TODO: implement a better error handling *)
let report line where message = print_endline ("[line " ^ line ^ "] Error" ^ where ^ ": " ^ message)

let error line where message = report (string_of_int line) (string_of_int where) message

let add_token state tokens t =
  let text = String.sub state.source state.start (state.current - state.start) in
  let token = { t; text; line = state.line } in
  Dynarray.add_last tokens token

let emit_token tokens t = add_token tokens t

let advance state =
  if not (is_at_end state)
  then begin
    let c = String.get state.source state.current in
    state.current <- state.current + 1;
    c
  end
  else ' '


let add_token tokens token =
  Dynarray.add_last tokens token

(* How to make "default" case redundant? *)
let scan_token state tokens =
  let c = advance state in
  match c with
    | '(' -> emit_token state tokens Token.Left_paren
    | ')' -> emit_token state tokens Token.Right_paren
    | '{' -> emit_token state tokens Token.Left_brace
    | '}' -> emit_token state tokens Token.Right_brace
    | ',' -> emit_token state tokens Token.Comma
    | '.' -> emit_token state tokens Token.Dot
    | '-' -> emit_token state tokens Token.Minus
    | '+' -> emit_token state tokens Token.Plus
    | ';' -> emit_token state tokens Token.Semicolon
    | '*' -> emit_token state tokens Token.Star
    | '\n' -> begin
      state.line <- state.line + 1
    end
    | _ -> error state.line state.current "Unexpected characther"

(* Should return the list of tokens *)
(* HOW TO PARSE PROPERLY *)
let scan_tokens source =
  let s = default source in
  let tokens = Dynarray.create () in
  while not (is_at_end s) do
    s.start <- s.current;
    scan_token s tokens;
  done;
  (* Dynarray.add_last tokens Token.Eof; *)
  List.iter (fun t -> Printf.printf "%s " (Token.to_string t.t)) (Dynarray.to_list tokens);
  tokens
