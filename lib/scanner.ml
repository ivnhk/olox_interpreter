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
let emit_token state t = { t; text = get_text state; line = state.line }

(* TODO: rework using `match` from the book *)
let emit_two_char_token c t1 t2 =
  match c with
  | '=' -> t1
  | _ -> t2


(* TODO: does not handle switching to the following line properly *)
(* TODO: handle EOF char *)
let rec scan_token state =
  let c = advance state in
  match c with
    | '(' -> Ok (emit_token state Token.Left_paren)
    | ')' -> Ok (emit_token state Token.Right_paren)
    | '{' -> Ok (emit_token state Token.Left_brace)
    | '}' -> Ok (emit_token state Token.Right_brace)
    | ',' -> Ok (emit_token state Token.Comma)
    | '.' -> Ok (emit_token state Token.Dot)
    | '-' -> Ok (emit_token state Token.Minus)
    | '+' -> Ok (emit_token state Token.Plus)
    | ';' -> Ok (emit_token state Token.Semicolon)
    | '*' -> Ok (emit_token state Token.Star)
    | '!' -> begin
        let nextCh = advance state in
        let t = emit_two_char_token nextCh Token.Bang_equal Token.Bang in
        Ok (emit_token state t)
      end
    | '=' -> begin
        let nextCh = advance state in
        let t = emit_two_char_token nextCh Token.Equal_equal Token.Equal in
        Ok (emit_token state t)
      end
    | '<' -> begin
        let nextCh = advance state in
        let t = emit_two_char_token nextCh Token.Less_equal Token.Less in
        Ok (emit_token state t)
      end
    | '>' -> begin
        let nextCh = advance state in
        let t = emit_two_char_token nextCh Token.Greater_equal Token.Greater in
        Ok (emit_token state t)
      end
    (* TODO: add handling for division and comments (//) *)
    | ' ' | '\r' | '\t' -> scan_token state
    | '\n' -> begin
      state.line <- state.line + 1;
      scan_token state
    end
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
    (* TODO: handle tokens and errors level above *)
  done;
  add_token tokens (emit_token state Token.Eof);

  Dynarray.iter (fun t -> Printf.printf "%s " (Token.to_string t.t)) tokens;

  if Dynarray.length errors > 0
  then Error errors
  else Ok tokens
