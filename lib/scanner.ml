(* WHERE AND HOW to store tokens *)
(* Parse tokens - how? *)

(* HOW to determine if I reached the end of the source file *)
type state = {
  source : string;
  mutable start : int;
  mutable current : int;
  line : int
}

let default source = { source; start = 0; current = 0; line = 1}

let is_at_end state = state.current >= String.length state.source

let scan_token () = prerr_endline "scan token"

(* Should return the list of tokens *)
(* HOW TO PARSE PROPERLY *)
let scan_tokens source =
  let s = default source in
  let tokens = Dynarray.create () in
  while not (is_at_end s) do
    s.start <- s.current;
    scan_token ();
  done;
  Dynarray.add_last tokens Token.Eof;
  tokens
