(* WHERE AND HOW to store tokens *)
(* Parse tokens - how? *)
type token = { t: Token.t; line: int }
type state = { source : string; start : int; current : int; line : int }

let default source = { source; start = 0; current = 0; line = 1 }
let is_at_end state = state.current >= String.length state.source

(* HOW to determine if I reached the end of the source file *)

(* Should return the list of tokens *)
(* HOW TO PARSE PROPERLY *)
let scan_tokens source =
  let s = default source in
  (* let rec scan state tokens errors *)
  let scan state _ _ =
    match is_at_end state with
    | true -> "END"
    | false -> "NOT AN END"
  in
  scan s [] []
