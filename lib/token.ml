type t =
  (* Single character token *)
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* One or two character tokens *)
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal
  (* LITERALS *)
  | Identifier of string
  | String of string
  | Number of float
  (* KEYWORDS *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  (* EOF *)
  | Eof
