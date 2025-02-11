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

let to_string = function
| Left_paren -> "("
| Right_paren -> ")"
| Left_brace -> "{"
| Right_brace -> "}"
| Comma -> ","
| Dot -> "."
| Minus -> "-"
| Plus -> "+"
| Semicolon -> ";"
| Slash -> "/"
| Star -> "*"
| Bang -> "!"
| Bang_equal -> "!="
| Equal -> "="
| Equal_equal -> "=="
| Greater -> ">"
| Greater_equal -> ">="
| Less -> "<"
| Less_equal -> "<="
| Identifier i -> "identifier " ^ i
| String s -> "string " ^ s
| Number f -> "number " ^ Float.to_string f
| And -> "and"
| Class -> "class"
| Else -> "else"
| False -> "false"
| Fun -> "fun"
| For -> "for"
| If -> "if"
| Nil -> "nil"
| Or -> "or"
| Print -> "print"
| Return -> "return"
| Super -> "super"
| This -> "this"
| True -> "true"
| Var -> "var"
| While -> "while"
| Eof -> "eof"

let t_of_identifier = function
  | "and" -> And
  | "class" -> Class
  | "else" -> Else
  | "false" -> False
  | "fun" -> Fun
  | "for" -> For
  | "if" -> If
  | "nil" -> Nil
  | "or" -> Or
  | "print" -> Print
  | "return" -> Return
  | "super" -> Super
  | "this" -> This
  | "true" -> True
  | "var" -> Var
  | "while" -> While
  | _ as i -> Identifier i
