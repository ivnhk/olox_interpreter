type tokens =
  (* Single character token *)
  | LEFT_PAREN | RIGHT_PAREN
  | LEFT_BRACE | RIGHT_BRACE
  | COMMA | DOT | SEMICOLON
  | MINUS | PLUS
  | SLASH | STAR
  (* One or two character tokens *)
  | BANG | BANG_EQUAL
  | EQUAL | EQUAL_EQUAL
  | GREATER | GREATER_EQUAL
  | LESS | LESS_EQUAL
  (* LITERALS *)
  | IDENTIFIER | STRING | NUMBER
  (* KEYWORDS *)
  | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR
  | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
  (* EO\F *)
  | EOF