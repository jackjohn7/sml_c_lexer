datatype token = EOF
               | IDENT of string
               | ILLEGAL of string
               | INT of int
               | FLOAT of real
               | CHAR of char
               | STRING of string
               (* Syntax *)
               | LPAREN
               | RPAREN
               | LBRACE
               | RBRACE
               | LBRACKET
               | RBRACKET
               | COMMA
               | SEMICOLON
               (* Operators *)
               | ASSIGN
               | PLUS
               | MINUS
               | MUL
               | DIV
               | EQ
               | NEQ
               | NOT
               | GT
               | LT
               | GTE
               | LTE
               (* Keywords *)
               | RETURN
               ;


fun tokenizeString str =
(* For speed, build in reverse and reverse list afterward *)
let
  fun tokenize [] acc = EOF::acc
    (* Ignore whitespace *)
    | tokenize (#" "::cs) acc = tokenize cs acc
    | tokenize (#"\t"::cs) acc = tokenize cs acc
    | tokenize (#"\n"::cs) acc = tokenize cs acc
    | tokenize (#"("::cs) acc = tokenize cs (LPAREN::acc)
    | tokenize (#")"::cs) acc = tokenize cs (RPAREN::acc)
    | tokenize (#"{"::cs) acc = tokenize cs (LBRACE::acc)
    | tokenize (#"}"::cs) acc = tokenize cs (RBRACE::acc)
    | tokenize (#"["::cs) acc = tokenize cs (LBRACKET::acc)
    | tokenize (#"]"::cs) acc = tokenize cs (RBRACKET::acc)
    | tokenize (#","::cs) acc = tokenize cs (COMMA::acc)
    | tokenize (#";"::cs) acc = tokenize cs (SEMICOLON::acc)
    | tokenize ((#"=")::(#"=")::cs) acc = tokenize cs (EQ::acc)
    | tokenize ((#"!")::(#"=")::cs) acc = tokenize cs (NEQ::acc)
    | tokenize ((#"!")::cs) acc = tokenize cs (NOT::acc)
    | tokenize ((#"=")::cs) acc = tokenize cs (ASSIGN::acc)
    | tokenize ((#"<")::(#"=")::cs) acc = tokenize cs (LTE::acc)
    | tokenize ((#">")::(#"=")::cs) acc = tokenize cs (GTE::acc)
    | tokenize ((#"<")::cs) acc = tokenize cs (LT::acc)
    | tokenize ((#">")::cs) acc = tokenize cs (GT::acc)
    | tokenize ((#"+")::cs) acc = tokenize cs (PLUS::acc)
    | tokenize ((#"-")::cs) acc = tokenize cs (MINUS::acc) (* no negatives? *)
    | tokenize ((#"*")::cs) acc = tokenize cs (MUL::acc)
    | tokenize ((#"/")::cs) acc = tokenize cs (DIV::acc)
    | tokenize ((#"r")::(#"e")::(#"t")::(#"u")::(#"r")::(#"n")::cs) acc =
        tokenize cs (RETURN::acc)
    | tokenize ((#"'")::c::(#"'")::cs) acc = tokenize cs ((CHAR c)::acc)
    | tokenize ((#"\"")::cs) acc = acc (* temp *)
    | tokenize (c::cs) acc = acc
  and 
(*fun read_number [] acc true = String.toInt (implode acc)*)
    read_string (#"\""::cs) acc str_acc = tokenize cs ((STRING (implode
      (rev str_acc)))::acc)
  | read_string (c::cs) acc str_acc = read_string cs acc (c::str_acc)
  | read_string [] acc str_acc = tokenize [] ((ILLEGAL (implode (rev str_acc)))::acc)
in rev (tokenize (explode str) []) end;

fun tok_to_string (EOF) = "EOF"
  | tok_to_string (IDENT x) = "IDENT("^x^")"
  | tok_to_string (ILLEGAL x) = "ILLEGAL("^x^")"
  | tok_to_string (INT x) = "INT("^(Int.toString x)^")"
  | tok_to_string (FLOAT x) = "FLOAT("^(Real.toString x)^")"
  | tok_to_string (CHAR x) = "CHAR("^(implode [x])^")"
  | tok_to_string (LPAREN) = "LPAREN"
  | tok_to_string (RPAREN) = "RPAREN"
  | tok_to_string (LBRACE) = "LBRACE"
  | tok_to_string (RBRACE) = "RBRACE"
  | tok_to_string (LBRACKET) = "LBRACKET"
  | tok_to_string (RBRACKET) = "RBRACKET"
  | tok_to_string (COMMA) = "COMMA"
  | tok_to_string (SEMICOLON) = "SEMICOLON"
  | tok_to_string (ASSIGN) = "ASSIGN"
  | tok_to_string (PLUS) = "PLUS"
  | tok_to_string (MINUS) = "MINUS"
  | tok_to_string (MUL) = "MUL"
  | tok_to_string (DIV) = "DIV"
  | tok_to_string (EQ) = "EQ"
  | tok_to_string (NEQ) = "NEQ"
  | tok_to_string (NOT) = "NOT"
  | tok_to_string (GT) = "GT"
  | tok_to_string (LT) = "LT"
  | tok_to_string (GTE) = "GTE"
  | tok_to_string (LTE) = "LTE"
  | tok_to_string (RETURN) = "RETURN"
  ;

fun pp_list [] = ()
  | pp_list (t::ts) = (
    print ((tok_to_string t)^"\n");
    pp_list ts
  );

