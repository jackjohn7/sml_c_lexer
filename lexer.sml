datatype token =
   EOF
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
 | COLON
 (* Operators *)
 | ASSIGN
 | PLUS
 | ASSIGN_PLUS
 | MINUS
 | ASSIGN_MINUS
 | MOD
 | ASSIGN_MOD
 | INC
 | DEC
 | STAR
 | ASSIGN_MUL
 | DIV
 | ASSIGN_DIV
 | EQ
 | NEQ
 | NOT
 | GT
 | LT
 | GTE
 | LTE
 | AND
 | OR
 | POUND (* for macros *)
 | BWAND
 | ASSIGN_BWAND
 | BWOR
 | ASSIGN_BWOR
 | BWXOR
 | ASSIGN_BWXOR
 | BWOCOMP
 | BWLSHIFT
 | ASSIGN_BWLSHIFT
 | BWRSHIFT
 | ASSIGN_BWRSHIFT
 | SIZEOF
 (* Keywords *)
 | RETURN
 | TYPEDEF
 | UNION
 | UNSIGNED
 (* Primitive types *)
 | INTTYPE
 | FLOATTYPE
 | CHARTYPE
 | STRUCTTYPE;


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
    | tokenize (#":"::cs) acc = tokenize cs (COLON::acc)
    | tokenize (#"="::(#"=")::cs) acc = tokenize cs (EQ::acc)
    | tokenize (#"!"::(#"=")::cs) acc = tokenize cs (NEQ::acc)
    | tokenize (#"!"::cs) acc = tokenize cs (NOT::acc)
    | tokenize (#"="::cs) acc = tokenize cs (ASSIGN::acc)
    | tokenize (#"<"::(#"<")::(#"=")::cs) acc = tokenize cs (ASSIGN_BWLSHIFT::acc)
    | tokenize (#"<"::(#"<")::cs) acc = tokenize cs (BWLSHIFT::acc)
    | tokenize (#"<"::(#"=")::cs) acc = tokenize cs (LTE::acc)
    | tokenize (#"<"::cs) acc = tokenize cs (LT::acc)
    | tokenize (#">"::(#">")::(#"=")::cs) acc = tokenize cs (ASSIGN_BWRSHIFT::acc)
    | tokenize (#">"::(#">")::cs) acc = tokenize cs (BWRSHIFT::acc)
    | tokenize (#">"::(#"=")::cs) acc = tokenize cs (GTE::acc)
    | tokenize (#">"::cs) acc = tokenize cs (GT::acc)
    | tokenize (#"^"::(#"=")::cs) acc = tokenize cs (ASSIGN_BWXOR::acc)
    | tokenize (#"^"::cs) acc = tokenize cs (BWXOR::acc)
    | tokenize (#"+"::(#"=")::cs) acc = tokenize cs (ASSIGN_PLUS::acc)
    | tokenize (#"+"::(#"+")::cs) acc = tokenize cs (INC::acc)
    | tokenize (#"+"::cs) acc = tokenize cs (PLUS::acc)
    | tokenize (#"-"::(#"=")::cs) acc = tokenize cs (ASSIGN_MINUS::acc)
    | tokenize (#"-"::(#"-")::cs) acc = tokenize cs (DEC::acc)
    | tokenize (#"-"::cs) acc = tokenize cs (MINUS::acc) (* no negatives? *)
    (* Could later match on last acc token (Ident) and specify ptr star *)
    | tokenize (#"*"::(#"=")::cs) acc = tokenize cs (ASSIGN_MUL::acc)
    | tokenize (#"*"::cs) acc = tokenize cs (STAR::acc)
    | tokenize (#"/"::(#"=")::cs) acc = tokenize cs (ASSIGN_DIV::acc)
    | tokenize (#"/"::cs) acc = tokenize cs (DIV::acc)
    | tokenize (#"%"::(#"=")::cs) acc = tokenize cs (ASSIGN_MOD::acc)
    | tokenize (#"%"::cs) acc = tokenize cs (MOD::acc)
    | tokenize (#"~"::cs) acc = tokenize cs (BWOCOMP::acc)
    | tokenize (#"#"::cs) acc = tokenize cs (POUND::acc)
    | tokenize (#"&"::(#"&")::cs) acc = tokenize cs (AND::acc)
    | tokenize (#"&"::(#"=")::cs) acc = tokenize cs (ASSIGN_BWAND::acc)
    | tokenize (#"&"::cs) acc = tokenize cs (BWAND::acc)
    | tokenize (#"|"::(#"|")::cs) acc = tokenize cs (OR::acc)
    | tokenize (#"|"::(#"=")::cs) acc = tokenize cs (ASSIGN_BWOR::acc)
    | tokenize (#"|"::cs) acc = tokenize cs (BWOR::acc)
    | tokenize ((#"s")::(#"i")::(#"z")::(#"e")::(#"o")::(#"f")::cs) acc =
        tokenize cs (SIZEOF::acc)
    (* Keywords *)
    | tokenize ((#"r")::(#"e")::(#"t")::(#"u")::(#"r")::(#"n")::cs) acc =
        tokenize cs (RETURN::acc)
    | tokenize ((#"u")::(#"n")::(#"i")::(#"o")::(#"n")::cs) acc =
        tokenize cs (UNION::acc)
    | tokenize ((#"t")::(#"y")::(#"p")::(#"e")::(#"d")::(#"e")::(#"f")::cs) acc =
        tokenize cs (TYPEDEF::acc)
    | tokenize ((#"u")::(#"n")::(#"s")::(#"i")::(#"g")::(#"n")::(#"e")::(#"d")::cs) acc =
        tokenize cs (TYPEDEF::acc)
    | tokenize ((#"i")::(#"n")::(#"t")::cs) acc =
        tokenize cs (INTTYPE::acc)
    | tokenize ((#"f")::(#"l")::(#"o")::(#"a")::(#"t")::cs) acc =
        tokenize cs (FLOATTYPE::acc)
    | tokenize ((#"c")::(#"h")::(#"a")::(#"r")::cs) acc =
        tokenize cs (CHARTYPE::acc)
    | tokenize ((#"s")::(#"t")::(#"r")::(#"u")::(#"c")::(#"t")::cs) acc =
        tokenize cs (STRUCTTYPE::acc)
    (* literals and idents *)
    | tokenize ((#"'")::c::(#"'")::cs) acc = tokenize cs ((CHAR c)::acc)
    | tokenize ((#"\"")::cs) acc = read_string cs acc []
    | tokenize (L as (c::cs)) acc =
      if ((ord c) >= 65 andalso (ord c) <= 90) orelse
      ((ord c) >= 97 andalso (ord c) <= 122) then
        read_ident cs acc [c]
      else tokenize cs acc

  and 
(*fun read_number [] acc true = String.toInt (implode acc)*)
    read_string (#"\""::cs) acc str_acc = tokenize cs ((STRING (implode
      (rev str_acc)))::acc)
  | read_string (c::cs) acc str_acc = read_string cs acc (c::str_acc)
  | read_string [] acc str_acc = tokenize [] ((ILLEGAL (implode (rev str_acc)))::acc)
  and
    read_ident [] acc str_acc =  tokenize [] ((ILLEGAL (implode (rev str_acc)))::acc)
  | read_ident (L as (c::cs)) acc (str_acc: char list) =
      case c of
           #"@" => tokenize L (ILLEGAL (implode (rev str_acc))::acc)
         | #"#" => tokenize L (ILLEGAL (implode (rev str_acc))::acc)
         | #"$" => tokenize L (ILLEGAL (implode (rev str_acc))::acc)
         | #"!" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"%" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"^" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"&" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"*" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"(" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #")" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"{" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"}" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"[" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"]" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"/" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"?" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"+" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"-" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"=" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #" " => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"\t" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | #"\n" => tokenize L (IDENT (implode (rev str_acc))::acc)
         | c => read_ident cs acc (c::str_acc)
  (*and

  read_number (L as (c::cs)) acc str_acc =
    case c of
        #"." => read_float *)



in rev (tokenize (explode str) []) end;
fun tok_to_string (EOF) = "EOF"
  | tok_to_string (IDENT x) = "IDENT(\""^x^"\")"
  | tok_to_string (ILLEGAL x) = "ILLEGAL(\""^x^"\")"
  | tok_to_string (INT x) = "INT("^(Int.toString x)^")"
  | tok_to_string (FLOAT x) = "FLOAT("^(Real.toString x)^")"
  | tok_to_string (CHAR x) = "CHAR("^(implode [x])^")"
  | tok_to_string (STRING x) = "STRING(\""^x^"\")"
  | tok_to_string (LPAREN) = "LPAREN"
  | tok_to_string (RPAREN) = "RPAREN"
  | tok_to_string (LBRACE) = "LBRACE"
  | tok_to_string (RBRACE) = "RBRACE"
  | tok_to_string (LBRACKET) = "LBRACKET"
  | tok_to_string (RBRACKET) = "RBRACKET"
  | tok_to_string (COMMA) = "COMMA"
  | tok_to_string (SEMICOLON) = "SEMICOLON"
  | tok_to_string (COLON) = "COLON"
  | tok_to_string (ASSIGN) = "ASSIGN"
  | tok_to_string (PLUS) = "PLUS"
  | tok_to_string (ASSIGN_PLUS) = "ASSIGN_PLUS"
  | tok_to_string (MINUS) = "MINUS"
  | tok_to_string (ASSIGN_MINUS) = "ASSIGN_MINUS"
  | tok_to_string (STAR) = "STAR"
  | tok_to_string (ASSIGN_MUL) = "ASSIGN_MUL"
  | tok_to_string (DIV) = "DIV"
  | tok_to_string (ASSIGN_DIV) = "ASSIGN_DIV"
  | tok_to_string (EQ) = "EQ"
  | tok_to_string (NEQ) = "NEQ"
  | tok_to_string (NOT) = "NOT"
  | tok_to_string (GT) = "GT"
  | tok_to_string (LT) = "LT"
  | tok_to_string (GTE) = "GTE"
  | tok_to_string (LTE) = "LTE"
  | tok_to_string (AND) = "AND"
  | tok_to_string (BWAND) = "BWAND"
  | tok_to_string (ASSIGN_BWAND) = "ASSIGN_BWAND"
  | tok_to_string (OR) = "OR"
  | tok_to_string (BWOR) = "BWOR"
  | tok_to_string (ASSIGN_BWOR) = "ASSIGN_BWOR"
  | tok_to_string (BWXOR) = "BWXOR"
  | tok_to_string (ASSIGN_BWXOR) = "ASSIGN_BWXOR"
  | tok_to_string (MOD) = "MOD"
  | tok_to_string (ASSIGN_MOD) = "ASSIGN_MOD"
  | tok_to_string (BWOCOMP) = "BWOCOMP"
  | tok_to_string (BWLSHIFT) = "BWLSHIFT"
  | tok_to_string (ASSIGN_BWLSHIFT) = "ASSIGN_BWLSHIFT"
  | tok_to_string (BWRSHIFT) = "BWRSHIFT"
  | tok_to_string (ASSIGN_BWRSHIFT) = "ASSIGN_BWRSHIFT"
  | tok_to_string (INC) = "INC"
  | tok_to_string (DEC) = "DEC"
  | tok_to_string (POUND) = "POUND"
  | tok_to_string (SIZEOF) = "SIZEOF"
  | tok_to_string (RETURN) = "RETURN"
  | tok_to_string (TYPEDEF) = "TYPEDEF"
  | tok_to_string (UNION) = "UNION"
  | tok_to_string (UNSIGNED) = "UNSIGNED"
  | tok_to_string (INTTYPE) = "INTTYPE"
  | tok_to_string (FLOATTYPE) = "FLOATTYPE"
  | tok_to_string (CHARTYPE) = "CHARTYPE"
  | tok_to_string (STRUCTTYPE) = "STRUCTTYPE"
  ;

fun pp_list [] = ()
  | pp_list (t::ts) = (
    print ((tok_to_string t)^"\n");
    pp_list ts
  );

