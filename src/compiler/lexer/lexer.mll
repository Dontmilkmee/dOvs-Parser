(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)
{
  open Tigerparser.Parser 

  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str);;
}
let number = ['0'-'9']

(* standard regex *)
let whitespacePattern = [' ' '\t']
let digitsPattern = number+
let IDsPattern = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

(* string regex *)
let octalPattern = number number number
let caretPattern = ['a'-'z' 'A'-'Z' '@' '_' '^' '[' ']' '\\']
let tildeCaretPattern = ['a'-'z' 'A'-'Z' '@' '_' '^' '[' ']' '\\' '?']
let tildePattern = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '!' '"' '#' '$' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '`' '{' '|' '}' '~' '@' '_' '^' '[' ']' '\\']
let validCharsPattern = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '!' '#' '$' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '`' '{' '|' '}' '~' '@' '_' '^' '[' ']' '\t' '\r']+

(* reg errors *)
let wrongIDsPattern = number['A'-'Z' 'a'-'z' '0'-'9' '_']*

(* an entrypoint with a few starting regexps *)
rule token = parse
|  whitespacePattern        { token lexbuf } (* skip blanks *)
| '\n'                      { Lexing.new_line lexbuf; token lexbuf  } (* handle newline *)
| "/*"                      { commentParse 0 lexbuf } (* handle comment *)
| eof                       { EOF }
(* seperator tokens *)
| ','                       { COMMA }
| ';'                       { SEMICOLON }
| ":="                      { ASSIGN }
| ":"                       { COLON }
(* name tokens *)
| "array"                   { ARRAY }
| "if"                      { IF }
| "then"                    { THEN }
| "else"                    { ELSE }
| "let"                     { LET }
| "in"                      { IN }
| "end"                     { END }
| "of"                      { OF }
| "break"                   { BREAK }
| "nil"                     { NIL }
| "function"                { FUNCTION }
| "var"                     { VAR }
| "type"                    { TYPE }
| "for"                     { FOR }
| "to"                      { TO }
| "do"                      { DO }
| "while"                   { WHILE }
(* digits *)
| digitsPattern as i        { if String.length i > 10 then error lexbuf "Max int reached!" else INT (int_of_string i) }
(* handle strings *)
| '"'                       { stringParse (Buffer.create 1) (lexbuf.Lexing.lex_start_p) lexbuf }
(* binary operator tokens*)
| "<>"                      { NEQ }
| "<="                      { LE }
| "<"                       { LT }
| ">="                      { GE }
| ">"                       { GT }
| "="                       { EQ }
| "&"                       { AND }
| "|"                       { OR }
| "+"                       { PLUS }
| "-"                       { MINUS }
| "*"                       { TIMES }
| "/"                       { DIVIDE }
(* string operator tokens*)
| "."                       { DOT }
| "^"                       { CARET }
(* bracket tokens *)
| "("                       { LPAREN }
| ")"                       { RPAREN }
| "["                       { LBRACK }
| "]"                       { RBRACK }
| "{"                       { LBRACE }
| "}"                       { RBRACE }
(* handle ids *)
| IDsPattern as id          { ID(id) }
(* catch error *)
| wrongIDsPattern           { error lexbuf ("Wrong ids") }
| _ as t                    { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }

(* handle comments *)
and commentParse commentLevel = parse
| '\n' { Lexing.new_line lexbuf; commentParse commentLevel lexbuf }
| "/*" { commentParse (commentLevel+1 ) lexbuf } (* recursively tail-call oneself *)
| "*/" { (if commentLevel = 0 then token else commentParse (commentLevel-1) ) lexbuf }
| _    { commentParse commentLevel lexbuf } (* contiune *)
| eof  { error lexbuf ("Comment is not terminated") }

(* handle strings *)
and stringParse buf pos = parse
| '"'                                     { lexbuf.Lexing.lex_start_p <- pos; STRING(Buffer.contents buf) }
| '\\' (octalPattern as s)                { if int_of_string (s) > 255 then error lexbuf ("Unicode is over 255") else (Buffer.add_char buf (Char.chr (int_of_string (s))); stringParse buf pos lexbuf) }
| '\\' '\n'                               { Lexing.new_line lexbuf; handleStringNewline lexbuf; stringParse buf pos lexbuf }
| '\n'                                    { error lexbuf ("Invalid newline in string")}
| '\\' 'n'                                { Buffer.add_char buf '\n'; stringParse buf pos lexbuf }
| '\\' 'r'                                { Buffer.add_char buf '\r'; stringParse buf pos lexbuf }
| '\\' 't'                                { Buffer.add_char buf '\t'; stringParse buf pos lexbuf }
| '\\' '\\'                               { Buffer.add_char buf '\\'; stringParse buf pos lexbuf }
| '\\' '"'                                { Buffer.add_char buf '\"'; stringParse buf pos lexbuf }
| '\\' '^' '?'                            { Buffer.add_char buf (Char.chr 127); stringParse buf pos lexbuf }
| '\\' '~' '^' (tildeCaretPattern as s)   { Buffer.add_char buf (Char.chr ((Char.code (Char.uppercase_ascii s)) + 64)); stringParse buf pos lexbuf }
| '\\' '~' (tildePattern as s)            { Buffer.add_char buf (Char.chr ((Char.code s) + 128)); stringParse buf pos lexbuf }
| '\\' '^' (caretPattern as s)            { Buffer.add_char buf (Char.chr ((Char.code (Char.uppercase_ascii s)) - 64)); stringParse buf pos lexbuf }
| validCharsPattern                       { Buffer.add_string buf (Lexing.lexeme lexbuf); stringParse buf pos lexbuf }
| _ as s                                  { error lexbuf ("Invalid character in string '" ^ (String.make 1 s) ^ "'") }
| eof                                     { error lexbuf ("String is not terminated") }

(* handle multi-line strings (helper parser) *)
and handleStringNewline = parse
| '\n'                  { Lexing.new_line lexbuf; handleStringNewline lexbuf }
| whitespacePattern+    { handleStringNewline lexbuf }
| '\\'                  { }
| _                     { error lexbuf ("Error in multi-line string. Invalid character between the lines") }