{
open Lexing
open Parser

exception Syntax_error of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let varname = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read =
  parse
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '?' { QUESTION}
  | '-' { DASH }
  | "$$" { DOLLAR_ESCAPE }
  | '$' { DOLLAR }
  | _ { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
