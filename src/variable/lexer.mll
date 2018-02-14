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

rule read_variable =
  parse
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '?' { QUESTION }
  | '-' { DASH }
  (* | "$$" { DOLLAR_ESCAPE } *)
  (* | '$' { DOLLAR } *)
  | varname { VARNAME (Lexing.lexeme lexbuf) }
  | _ { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_string buf =
  parse
  | "$$" { Buffer.add_char buf '$'; read_string buf lexbuf }
  | '$' { read_variable lexbuf }
  | [^ '$']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Syntax_error ("Unknown string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_substitution buf =
  parse
  | '}' { STRING (Buffer.contents buf) }
  | _ { raise (Syntax_error ("Unknown character in substitution: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Syntax_error ("Substitution not terminated")) }
