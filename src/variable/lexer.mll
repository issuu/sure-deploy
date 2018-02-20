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
let split = '-'|":-"|'?'|":?"

rule read_subst_variable buf =
  parse
  | varname as v (split as divider) ([^ '}']+ as subst) '}' { VARNAME (v, Some divider, Some subst) }
  | varname as v '}' { VARNAME (v, None, None) }
  | _ { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_variable =
  parse
  | '{' { read_subst_variable (Buffer.create 17) lexbuf }
  | varname { VARNAME ((Lexing.lexeme lexbuf), None, None) }
  | eof { EOF }

and read_string buf =
  parse
  | "$$" { Buffer.add_char buf '$'; read_string buf lexbuf }
  | '$' { read_variable lexbuf }
  | [^ '$']+ { STRING (Lexing.lexeme lexbuf) }
  | _ { raise (Syntax_error ("Unknown string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
