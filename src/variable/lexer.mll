{
open Parser

module Or_error = Core.Or_error
let return = Or_error.return
}

let varname = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let split = '-'|":-"|'?'|":?"

rule read_subst_variable buf =
  parse
  | varname as v (split as divider) ([^ '}']+ as subst) '}' { return @@ VARNAME (v, Some divider, Some subst) }
  | varname as v '}' { return @@ VARNAME (v, None, None) }
  | _ { Or_error.errorf "Unexpected char: %s" (Lexing.lexeme lexbuf) }
  | eof { return EOF }

and read_variable =
  parse
  | '{' { read_subst_variable (Buffer.create 17) lexbuf }
  | varname { return @@ VARNAME ((Lexing.lexeme lexbuf), None, None) }
  | eof { return @@ EOF }

and read_string buf =
  parse
  | "$$" { Buffer.add_char buf '$'; read_string buf lexbuf }
  | '$' { read_variable lexbuf }
  | [^ '$']+ { return @@ STRING (Lexing.lexeme lexbuf) }
  | _ { Or_error.errorf "Unknown string character: %s" (Lexing.lexeme lexbuf) }
  | eof { return @@ EOF }
