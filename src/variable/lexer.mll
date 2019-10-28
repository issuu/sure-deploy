{
open Parser

module Or_error = Core.Or_error
let return = Or_error.return
}

let varname = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read_subst_variable =
  parse
  | varname as v ":-" ([^ '}']+ as subst) '}' { return @@ VARNAME (v, Some (Unset_or_empty subst)) }
  | varname as v '-' ([^ '}']+ as subst) '}' { return @@ VARNAME (v, Some (Unset subst)) }
  | varname as v ":?" ([^ '}']+ as msg) '}' { return @@ VARNAME (v, Some (Unset_or_empty_error msg)) }
  | varname as v '?' ([^ '}']+ as msg) '}' { return @@ VARNAME (v, Some (Unset_error msg)) }
  | varname as v '}' { return @@ VARNAME (v, None) }
  | _ { Or_error.errorf "Unexpected char: %s" (Lexing.lexeme lexbuf) }
  | eof { return EOF }

and read_variable =
  parse
  | '{' { read_subst_variable lexbuf }
  | varname { return @@ VARNAME ((Lexing.lexeme lexbuf), None) }
  | eof { return @@ EOF }

and read_string buf =
  parse
  | "$$" { Buffer.add_char buf '$'; read_string buf lexbuf }
  | '$' { read_variable lexbuf }
  | [^ '$']+ { return @@ STRING (Lexing.lexeme lexbuf) }
  | _ { Or_error.errorf "Unknown string character: %s" (Lexing.lexeme lexbuf) }
  | eof { return @@ EOF }
