%token <string> STRING
%token <string> SHORT_VARNAME
%token <string> VARNAME
%token DOLLAR
%token DOLLAR_ESCAPE
%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token QUESTION
%token DASH
%token EOF

%start <Variable.value option> prog
%%

prog:
  | EOF { None }
  | v = value { Some v }
  ;

value:
  | DOLLAR; LEFT_BRACE; n = VARNAME; RIGHT_BRACE
    { Variable n }
  | DOLLAR; LEFT_BRACE; n = VARNAME; COLON ; DASH ; s = STRING ; RIGHT_BRACE
    { Empty_variable (n, s) }
  | DOLLAR; LEFT_BRACE; n = VARNAME; DASH ; s = STRING ; RIGHT_BRACE
    { Unset_variable (n, s) }
  | DOLLAR; LEFT_BRACE; n = VARNAME; COLON ; QUESTION ; s = STRING ; RIGHT_BRACE
    { Empty_error_variable (n, s) }
  | DOLLAR; LEFT_BRACE; n = VARNAME; QUESTION ; s = STRING ; RIGHT_BRACE
    { Unset_error_variable (n, s) }
  | DOLLAR; n = SHORT_VARNAME
    { Variable n }
  | DOLLAR_ESCAPE
    { String "$$" }
  | s = STRING
    { String s }
  ;
