%token <string> STRING
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
  | DOLLAR; LEFT_BRACE; n = variable_name; RIGHT_BRACE
    { Variable n }
  | DOLLAR; LEFT_BRACE; n = variable_name; COLON ; DASH ; s = substitution_value ; RIGHT_BRACE
    { Empty_variable n s }
  | DOLLAR; LEFT_BRACE; n = variable_name; DASH ; s = substitution_value ; RIGHT_BRACE
    { Unset_variable n s }
  | DOLLAR; LEFT_BRACE; n = variable_name; COLON ; QUESTION ; s = substitution_value ; RIGHT_BRACE
    { Empty_error_variable n s }
  | DOLLAR; LEFT_BRACE; n = variable_name; QUESTION ; s = substitution_value ; RIGHT_BRACE
    { Unset_error_variable n s }
  | DOLLAR_ESCAPE
    { String "$$" }
  | s = STRING
    { String s }
  ;
