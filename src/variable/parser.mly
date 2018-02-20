%token <string> STRING
%token <string * string option * string option> VARNAME
%token EOF

%start <Types.value list option> prog
%%

prog:
  | EOF
    { None }
  | v = value_sequence; EOF
    { Some v }
  ;

value_sequence:
  | seq = rev_value_sequence
    { List.rev seq }
  ;

rev_value_sequence:
  | v = value
    { [v] }
  | seq = rev_value_sequence; v = value
    { v :: seq }
  ;

value:
  | v = VARNAME;
    {
    let (name, mode, subst) = v in
    match mode, subst with
    | None, _ -> Types.Variable name
    | Some ":-", Some s -> Unset_or_empty_variable (name, s)
    | Some ":-", None -> Unset_or_empty_variable (name, "")
    | Some "-", Some s -> Unset_variable (name, s)
    | Some "-", None -> Unset_variable (name, "")
    | Some ":?", Some s -> Unset_or_empty_error_variable (name, s)
    | Some ":?", None -> Unset_or_empty_error_variable (name, "")
    | Some "?", Some s -> Unset_error_variable (name, s)
    | Some "?", None -> Unset_error_variable (name, "")
    | _, _ -> failwith "Parsing failed due to invalid token"
    }
  | s = STRING
    { Types.String s }
  ;
