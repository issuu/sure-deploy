open Core

type token =
  | STRING of string
  | VARNAME of string * string option * string option
  | EOF

let return = Or_error.return

let value = function
  | EOF -> Or_error.errorf "EOF can't be converted to a value"
  | STRING s -> return @@ Types.String s
  | VARNAME (name, mode, subst) ->
    match mode, subst with
    | None, _ -> return @@ Types.Variable name
    | Some ":-", Some s -> return @@ Types.Unset_or_empty_variable (name, s)
    | Some ":-", None -> return @@ Types.Unset_or_empty_variable (name, "")
    | Some "-", Some s -> return @@ Types.Unset_variable (name, s)
    | Some "-", None -> return @@ Types.Unset_variable (name, "")
    | Some ":?", Some s -> return @@ Types.Unset_or_empty_error_variable (name, s)
    | Some ":?", None -> return @@ Types.Unset_or_empty_error_variable (name, "")
    | Some "?", Some s -> return @@ Types.Unset_error_variable (name, s)
    | Some "?", None -> return @@ Types.Unset_error_variable (name, "")
    | _, _ -> Or_error.errorf "Parsing failed due to invalid token"

let prog : _ -> _ -> Types.value list Or_error.t = fun lex lexbuf ->
  let open Or_error.Let_syntax in
  let rec lex_all' acc =
    match%bind lex lexbuf with
    | EOF -> Or_error.return acc
    | token ->
      let%bind parsed = value token in
      lex_all' @@ parsed :: acc
  in
  let%map parsed = lex_all' [] in
  List.rev parsed
