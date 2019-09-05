open Core

type substitution =
  | Unset_or_empty of string
  | Unset of string
  | Unset_or_empty_error of string
  | Unset_error of string

type token =
  | STRING of string
  | VARNAME of string * substitution option
  | EOF

let return = Or_error.return

let value = function
  | EOF -> Or_error.errorf "EOF can't be converted to a value"
  | STRING s -> return @@ Types.String s
  | VARNAME (name, subst) -> (
    match subst with
    | None -> return @@ Types.Variable name
    | Some (Unset_or_empty s) -> return @@ Types.Unset_or_empty_variable (name, s)
    | Some (Unset s) -> return @@ Types.Unset_variable (name, s)
    | Some (Unset_or_empty_error msg) ->
        return @@ Types.Unset_or_empty_error_variable (name, msg)
    | Some (Unset_error msg) -> return @@ Types.Unset_error_variable (name, msg))

let prog : _ -> _ -> Types.value list Or_error.t =
 fun lex lexbuf ->
  let open Or_error.Let_syntax in
  let rec lex_all' acc =
    match%bind lex lexbuf with
    | EOF -> Or_error.return acc
    | token ->
        let%bind parsed = value token in
        lex_all' @@ (parsed :: acc)
  in
  let%map parsed = lex_all' [] in
  List.rev parsed
