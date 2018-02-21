open Core

type token =
  | STRING of string
  | VARNAME of string * string option * string option
  | EOF

let value = function
  | STRING s -> Types.String s
  | VARNAME (name, mode, subst) -> (
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
    | _, _ -> failwith "Parsing failed due to invalid token")
  | EOF -> failwith "TODO: handle EOF"

let prog : _ -> _ -> Types.value list Or_error.t = fun lex lexbuf ->
  let rec lex_all' acc =
    match lex lexbuf with
    | Error _ as e -> e
    | Ok EOF -> Or_error.return acc
    | Ok token -> lex_all' @@ (value token) :: acc
  in
  let parsed = lex_all' [] in
  Or_error.map parsed ~f:List.rev
