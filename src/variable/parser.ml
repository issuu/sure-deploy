exception Error

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

let prog lex lexbuf =
  let rec lex_all' acc =
    let token = lex lexbuf in
    match token with
    | EOF -> acc
    | token -> lex_all' @@ (value token) :: acc
  in
  lex_all' []
  |> List.rev
  |> fun x -> Some x
