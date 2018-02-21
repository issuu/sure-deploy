(** Module to parse docker-compose.yml style template strings
  * 
  * This module links the lexer & parser parts together. *)

(* TODO: some tests *)

open Core

type context = string String.Map.t

let parse_with_error lexbuf =
  let buf = Buffer.create 17 in
  Parser.prog (Lexer.read_string buf) lexbuf

let parse template =
  template
  |> Lexing.from_string
  |> parse_with_error

let lookup_or_empty context k =
  String.Map.find context k
  |> Option.value ~default:""

let substitute context template =
  let open Or_error.Let_syntax in
  let%bind parts = parse template in
  let%bind resolved = List.fold_result parts ~init:[] ~f:(fun acc part ->
    match part with
    | String s -> Or_error.return @@ s :: acc
    | Variable n -> Or_error.return @@ lookup_or_empty context n :: acc
    | Unset_variable (n, subst) ->
        (match String.Map.find context n with
        | None -> Or_error.return @@ subst :: acc
        | Some v -> Or_error.return @@ v :: acc)
    | Unset_or_empty_variable (n, subst) ->
        (match String.Map.find context n with
        | None
        | Some "" -> Or_error.return @@ subst :: acc
        | Some v -> Or_error.return @@ v :: acc)
    | Unset_error_variable (n, msg) ->
        (match String.Map.find context n with
        | None -> Or_error.errorf "Error im template: '%s'" msg
        | Some v -> Or_error.return @@ v :: acc)
    | Unset_or_empty_error_variable (n, msg) ->
        (match String.Map.find context n with
        | None
        | Some "" -> Or_error.errorf "Error im template: '%s'" msg
        | Some v -> Or_error.return @@ v :: acc))
  in
  resolved |> List.rev |> String.concat ~sep:"" |> return


(* functions to visualize results of tokenizer and parser *)

let string_of_token = function
  | Parser.STRING s -> Printf.sprintf "STRING(\"%s\")" s
  | VARNAME (n, t, s) ->
      let s = match s with | None -> "[None]" | Some v -> v in
      let t = match t with | None -> "[None]" | Some v -> v in
      Printf.sprintf "VARNAME(%s,%s,%s)" n t s
  | EOF -> "EOF"

let string_of_value = function
  | Types.String s -> Printf.sprintf "String(\"%s\")" s
  | Variable n -> Printf.sprintf "Variable(%s)" n
  | Unset_variable (n, subst) -> Printf.sprintf "Unset_variable(%s, \"%s\")" n subst
  | Unset_or_empty_variable (n, subst) -> Printf.sprintf "Unset_or_empty_variable(%s, \"%s\")" n subst
  | Unset_error_variable (n, msg) -> Printf.sprintf "Unset_error_variable(%s, \"%s\")" n msg
  | Unset_or_empty_error_variable (n, msg) -> Printf.sprintf "Unset_or_empty_error_variable(%s, \"%s\")" n msg
