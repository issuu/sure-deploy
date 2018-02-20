(** Module to parse docker-compose.yml style template strings
  * 
  * This module links the lexer & parser parts together. *)

(* TODO: some tests *)

open Core

type context = string String.Map.t

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let buf = Buffer.create 17 in
  match Parser.prog (Lexer.read_string buf) lexbuf with
  | success -> success
  | exception Lexer.Syntax_error msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | exception Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse template =
  template
  |> Lexing.from_string
  |> parse_with_error

let lookup_or_empty context k =
  String.Map.find context k
  |> Option.value ~default:""

let substitute context template =
  match parse template with
  | None -> Or_error.errorf "Parsing template failed"
  | Some parts ->
    let open Or_error.Let_syntax in
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

let lex_all s =
  let lexbuf = Lexing.from_string s in
  let buffer = Buffer.create 17 in
  let rec lex_all' acc =
    let token = Lexer.read_string buffer lexbuf in
    match token with
    | Parser.EOF as token -> string_of_token token :: acc
    | token -> lex_all' @@ string_of_token token :: acc
  in
  lex_all' []
  |> List.rev

let string_of_value = function
  | Types.String s -> Printf.sprintf "String(\"%s\")" s
  | Variable n -> Printf.sprintf "Variable(%s)" n
  | Unset_variable (n, subst) -> Printf.sprintf "Unset_variable(%s, \"%s\")" n subst
  | Unset_or_empty_variable (n, subst) -> Printf.sprintf "Unset_or_empty_variable(%s, \"%s\")" n subst
  | Unset_error_variable (n, msg) -> Printf.sprintf "Unset_error_variable(%s, \"%s\")" n msg
  | Unset_or_empty_error_variable (n, msg) -> Printf.sprintf "Unset_or_empty_error_variable(%s, \"%s\")" n msg

let parse_all s =
  match parse s with
  | None -> failwith "failed to parse"
  | Some parsed -> parsed |> List.map ~f:string_of_value
