open Core

module type Identifier = sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  val pp : unit -> t -> string
end

module Stack : Identifier = struct
  type t = string

  let of_string = Fn.id

  let to_string = Fn.id

  let pp () = to_string
end

module type SwarmIdentifier = sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  val pp : unit -> t -> string

  val basename : Stack.t -> t -> string

  val equal : t -> t -> bool

  val equal_basename : Stack.t -> t -> t -> bool
end

module Service : SwarmIdentifier = struct
  type t = string

  let of_string = Fn.id

  let to_string = Fn.id

  let pp () = to_string

  let equal = String.equal

  let basename stack service =
    let prefix = Printf.sprintf "%a_" Stack.pp stack in
    match String.chop_prefix ~prefix service with
    | Some suffix -> suffix
    | None -> service

  let equal_basename stack a b =
    let a = basename stack a in
    let b = basename stack b in
    String.equal a b
end

module Image : sig
  type t

  val pp : unit -> t -> string

  val of_string : string -> t

  val to_string : t -> string

  val equal_nametag : t -> t -> bool

  val of_yojson : Yojson.Safe.t -> (t, string) result

  val registry : t -> string option

  val name : t -> string

  val tag : t -> string option
end = struct
  type t = {
    registry : string option;
    name : string;
    tag : string option;
    hash : string option;
  }
  [@@deriving eq]

  let parse_name s =
    match String.lsplit2 ~on:':' s with
    | None -> s, None, None
    | Some (name, rest) -> (
      match String.lsplit2 ~on:'@' rest with
      | None -> name, Some rest, None
      | Some (tag, hash) -> name, Some tag, Some hash)

  let of_string s =
    match String.lsplit2 ~on:'/' s with
    | None ->
        let registry = None in
        let name, tag, hash = parse_name s in
        {registry; name; tag; hash}
    | Some (candidate_registry, candidate_name) -> (
      match String.mem candidate_registry '.' with
      | true ->
          let registry = Some candidate_registry in
          let name, tag, hash = parse_name candidate_name in
          {registry; name; tag; hash}
      | false ->
          let registry = None in
          let name, tag, hash = parse_name s in
          {registry; name; tag; hash})

  let to_string {registry; name; tag; hash} =
    let registry_chunk =
      match registry with
      | Some r -> Printf.sprintf "%s/" r
      | None -> ""
    in
    let tag_chunk =
      match tag with
      | None -> ""
      | Some t -> (
        match hash with
        | None -> Printf.sprintf ":%s" t
        | Some h -> Printf.sprintf ":%s@%s" t h)
    in
    Printf.sprintf "%s%s%s" registry_chunk name tag_chunk

  let pp () = to_string

  let of_yojson = function
    | `String s -> Ok (of_string s)
    | _ -> Error "Swarm_types.Image.t"

  let basename t = {t with hash = None}

  let equal_nametag a b = equal (basename a) (basename b)

  let registry {registry; _} = registry

  let tag {tag; _} = tag

  let name {name; _} = name
end

module Swarm : sig
  type t

  val of_host_and_port : string * int -> t

  val to_host_and_port : t -> string * int
end = struct
  type t = string * int

  let of_host_and_port = Fn.id

  let to_host_and_port = Fn.id
end

type service_name = Service.t

let service_name_of_yojson = function
  | `String s -> Ok (Service.of_string s)
  | _ -> Error "Swarm_types.service_name"

type container_spec = {image : Image.t [@key "Image"]}
[@@deriving of_yojson {strict = false}]

type task_template = {container_spec : container_spec [@key "ContainerSpec"]}
[@@deriving of_yojson {strict = false}]

type spec = {
  name : service_name; [@key "Name"]
  task_template : task_template; [@key "TaskTemplate"]
}
[@@deriving of_yojson {strict = false}]

type state =
  | Completed
  | Paused
  | Updating
  | RollbackCompleted
  | RollbackPaused
  | RollbackStarted

let state_of_yojson = function
  | `String "completed" -> Ok Completed
  | `String "updating" -> Ok Updating
  | `String "paused" -> Ok Paused
  | `String "rollback_started" -> Ok RollbackStarted
  | `String "rollback_paused" -> Ok RollbackPaused
  | `String "rollback_completed" -> Ok RollbackCompleted
  | `String unexpected ->
      Error (Printf.sprintf "Swarm_types.state unexpected string: '%s'" unexpected)
  | unexpected ->
      let passed_in =
        Yojson.Safe.to_string unexpected
        |> Printf.sprintf "Swarm_types.state invalid JSON: '%s'"
      in
      Error passed_in

type update_status = {state : state [@key "State"]}
[@@deriving of_yojson {strict = false}]

type service_status = {
  id : string; [@key "ID"]
  spec : spec; [@key "Spec"]
  update_status : update_status; [@key "UpdateStatus"] [@default {state = Completed}]
}
[@@deriving of_yojson {strict = false}]

type service_response = service_status list [@@deriving of_yojson]

type image_config = {digest : string} [@@deriving of_yojson {strict = false}]

type image_manifest = {config : image_config} [@@deriving of_yojson {strict = false}]
