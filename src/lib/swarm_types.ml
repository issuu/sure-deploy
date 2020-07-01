open Core
module SSLConfig = Conduit_async.V2.Ssl.Config
module Client = Cohttp_async.Client

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

  val registry : t -> string

  val registry_port : t -> int option

  val registry_full : t -> string

  val name : t -> string

  val tag : t -> string
end = struct
  type t = {
    registry : string;
    registry_port : int option;
    name : string;
    tag : string;
    hash : string option;
  }
  [@@deriving eq]

  let default_registry = "registry.hub.docker.com"

  let default_tag = "latest"

  let parse_name_tag_hash s =
    match String.lsplit2 ~on:':' s with
    | None -> s, default_tag, None
    | Some (name, rest) -> (
      match String.lsplit2 ~on:'@' rest with
      | None -> name, rest, None
      | Some (tag, hash) -> name, tag, Some hash)

  let parse_registry_port_name_tag_hash s =
    match String.lsplit2 ~on:'/' s with
    (* Default Docker Registry *)
    | None ->
        let name, tag, hash = parse_name_tag_hash s in
        default_registry, None, name, tag, hash
    (* Maybe a Custom Registry *)
    | Some (registry, h) ->
        (* Check for localhost and or TLD *)
        if String.is_substring ~substring:"localhost" registry
           || String.is_substring ~substring:"." registry
        then
          let name, tag, hash = parse_name_tag_hash h in
          (* Custom registry could use a custom port *)
          match String.lsplit2 ~on:':' registry with
          | None -> registry, None, name, tag, hash
          | Some (registry, port) -> registry, Some (int_of_string port), name, tag, hash
        else
          let name, tag, hash = parse_name_tag_hash s in
          default_registry, None, name, tag, hash

  let of_string s =
    let registry, registry_port, name, tag, hash = parse_registry_port_name_tag_hash s in
    {registry; registry_port; name; tag; hash}

  let create ?(registry = default_registry) ?registry_port ?(tag = default_tag) ?hash
      name
    =
    {registry; registry_port; name; tag; hash}

  let%test "plain name" = equal (of_string "n") (create "n")

  let%test "org name" = equal (of_string "org/n") (create "org/n")

  let%test "registry name" =
    equal (of_string "registry.tld/n") (create ~registry:"registry.tld" "n")

  let%test "registry orgname" =
    equal (of_string "registry.tld/org/n") (create ~registry:"registry.tld" "org/n")

  let%test "name tag" = equal (of_string "n:t") (create ~tag:"t" "n")

  let%test "org name tag" = equal (of_string "org/n:t") (create ~tag:"t" "org/n")

  let%test "registry name tag" =
    equal (of_string "registry.tld/n:t") (create ~registry:"registry.tld" ~tag:"t" "n")

  let%test "registry orgname tag" =
    equal
      (of_string "registry.tld/org/n:t")
      (create ~registry:"registry.tld" ~tag:"t" "org/n")

  let%test "registry with port orgname tag" =
    equal
      (of_string "registry.tld:5000/org/n:t")
      (create ~registry:"registry.tld" ~registry_port:5000 ~tag:"t" "org/n")

  let%test "localhost registry with port orgname tag" =
    equal
      (of_string "localhost:5000/org/n:t")
      (create ~registry:"localhost" ~registry_port:5000 ~tag:"t" "org/n")

  let%test "localhost registry with orgname tag" =
    equal (of_string "localhost/org/n:t") (create ~registry:"localhost" ~tag:"t" "org/n")

  let print_full_registry registry registry_port =
    match registry, registry_port with
    | registry, Some port -> Printf.sprintf "%s:%d/" registry port
    | registry, None -> Printf.sprintf "%s/" registry

  let to_string {registry; registry_port; name; tag; hash} =
    let registry_chunk = print_full_registry registry registry_port in
    let tag_chunk =
      match hash with
      | None -> Printf.sprintf ":%s" tag
      | Some hash -> Printf.sprintf ":%s@%s" tag hash
    in
    Printf.sprintf "%s%s%s" registry_chunk name tag_chunk

  let pp () = to_string

  let of_yojson = function
    | `String s -> Ok (of_string s)
    | _ -> Error "Swarm_types.Image.t"

  let basename t = {t with hash = None}

  let equal_nametag a b = equal (basename a) (basename b)

  let registry {registry; _} = registry

  let registry_port {registry_port; _} = registry_port

  let registry_full {registry; registry_port; _} =
    print_full_registry registry registry_port

  let tag {tag; _} = tag

  let name {name; _} = name
end

module Swarm : sig
  type t

  type destination =
    | Host of string
    | Socket of string

  val of_destination_and_port : ?ssl_config:SSLConfig.t -> destination * int -> t

  val make_uri
    :  t ->
    ?userinfo:string ->
    ?path:string ->
    ?query:(string * string list) list ->
    ?fragment:string ->
    unit ->
    Uri.t

  val get
    :  t ->
    ?interrupt:unit Async_kernel.Deferred.t ->
    ?headers:Cohttp.Header.t ->
    Uri.t ->
    (Cohttp.Response.t * Cohttp_async.Body.t) Async_kernel.Deferred.t
end = struct
  type destination =
    | Host of string
    | Socket of string

  type t = {
    ssl_config : SSLConfig.t option;
    destination : destination;
    port : int;
  }

  let of_destination_and_port ?ssl_config (destination, port) =
    {ssl_config; destination; port}

  let get_swarm_scheme destination ssl_config =
    match destination, ssl_config with
    | Socket _, _ -> "httpunix"
    | Host _, None -> "http"
    | Host _, Some _ -> "https"

  let get_swarm_host = function
    | Host h -> h
    | Socket s -> s

  let make_uri t =
    let port =
      match t.destination with
      | Socket _ -> None
      | Host _ -> Some t.port
    in
    Uri.make
      ~scheme:(get_swarm_scheme t.destination t.ssl_config)
      ~host:(get_swarm_host t.destination)
      ?port

  let get t = Client.get ?ssl_config:t.ssl_config
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
