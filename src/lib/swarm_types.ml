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
end

module Service : SwarmIdentifier = struct
  type t = string
  let of_string = Fn.id
  let to_string = Fn.id
  let pp () = to_string
  let basename stack service =
    let prefix = Printf.sprintf "%a_" Stack.pp stack in
    String.chop_prefix_exn ~prefix service
end

module Swarm : sig
  type t
  val of_host_and_port : (string * int) -> t
  val to_host_and_port : t -> (string * int)
end = struct
  type t = string * int
  let of_host_and_port = Fn.id
  let to_host_and_port = Fn.id
end

type service_name = Service.t

let service_name_of_yojson = function
  | `String s -> Ok (Service.of_string s)
  | _ -> Error "Swarm_types.service_name"

type container_spec = {
  image : string [@key "Image"]
} [@@deriving of_yojson { strict = false }]

type task_template = {
  container_spec : container_spec [@key "ContainerSpec"]
} [@@deriving of_yojson { strict = false }]

type spec = {
  name : service_name [@key "Name"];
  task_template : task_template [@key "TaskTemplate"]
} [@@deriving of_yojson { strict = false }]

type state = Completed | Paused | Updating | RollbackCompleted | RollbackPaused | RollbackStarted

let state_of_yojson = function
  | `String "completed" -> Ok Completed
  | `String "updating" -> Ok Updating
  | `String "paused" -> Ok Paused
  | `String "rollback_started" -> Ok RollbackStarted
  | `String "rollback_paused" -> Ok RollbackPaused
  | `String "rollback_completed" -> Ok RollbackCompleted
  | `String unexpected -> Error (Printf.sprintf "Swarm_types.state unexpected string: '%s'" unexpected)
  | unexpected ->
    let passed_in = Yojson.Safe.to_string unexpected
      |> Printf.sprintf "Swarm_types.state invalid JSON: '%s'"
    in
    Error passed_in

type update_status = {
  state : state [@key "State"];
} [@@deriving of_yojson { strict = false }]

type service_status = {
  id : string [@key "ID"];
  spec : spec [@key "Spec"];
  update_status : update_status [@key "UpdateStatus"] [@default { state = Updating } ];
} [@@deriving of_yojson { strict = false }]

type service_response = service_status list [@@deriving of_yojson]
