module type Identifier = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val pp : unit -> t -> string
end

module Stack : Identifier = struct
  type t = string
  let of_string = Core.Fn.id
  let to_string = Core.Fn.id
  let pp () = to_string
end

module Service : Identifier = struct
  type t = string
  let of_string = Core.Fn.id
  let to_string = Core.Fn.id
  let pp () = to_string
end

module Swarm : sig
  type t
  val of_host_and_port : (string * int) -> t
  val to_host_and_port : t -> (string * int)
end = struct
  type t = string * int
  let of_host_and_port = Core.Fn.id
  let to_host_and_port = Core.Fn.id
end

type service_name = Service.t

let service_name_of_yojson = function
  | `String s -> Ok (Service.of_string s)
  | _ -> Error "swarm_types.service_name"

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

type state = Completed | Paused | Updating

let state_of_yojson = function
  | `String "completed" -> Ok Completed
  | `String "updating" -> Ok Updating
  | `String "paused" -> Ok Paused
  | `String unexpected -> Error (Printf.sprintf "swarm_types.state unexpected string: '%s'" unexpected)
  | unexpected ->
    let passed_in = Yojson.Safe.to_string unexpected
      |> Printf.sprintf "swarm_types.state invalid JSON: '%s'"
    in
    Error passed_in

type update_status = {
  state : state [@key "State"];
} [@@deriving of_yojson { strict = false }]

type service_status = {
  id : string [@key "ID"];
  spec : spec [@key "Spec"];
  update_status : update_status [@key "UpdateStatus"];
} [@@deriving of_yojson { strict = false }]

type service_response = service_status list [@@deriving of_yojson]
