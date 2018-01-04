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

type spec = {
  name : service_name [@key "Name"];
} [@@deriving of_yojson { strict = false }]

type state = Completed | Paused | Updating

let state_of_yojson = function
  | `String "completed" -> Ok Completed
  | `String "updating" -> Ok Updating
  | `String "paused" -> Ok Paused
  | _ -> Error "swarm_types.state"

type update_status = {
  state : state [@key "State"];
  started_at : string [@key "StartedAt"];
  completed_at : string [@key "CompletedAt"];
  message : string [@key "Message"];
} [@@deriving of_yojson { strict = false }]

type service_status = {
  id : string [@key "ID"];
  spec : spec [@key "Spec"];
  update_status : update_status [@key "UpdateStatus"];
} [@@deriving of_yojson { strict = false }]

type service_response = service_status list [@@deriving of_yojson]
