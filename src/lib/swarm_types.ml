type spec = {
  name : string [@key "Name"];
} [@@deriving of_yojson { strict = false }]

type update_status = {
  state : string [@key "State"];
  started_at : string [@key "StartedAt"];
  completed_at : string [@key "CompletedAt"];
  message : string [@key "Message"];
} [@@deriving of_yojson { strict = false }]

type service_status = {
  id : string [@key "ID"];
  spec : spec [@key "Spec"];
  previous_spec : spec [@key "PreviousSpec"];
} [@@deriving of_yojson { strict = false }]
