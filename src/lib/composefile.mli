open Core

type context = string String.Map.t
type service_spec = {
  name: string;
  image: string;
}
type parsed

val load_file : string -> parsed Or_error.t
val specs : parsed -> service_spec list Or_error.t
val resolve_specs : context -> service_spec list -> service_spec list Or_error.t
val environment : unit -> context
