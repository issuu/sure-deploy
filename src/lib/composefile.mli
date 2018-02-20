open Core

type context = string String.Map.t
type service_spec = {
  name: string;
  image: string;
}
type parsed

val load_file : string -> parsed Yaml.res
val specs : parsed -> (service_spec list, string) result
val resolve_specs : context -> service_spec list -> (service_spec list, string) result
val environment : unit -> context
