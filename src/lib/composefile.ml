open Core

type service_spec = {
  name: string;
  image: string;
  (* TODO: possibly other interesting values *)
}

type parsed = Yaml.value

let environment () =
  Unix.environment ()
  |> Array.map ~f:(String.lsplit2_exn ~on:'=')
  |> Array.to_list
  |> String.Map.of_alist_exn

let load_file : string -> parsed Yaml.res =
  fun filename ->
  filename
  |> In_channel.read_all
  |> Yaml.of_string

let parse_service : (string * Yaml.value) -> (service_spec, string) result =
  fun (name, definition) ->
    match definition with
      | `O defs -> (
        match List.Assoc.find defs ~equal:String.equal "image" with
        | None -> Result.Error (Printf.sprintf "Definition of service '%s' has no 'image' field" name)
        | Some (`String image) -> Result.Ok { name; image }
        | Some _ -> Result.Error (Printf.sprintf "Definition of 'image' field in service '%s' has invalid type" name))
      | _ -> Result.Error (Printf.sprintf "Expected an object in definition of service '%s'" name)

let parse_services : Yaml.value -> (service_spec list, string) result = function
  | `O service_list ->
    service_list
    |> List.map ~f:parse_service
    |> Result.all
  | _ -> Result.Error "'service' key did not map to an object"

let specs : parsed -> (service_spec list, string) result = function
  | `O top_level ->
    (match List.Assoc.find top_level ~equal:String.equal "services" with
    | None -> Result.Error "No 'services' defined"
    | Some services -> parse_services services)
  | _ -> Result.Error "Top level object expected"

type context = string String.Map.t

let substitute_template : context -> service_spec -> (service_spec, string) result =
  fun context ({image; _} as spec) ->
    match Variable.substitute context image with
    | Ok image -> Result.Ok  { spec with image }
    | Error e -> Result.Error (Error.to_string_hum e)

let resolve_specs : context -> service_spec list -> (service_spec list, string) result =
  fun context specs ->
    specs
    |> List.map ~f:(substitute_template context)
    |> Result.all
