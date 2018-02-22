open Core

module Service = Swarm_types.Service
module Image = Swarm_types.Image

type service_spec = {
  name: Service.t;
  image: Image.t;
}

let load_file : string -> Yaml.value Or_error.t =
  fun filename ->
  Or_error.try_with_join @@ fun () ->
    In_channel.read_all filename
    |> Yaml.of_string
    |> Result.map_error ~f:(fun (`Msg s) -> Error.createf "%s" s)

let extract_service : (string * Yaml.value) -> service_spec Or_error.t =
  fun (name, definition) ->
    match definition with
      | `O defs -> (
        match List.Assoc.find defs ~equal:String.equal "image" with
        | None -> Or_error.errorf "Definition of service '%s' has no 'image' field" name
        | Some (`String image) ->
          let name = Service.of_string name in
          let image = Image.of_string image in
          Or_error.return { name; image }
        | Some _ -> Or_error.errorf "Definition of 'image' field in service '%s' has invalid type" name)
      | _ -> Or_error.errorf "Expected an object in definition of service '%s'" name

let extract_services : Yaml.value -> service_spec list Or_error.t = function
  | `O service_list ->
    service_list
    |> List.map ~f:extract_service
    |> Or_error.all
  | _ -> Or_error.errorf "'service' key did not map to an object"

let specs : Yaml.value -> service_spec list Or_error.t = function
  | `O top_level ->
    (match List.Assoc.find top_level ~equal:String.equal "services" with
    | None -> Or_error.errorf "No 'services' defined"
    | Some services -> extract_services services)
  | _ -> Or_error.errorf "Top level object expected"

type context = string String.Map.t

let substitute_template : context -> service_spec -> service_spec Or_error.t =
  fun context ({image; _} as spec) ->
    let open Or_error.Let_syntax in
    let%bind image = Variable.substitute context (Image.to_string image) in
    let image = Image.of_string image in
    return { spec with image }

let resolve_specs : context -> service_spec list -> service_spec list Or_error.t =
  fun context specs ->
    specs
    |> List.map ~f:(substitute_template context)
    |> Or_error.all

let load : string -> context -> service_spec list Or_error.t =
  fun filename context ->
    let open Or_error.Let_syntax in
    let%bind yaml_parsed = load_file filename in
    let%bind extracted_specs = specs yaml_parsed in
    resolve_specs context extracted_specs
