open Core

type service_spec = {
  name: Swarm_types.service_name;
  image: string;
  (* TODO: possibly other interesting values *)
}

type parsed = Yaml.value

let environment () =
  Unix.environment ()
  |> Array.map ~f:(String.lsplit2_exn ~on:'=')
  |> Array.to_list
  |> String.Map.of_alist_exn

let load_file : string -> parsed Or_error.t =
  fun filename ->
  match In_channel.read_all filename with
  | exception Sys_error e -> Or_error.errorf "%s" e
  | v ->
      v
      |> Yaml.of_string
      |> function
        | Ok v -> Or_error.return v
        | Error (`Msg s) -> Or_error.errorf "%s" s

let parse_service : (string * Yaml.value) -> service_spec Or_error.t =
  fun (name, definition) ->
    match definition with
      | `O defs -> (
        match List.Assoc.find defs ~equal:String.equal "image" with
        | None -> Or_error.errorf "Definition of service '%s' has no 'image' field" name
        | Some (`String image) -> Or_error.return { name = Swarm_types.Service.of_string name; image }
        | Some _ -> Or_error.errorf "Definition of 'image' field in service '%s' has invalid type" name)
      | _ -> Or_error.errorf "Expected an object in definition of service '%s'" name

let parse_services : Yaml.value -> service_spec list Or_error.t = function
  | `O service_list ->
    service_list
    |> List.map ~f:parse_service
    |> Or_error.all
  | _ -> Or_error.errorf "'service' key did not map to an object"

let specs : parsed -> service_spec list Or_error.t = function
  | `O top_level ->
    (match List.Assoc.find top_level ~equal:String.equal "services" with
    | None -> Or_error.errorf "No 'services' defined"
    | Some services -> parse_services services)
  | _ -> Or_error.errorf "Top level object expected"

type context = string String.Map.t

let substitute_template : context -> service_spec -> service_spec Or_error.t =
  fun context ({image; _} as spec) ->
    let open Or_error.Let_syntax in
    let%bind image = Variable.substitute context image in
    return { spec with image }

let resolve_specs : context -> service_spec list -> service_spec list Or_error.t =
  fun context specs ->
    specs
    |> List.map ~f:(substitute_template context)
    |> Or_error.all

let load : string -> context -> service_spec list Or_error.t =
  fun filename context ->
    let open Or_error.Let_syntax in
    let%bind parsed = load_file filename in
    let%bind specs = specs parsed in
    resolve_specs context specs
