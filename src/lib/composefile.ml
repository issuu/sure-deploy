open Core

type service_spec = {
  name: string;
  image: string;
  (* TODO: possibly other interesting values *)
}

let environment () =
  Unix.environment ()
  |> Array.map ~f:(String.lsplit2_exn ~on:'=')
  |> Array.to_list
  |> String.Map.of_alist_exn

let load_file filename =
  filename
  |> In_channel.read_all
  |> Yaml.yaml_of_string

let parse_service (name, definition) =
  match List.Assoc.find definition ~equal:String.equal "image" with
  | None -> Result.Error "Definition of service TODO has no 'image'"
  | Some (`String image) -> Result.Ok { name; image }
  | Some _ -> Result.Error "Definition of service TODO on 'image' has invalid type"

let parse_services = function
  | `O service_list ->
    service_list
    |> List.map ~f:parse_service
    |> Result.all
  | _ -> Result.Error "'service' key did not map to an object"

let specs = function
  | `O top_level ->
    (match List.Assoc.find top_level ~equal:String.equal "services" with
    | None -> Result.Error "No 'services' defined"
    | Some services -> parse_services services)
  | _ -> Result.Error "Top level object expected"

type context = string String.Map.t

let substitute_template : context -> service_spec -> service_spec Or_error.t = fun context ({image; _} as spec) ->
  let open Or_error.Let_syntax in
  let%bind image = Variable.substitute context image in
  return { spec with image }
