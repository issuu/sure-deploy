open Core
open Async
module Client = Cohttp_async.Client
module Body = Cohttp_async.Body
module Response = Cohttp.Response
module Code = Cohttp.Code
module Headers = Cohttp.Header
module Swarm = Swarm_types.Swarm
module Stack = Swarm_types.Stack
module Service = Swarm_types.Service
module Image = Swarm_types.Image

(* Docker supports a number of different APIs *)
let api_version = "v1.24"

let filter_for_stack stack_name =
  `Assoc
    [ ( "label",
        `Assoc
          [Printf.sprintf "com.docker.stack.namespace=%a" Stack.pp stack_name, `Bool true]
      ) ]

let filter stack_name = stack_name |> filter_for_stack |> Yojson.Safe.to_string

let service_metadata swarm stack_name =
  let url =
    Swarm.make_uri swarm ~path:(Printf.sprintf "/%s/services" api_version) ()
    |> (Fn.flip Uri.add_query_param') ("filters", filter stack_name)
  in
  let%bind resp, body = Swarm.get swarm url in
  match Response.status resp |> Code.code_of_status with
  | 200 -> (
      let%bind body = Body.to_string body in
      match
        body |> Yojson.Safe.from_string |> Swarm_types.service_response_of_yojson
      with
      | Ok v -> Deferred.Or_error.return v
      | Error e ->
          Deferred.Or_error.errorf "Parsing response failed with '%s' on '%s'" e body)
  | invalid -> Deferred.Or_error.errorf "Listing services failed with error %d" invalid

let service_images swarm stack =
  let open Deferred.Or_error.Let_syntax in
  let%map resp = service_metadata swarm stack in
  resp
  |> List.map ~f:(fun service ->
         ( Swarm_types.(service.spec.name),
           Swarm_types.(service.spec.task_template.container_spec.image) ))

let services swarm stack =
  let open Deferred.Or_error.Let_syntax in
  let%map resp = service_images swarm stack in
  resp |> List.map ~f:fst

let status swarm service_name =
  let url = Swarm.make_uri swarm ~path:(Printf.sprintf "/%s/services/%a" api_version Service.pp service_name) () in
  let%bind resp, body = Swarm.get swarm url in
  match Response.status resp |> Code.code_of_status with
  | 200 -> (
      let%bind body = Body.to_string body in
      match body |> Yojson.Safe.from_string |> Swarm_types.service_status_of_yojson with
      | Ok service -> service.update_status.state |> Deferred.Or_error.return
      | Error e -> Deferred.Or_error.errorf "Parsing response failed with '%s'" e)
  | invalid -> Deferred.Or_error.errorf "Accessing status failed with error %d" invalid

let finished swarm service_name =
  let open Deferred.Or_error.Let_syntax in
  match%map status swarm service_name with
  | Completed | Paused | RollbackCompleted | RollbackPaused -> true
  | Updating | RollbackStarted -> false

let v2_manifest = "application/vnd.docker.distribution.manifest.v2+json"

let image_digest ~is_insecure_registry ?(registry_access_token = None) ~image =
  let scheme =
    match is_insecure_registry with
    | true -> "http"
    | false -> "https"
  in
  let url =
    Uri.make
      ~scheme
      ~host:(Image.registry image)
      ?port:(Image.registry_port image)
      ~path:(Printf.sprintf "/v2/%s/manifests/%s" (Image.name image) (Image.tag image))
      ()
  in
  let headers = Cohttp.Header.init_with "Accept" v2_manifest in
  let headers =
    match registry_access_token with
    | None -> headers
    | Some registry_access_token ->
        Cohttp.Header.add
          headers
          "authorization"
          (Printf.sprintf "Basic %s" registry_access_token)
  in
  let%bind resp, body = Client.get ~headers url in
  match Response.status resp |> Code.code_of_status with
  | 200 -> (
      let%bind body = Body.to_string body in
      match body |> Yojson.Safe.from_string |> Swarm_types.image_manifest_of_yojson with
      | Ok {config} -> Deferred.Or_error.return config.digest
      | Error e -> Deferred.Or_error.errorf "Parsing registry response failed on '%s'" e)
  | invalid -> Deferred.Or_error.errorf "Accessing registry failed with error %d" invalid
