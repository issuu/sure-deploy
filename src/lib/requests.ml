open Core
open Async

module Client = Cohttp_async.Client
module Body = Cohttp_async.Body
module Response = Cohttp.Response
module Code = Cohttp.Code

(* Docker supports a number of different APIs *)
let api_version = "v1.24"

let filter_for_stack stack_name =
  `Assoc [
    ("label", `Assoc [
      (Printf.sprintf "com.docker.stack.namespace=%a" Swarm_types.Stack.pp stack_name, `Bool true);
    ]);
  ]

let filter stack_name =
  stack_name
  |> filter_for_stack
  |> Yojson.Safe.to_string

let service_list service_resp =
  List.map service_resp ~f:(fun service -> Swarm_types.(service.spec.name))

let services (host, port) stack_name =
  let url =
    Uri.make ~scheme:"http" ~host ~port ~path:(Printf.sprintf "/%s/services" api_version) ()
    |> (Fn.flip Uri.add_query_param') ("filters", filter stack_name)
  in
  let%bind (resp, body) = Client.get url in
  match Response.status resp |> Code.code_of_status with
  | 200 -> (
    let%bind body = Body.to_string body in
    match body |> Yojson.Safe.from_string |> Swarm_types.service_response_of_yojson with
    | Ok v ->
      v
      |> service_list
      |> Deferred.Or_error.return
    | Error e -> Deferred.Or_error.errorf "Parsing response failed with '%s'" e)
  | invalid -> Deferred.Or_error.errorf "Listing services failed with error %d" invalid

let status (host, port) service_name =
  let url = Uri.make ~scheme:"http" ~host ~port
    ~path:(Printf.sprintf "/%s/services/%a" api_version Swarm_types.Service.pp service_name) ()
  in
  let%bind (resp, body) = Client.get url in
  match Response.status resp |> Code.code_of_status with
  | 200 -> (
    let%bind body = Body.to_string body in
    match body |> Yojson.Safe.from_string |> Swarm_types.service_status_of_yojson with
    | Ok service ->
      service.update_status.state
      |> Deferred.Or_error.return
    | Error e -> Deferred.Or_error.errorf "Parsing response failed with '%s'" e)
  | invalid -> Deferred.Or_error.errorf "Accessing status failed with error %d" invalid

let finished swarm service_name =
  let open Deferred.Or_error.Let_syntax in
  match%map status swarm service_name with
  | Completed -> true
  | Paused -> true
  | Updating -> false
