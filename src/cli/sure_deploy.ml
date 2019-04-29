open Core
open Async

module Stack = Lib.Swarm_types.Stack
module Swarm = Lib.Swarm_types.Swarm
module Service = Lib.Swarm_types.Service
module Image = Lib.Swarm_types.Image

let set_verbose verbose =
  Log.Global.set_level @@ match verbose with
    | true -> `Info
    | false -> `Error

let environment () =
  let open Or_error.Monad_infix in
  ()
  |> Unix.environment
  |> Array.map ~f:(String.lsplit2 ~on:'=')
  |> Array.to_list
  |> Option.all
  |> Result.of_option ~error:(Error.of_string "Variables could not be parsed, you have environment variables not containing '='")
  >>= String.Map.of_alist_or_error

let converge host port verbose stack timeout_seconds poll_interval =
  set_verbose verbose;
  let swarm = Swarm.of_host_and_port (host, port) in
  let timeout = Time.Span.of_sec timeout_seconds in
  let open Deferred.Or_error.Let_syntax in
  match%bind Lib.Requests.services swarm stack with
  | [] ->
    Deferred.Or_error.errorf "No services found for stack '%a'" Stack.pp stack
  | services ->
    let service_names = List.map services ~f:Service.to_string |> String.concat ~sep:", " in
    Log.Global.info "Services detected in '%a' stack: %s" Stack.pp stack service_names;
    let open Deferred.Let_syntax in
    match%bind Lib.Monitor.wait_for_completion_with_timeout timeout poll_interval swarm stack services with
    | `Timeout ->
      Deferred.Or_error.errorf "Waiting for convergence of stack '%a' timed out after %.3f seconds" Stack.pp stack timeout_seconds
    | `Result () ->
      Log.Global.info "Stack '%a' has converged" Stack.pp stack;
      Deferred.Or_error.return ()

let match_spec_and_service
  : Stack.t -> Lib.Composefile.service_spec list -> (Service.t * Image.t) list -> (Service.t * Image.t * Image.t) list Or_error.t =
  fun stack specs service_images ->
  let open Or_error.Let_syntax in
  let%bind matched = List.fold_result specs ~init:[] ~f:(fun acc {Lib.Composefile.name; image} ->
    match List.Assoc.find service_images ~equal:(Service.equal_basename stack) name with
    | None -> Or_error.errorf "Failed to find spec '%a' in deployed services" Service.pp name
    | Some deployed_image -> Or_error.return @@ (name, image, deployed_image) :: acc)
  in
  let spec_count = List.length specs in
  let service_count = List.length service_images in
  match spec_count = service_count with
  | true -> Or_error.return matched
  | false ->
    Or_error.errorf "Amount of services deployed (%d) does not match services specified (%d)" service_count spec_count

let verify host port verbose stack composefile =
  let open Deferred.Or_error.Let_syntax in
  set_verbose verbose;
  let swarm = Swarm.of_host_and_port (host, port) in
  let%bind env = Deferred.return @@ environment () in
  match Lib.Composefile.load composefile env with
  | Error _ as e -> Deferred.return e
  | Ok specs ->
    let%bind deployed_service_images = Lib.Requests.service_images swarm stack in
    let%bind matched_spec = Deferred.return @@ match_spec_and_service stack specs deployed_service_images in
    let%bind () = Deferred.Result.all_unit @@ List.map matched_spec ~f:(fun (name, desired, deployed) ->
      match Image.equal_nametag desired deployed with
      | true -> Deferred.Or_error.return ()
      | false ->
          let default = "registry.hub.docker.com" in
          let deployed_registry = Option.value ~default @@ Image.registry deployed in
          let desired_registry = Option.value ~default @@ Image.registry desired in
          let default = "latest" in
          let deployed_tag = Option.value ~default @@ Image.tag deployed in
          let desired_tag = Option.value ~default @@ Image.tag desired in
          let%bind deployed_hash = Lib.Requests.image_digest ~registry:deployed_registry ~name:(Image.name deployed) ~tag:deployed_tag in
          let%bind desired_hash = Lib.Requests.image_digest ~registry:desired_registry ~name:(Image.name desired) ~tag:desired_tag in
          match String.equal deployed_hash desired_hash with
          | true -> Deferred.Or_error.return ()
          | false ->
            Deferred.Or_error.errorf "Service '%a' expected '%a' but '%a' was deployed" Service.pp name Image.pp desired Image.pp deployed)
    in
    return @@ Log.Global.info "Swarm state and composefile match"

let () =
  let stack_name = Command.Spec.Arg_type.create Stack.of_string in
  let span_ms = Command.Spec.Arg_type.create (Fn.compose Time.Span.of_ms float_of_string) in
  let converge = Command.async_or_error
    ~summary:"Wait for convergence of stack deployment on Docker Swarm"
    (let open Command.Let_syntax in
    [%map_open
      let host = flag "--host" (required string)
         ~doc:" Hostname to connect to"
      and port = flag "--port" (optional_with_default 2375 int)
         ~doc:" Port to connect to"
      and verbose = flag "--verbose" no_arg
         ~doc:" Display more status information"
      and stack = anon ("stack-name" %: stack_name)
      and timeout = flag "--timeout" (optional_with_default 600. float)
         ~doc:" Maximum time to wait for convergence"
      and poll = flag "--poll-interval" (optional_with_default (Time.Span.of_ms 500.) span_ms)
         ~doc:" Maximum time to wait for convergence"
      in
      fun () -> converge host port verbose stack timeout poll])
  in
  let verify = Command.async_or_error
    ~summary:"Compare deployment status of services in Docker Swarm with docker-compose.yml definitions"
    (let open Command.Let_syntax in
    [%map_open
      let host = flag "--host" (required string)
         ~doc:" Hostname to connect to"
      and port = flag "--port" (optional_with_default 2375 int)
         ~doc:" Port to connect to"
      and verbose = flag "--verbose" no_arg
         ~doc:" Display more status information"
      and stack = anon ("stack-name" %: stack_name)
      and composefile = flag "--compose-file" (optional_with_default "docker-compose.yml" string)
        ~doc:" Compose file to read (default: docker-compose.yml)"
      in
      fun () -> verify host port verbose stack composefile])
  in
  Command.group
    ~summary:"Deployment helper for Docker Stack"
    [
      ("converge", converge);
      ("verify", verify);
    ]
  |> Command.run
