open Core
open Async

module Stack = Lib.Swarm_types.Stack
module Swarm = Lib.Swarm_types.Swarm
module Service = Lib.Swarm_types.Service

let set_verbose verbose =
  Log.Global.set_level @@ match verbose with
    | true -> `Info
    | false -> `Error

let converge host port verbose stack timeout_seconds () =
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
    match%bind Lib.Monitor.wait_for_completion_with_timeout timeout swarm services with
    | `Timeout ->
      Log.Global.error "Waiting for convergence of stack '%a' timed out after %.3f seconds" Stack.pp stack timeout_seconds;
      Deferred.Or_error.return ()
    | `Result () ->
      Log.Global.info "Stack '%a' has converged" Stack.pp stack;
      Deferred.Or_error.return ()

let check host port verbose stack prefix () =
  set_verbose verbose;
  let swarm = Swarm.of_host_and_port (host, port) in
  let open Deferred.Or_error.Let_syntax in
  match%bind Lib.Requests.images swarm stack with
  | [] ->
    Deferred.Or_error.errorf "No services found for stack '%a'" Stack.pp stack
  | images ->
    let image_names = String.concat ~sep:", " images in
    Log.Global.info "Images detected in '%a' stack: %s" Stack.pp stack image_names;
    match List.for_all images ~f:(fun image -> String.is_prefix ~prefix image) with
    | true ->
      Log.Global.info "Stack '%a' has all services run '%s*'" Stack.pp stack prefix;
      Deferred.Or_error.return ()
    | false -> Deferred.Or_error.errorf "Some services have different images deployed"

let () =
  let stack_name = Command.Spec.Arg_type.create Stack.of_string in
  let common_spec () = Command.Spec.(
      empty
      +> flag "--host" (required string)
         ~doc:" Hostname to connect to"
      +> flag "--port" (optional_with_default 2375 int)
         ~doc:" Port to connect to"
      +> flag "--verbose" no_arg
         ~doc:" Display more status information"
      +> anon ("stack-name" %: stack_name))
  in
  let converge = Command.async_or_error
    ~summary:"Wait for convergence of stack deployment on Docker Swarm"
    Command.Spec.(
      (common_spec ())
      +> flag "--timeout" (optional_with_default 600. float)
         ~doc:" Maximum time to wait for convergence")
    converge
  in
  let check = Command.async_or_error
    ~summary:"Check deployment status of services in Docker Starm"
    Command.Spec.(
      (common_spec ())
      +> flag "--ensure-image" (required string)
         ~doc:" Ensure all containers run a specific image (prefix)")
    check
  in
  Command.group
    ~summary:"Deployment helper for Docker Stack"
    [
      ("converge", converge);
      ("check", check);
    ]
  |> Command.run
