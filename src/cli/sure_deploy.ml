open Core
open Async

module Stack = Lib.Swarm_types.Stack

let main host port timeout_seconds verbose stack () =
  let swarm = (host, port) in
  let timeout = Time.Span.of_sec timeout_seconds in
  let level = match verbose with
    | true -> `Info
    | false -> `Error
  in
  Log.Global.set_level level;
  let open Deferred.Or_error.Let_syntax in
  match%bind Lib.Requests.services swarm stack with
  | [] ->
    Deferred.Or_error.errorf "No services found for stack '%a'" Stack.pp stack
  | services ->
    let service_names = List.map services ~f:Lib.Swarm_types.Service.to_string |> String.concat ~sep:", " in
    Log.Global.info "Services detected in '%a' stack: %s" Stack.pp stack service_names;
    let open Deferred.Let_syntax in
    match%bind Lib.Monitor.wait_for_completion_with_timeout timeout swarm services with
    | `Timeout ->
      Log.Global.error "Waiting for convergence of stack '%a' timed out after %.3f seconds" Stack.pp stack timeout_seconds;
      Deferred.Or_error.return ()
    | `Result () ->
      Log.Global.info "Stack '%a' has converged" Stack.pp stack;
      Deferred.Or_error.return ()

let () =
  let stack_name = Command.Spec.Arg_type.create Lib.Swarm_types.Stack.of_string in
  Command.async_or_error
    ~summary:"Wait for convergence of stack deployment on Docker Swarm"
    Command.Spec.(
      empty
      +> flag "--host" (required string)
         ~doc:" Hostname to connect to"
      +> flag "--port" (optional_with_default 2375 int)
         ~doc:" Port to connect to"
      +> flag "--timeout" (optional_with_default 600. float)
         ~doc:" Maximum time to wait for convergence"
      +> flag "--verbose" no_arg
         ~doc:" Display more status information"
      +> anon ("stack-name" %: stack_name))
    main
  |> Command.run
