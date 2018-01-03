open Core
open Async

let main host port stack_name () =
  let%bind services_or = Lib.Requests.services (host, port) stack_name in
  let services = Or_error.ok_exn services_or |> List.map ~f:Lib.Swarm_types.Service.to_string in
  let v = String.concat ~sep:", " services in
  Caml.Printf.printf "Services: %s\n" v;
  Deferred.return ()

let () =
  let stack_name = Command.Spec.Arg_type.create Lib.Swarm_types.Stack.of_string in
  Command.async
    ~summary:"Return recommendation JSON on stdout"
    Command.Spec.(
      empty
      +> flag "--host" (required string)
         ~doc:" Hostname to connect to"
      +> flag "--port" (optional_with_default 2375 int)
         ~doc:" Port to connect to"
      +> anon ("stack-name" %: stack_name))
    main
  |> Command.run
