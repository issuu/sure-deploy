open Core
open Async

let main host port swarm_name () =
  let%bind services_or = Lib.Requests.services (host, port) swarm_name in
  let services = Or_error.ok_exn services_or in
  let v = String.concat ~sep:", " services in
  Caml.Printf.printf "Services %s\n" v;
  Deferred.return ()

let () =
  Command.async
    ~summary:"Return recommendation JSON on stdout"
    Command.Spec.(
      empty
      +> flag "--host" (required string)
         ~doc:" Hostname to connect to"
      +> flag "--port" (optional_with_default 2375 int)
         ~doc:" Port to connect to"
      +> anon ("swarm-name" %: string))
    main
  |> Command.run
