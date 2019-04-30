open Core
open Async
module Stack = Swarm_types.Stack
module Service = Swarm_types.Service

let check_service swarm service =
  let%bind res = Requests.finished swarm service in
  match res with
  | Ok true -> return None
  | Ok false -> return @@ Some service
  | Error e ->
      Log.Global.error
        "Checking status of service '%a' failed with %s, removing from waitlist."
        Swarm_types.Service.pp
        service
      @@ Error.to_string_hum e;
      return None

let wait_for_completion polling_interval swarm stack services =
  let debounce = Debounce.init () in
  let wait services =
    match%bind
      Deferred.List.filter_map ~how:`Parallel services ~f:(check_service swarm)
    with
    | [] -> return @@ `Finished ()
    | uncompleted ->
        Debounce.trigger debounce (fun () ->
            let waiting_for = List.length uncompleted in
            let service_names =
              List.map uncompleted ~f:(Service.basename stack) |> String.concat ~sep:", "
            in
            match waiting_for with
            | 1 ->
                Log.Global.info
                  "Waiting for '%s' of stack '%a' to settle"
                  service_names
                  Stack.pp
                  stack
            | n ->
                Log.Global.info
                  "Waiting for %d services of stack '%a' to settle: %s"
                  n
                  Stack.pp
                  stack
                  service_names );
        let%bind () = after polling_interval in
        return @@ `Repeat uncompleted
  in
  Deferred.repeat_until_finished services wait

let wait_for_completion_with_timeout timeout polling_interval swarm stack services =
  Clock.with_timeout timeout (wait_for_completion polling_interval swarm stack services)
