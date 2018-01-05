open Core
open Async

let check_service swarm service =
  let%bind res = Requests.finished swarm service in
  match res with
  | Ok true -> return None
  | Ok false -> return @@ Some service
  | Error e ->
    Log.Global.error "Checking status of service '%a' failed with %s, removing from waitlist." Swarm_types.Service.pp service @@ Error.to_string_hum e;
    return None

let wait_for_completion polling_interval swarm services =
  let debounce = Debounce.init () in
  let rec wait services =
    match%bind Deferred.List.filter_map ~how:`Parallel services ~f:(check_service swarm) with
    | [] -> Deferred.unit
    | uncompleted ->
      Debounce.trigger debounce (fun () -> Log.Global.info "Waiting for %d more service(s) to settle" @@ List.length uncompleted);
      let%bind () = after polling_interval in
      wait uncompleted
  in
  wait services

let rec wait_for_completion_with_timeout timeout polling_interval swarm services =
  Clock.with_timeout timeout (wait_for_completion polling_interval swarm services)
