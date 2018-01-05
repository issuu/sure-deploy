open Core
open Async

module Service = Swarm_types.Service

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
      Debounce.trigger debounce (fun () ->
        let waiting_for = List.length uncompleted in
        let service_names = List.map uncompleted ~f:Service.to_string |> String.concat ~sep:", " in
        (match waiting_for with
        | 1 -> Log.Global.info "Waiting for '%s' to settle" service_names
        | n -> Log.Global.info "Waiting for %d services to settle: %s" n service_names));
      let%bind () = after polling_interval in
      wait uncompleted
  in
  wait services

let rec wait_for_completion_with_timeout timeout polling_interval swarm services =
  Clock.with_timeout timeout (wait_for_completion polling_interval swarm services)
