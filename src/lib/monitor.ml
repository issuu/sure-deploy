open Core
open Async

let delay = sec 0.5

let check_service swarm service =
  let%bind res = Requests.finished swarm service in
  match res with
  | Ok true -> return None
  | Ok false -> return @@ Some service
  | Error e ->
    Log.Global.error "Checking status of service '%a' failed with %s, removing from waitlist." Swarm_types.Service.pp service @@ Error.to_string_hum e;
    return None

let rec wait_for_completion swarm services =
  match%bind Deferred.List.filter_map ~how:`Parallel services ~f:(check_service swarm) with
  | [] -> Deferred.unit
  | uncompleted ->
    let%bind () = after delay in 
    wait_for_completion swarm uncompleted

let rec wait_for_completion_with_timeout timeout swarm services =
  Clock.with_timeout timeout (wait_for_completion swarm services)
