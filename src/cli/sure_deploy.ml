open Core
open Async
module Stack = Lib.Swarm_types.Stack
module Swarm = Lib.Swarm_types.Swarm
module Service = Lib.Swarm_types.Service
module Image = Lib.Swarm_types.Image
module SSLConfig = Conduit_async.V2.Ssl.Config

let set_verbose verbose =
  Log.Global.set_level
  @@
  match verbose with
  | true -> `Info
  | false -> `Error

let environment () =
  let open Or_error.Monad_infix in
  ()
  |> Unix.environment
  |> Array.map ~f:(String.lsplit2 ~on:'=')
  |> Array.to_list
  |> Option.all
  |> Result.of_option
       ~error:
         (Error.of_string
            "Variables could not be parsed, you have environment variables not \
             containing '='")
  >>= String.Map.of_alist_or_error

let registry_access_value = Command.Arg_type.create (String.lsplit2_exn ~on:'=')

let (cert_flag, ca_cert_flag, key_flag) =
  let open Command.Param in
  flag "--cert" (optional string) ~doc:" Path to the vertificate",
  flag "--cacert" (optional string)
            ~doc:" Path to the certificate to verify the peer",
  flag "--key" (optional string) ~doc:" Path to the key file"


let converge ~verbose ~ssl_config host port stack timeout_seconds poll_interval =
  set_verbose verbose;
  let swarm = Swarm.of_host_and_port_and_ssl_config (host, port, ssl_config) in
  let timeout = Time.Span.of_sec timeout_seconds in
  let open Deferred.Or_error.Let_syntax in
  match%bind Lib.Requests.services swarm stack with
  | [] -> Deferred.Or_error.errorf "No services found for stack '%a'" Stack.pp stack
  | services -> (
      let service_names =
        List.map services ~f:Service.to_string |> String.concat ~sep:", "
      in
      Log.Global.info "Services detected in '%a' stack: %s" Stack.pp stack service_names;
      let open Deferred.Let_syntax in
      match%bind
        Lib.Monitor.wait_for_completion_with_timeout
          timeout
          poll_interval
          swarm
          stack
          services
      with
      | `Timeout ->
          Deferred.Or_error.errorf
            "Waiting for convergence of stack '%a' timed out after %.3f seconds"
            Stack.pp
            stack
            timeout_seconds
      | `Result () ->
          Log.Global.info "Stack '%a' has converged" Stack.pp stack;
          Deferred.Or_error.return ())

let match_spec_and_service
    :  Stack.t -> Lib.Composefile.service_spec list -> (Service.t * Image.t) list ->
    (Service.t * Image.t * Image.t) list Or_error.t
  =
 fun stack specs service_images ->
  let open Or_error.Let_syntax in
  let%bind matched =
    List.fold_result specs ~init:[] ~f:(fun acc {Lib.Composefile.name; image} ->
        match
          List.Assoc.find service_images ~equal:(Service.equal_basename stack) name
        with
        | None ->
            Or_error.errorf
              "Failed to find spec '%a' in deployed services"
              Service.pp
              name
        | Some deployed_image -> Or_error.return @@ ((name, image, deployed_image) :: acc))
  in
  let spec_count = List.length specs in
  let service_count = List.length service_images in
  match spec_count = service_count with
  | true -> Or_error.return matched
  | false ->
      Or_error.errorf
        "Amount of services deployed (%d) does not match services specified (%d)"
        service_count
        spec_count

let is_insecure_registry image insecure_registries =
  List.mem ~equal:String.equal insecure_registries (Image.registry_full image)

let find_registry_access_token list image =
  List.Assoc.find ~equal:String.equal list (Image.registry_full image)

let verify ~registry_access_tokens ~insecure_registries ~verbose ~ssl_config host port
    stack composefile
  =
  let open Deferred.Or_error.Let_syntax in
  set_verbose verbose;
  let swarm = Swarm.of_host_and_port_and_ssl_config (host, port, ssl_config) in
  let%bind env = Deferred.return @@ environment () in
  match Lib.Composefile.load composefile env with
  | Error _ as e -> Deferred.return e
  | Ok specs ->
      let%bind deployed_service_images = Lib.Requests.service_images swarm stack in
      let%bind matched_spec =
        Deferred.return @@ match_spec_and_service stack specs deployed_service_images
      in
      let%bind () =
        Deferred.Result.all_unit
        @@ List.map matched_spec ~f:(fun (name, desired, deployed) ->
               match Image.equal_nametag desired deployed with
               | true -> Deferred.Or_error.return ()
               | false -> (
                   let%bind deployed_hash =
                     Lib.Requests.image_digest
                       ~image:deployed
                       ~registry_access_token:
                         (find_registry_access_token registry_access_tokens deployed)
                       ~is_insecure_registry:
                         (is_insecure_registry deployed insecure_registries)
                   in
                   let%bind desired_hash =
                     Lib.Requests.image_digest
                       ~image:desired
                       ~registry_access_token:
                         (find_registry_access_token registry_access_tokens desired)
                       ~is_insecure_registry:
                         (is_insecure_registry desired insecure_registries)
                   in
                   match String.equal deployed_hash desired_hash with
                   | true -> Deferred.Or_error.return ()
                   | false ->
                       Deferred.Or_error.errorf
                         "Service '%a' expected '%a' but '%a' was deployed"
                         Service.pp
                         name
                         Image.pp
                         desired
                         Image.pp
                         deployed))
      in
      return @@ Log.Global.info "Swarm state and composefile match"

let construct_ssl_config cert cacert key =
  match cert, cacert, key with
  | Some crt_file, Some ca_file, Some key_file ->
      Some (SSLConfig.create ~crt_file ~ca_file ~key_file ())
  | _ -> None

let verify_ssl_config ssl_config =
  match ssl_config with
  | None, None, None -> None
  | _ -> Some (Deferred.Or_error.error_string "All the flags 'cert', 'cacert' and 'key' must be specified.")

let () =
  let stack_name = Command.Spec.Arg_type.create Stack.of_string in
  let span_ms =
    Command.Spec.Arg_type.create (Fn.compose Time.Span.of_ms float_of_string)
  in
  let converge =
    Command.async_or_error
      ~summary:"Wait for convergence of stack deployment on Docker Swarm"
      (let open Command.Let_syntax in
      [%map_open
        let host = flag "--host" (required string) ~doc:" Hostname to connect to"
        and port =
          flag "--port" (optional_with_default 2375 int) ~doc:" Port to connect to"
        and cert = cert_flag
        and ca_cert = ca_cert_flag
        and key =  key_flag
        and verbose = flag "--verbose" no_arg ~doc:" Display more status information"
        and stack = anon ("stack-name" %: stack_name)
        and timeout =
          flag
            "--timeout"
            (optional_with_default 600. float)
            ~doc:" Maximum time to wait for convergence"
        and poll =
          flag
            "--poll-interval"
            (optional_with_default (Time.Span.of_ms 500.) span_ms)
            ~doc:" Maximum time to wait for convergence"
        in
        fun () ->
        match verify_ssl_config (cert, ca_cert, key) with
        | Some error ->  error
        | None ->
          converge ~verbose ~ssl_config:(construct_ssl_config cert ca_cert key) host port stack timeout poll])
        
  in
  let verify =
    Command.async_or_error
      ~summary:
        "Compare deployment status of services in Docker Swarm with docker-compose.yml \
         definitions"
      (let open Command.Let_syntax in
      [%map_open
        let host = flag "--host" (required string) ~doc:" Hostname to connect to"
        and port =
          flag "--port" (optional_with_default 2375 int) ~doc:" Port to connect to"
        and cert = cert_flag
        and ca_cert = ca_cert_flag
        and key =  key_flag
        and verbose = flag "--verbose" no_arg ~doc:" Display more status information"
        and stack = anon ("stack-name" %: stack_name)
        and registry_access_tokens =
          flag
            "--registry-access"
            (listed registry_access_value)
            ~doc:
              " A list of registries and their access token (e.g. \
               localhost:5000=myaccesstoken)"
        and insecure_registries =
          flag
            "--insecure-registries"
            (listed string)
            ~doc:" List of insecure registries (separated by comma)"
        and composefile =
          flag
            "--compose-file"
            (optional_with_default "docker-compose.yml" string)
            ~doc:" Compose file to read (default: docker-compose.yml)"
        in
        fun () ->
        match verify_ssl_config (cert, ca_cert, key) with
        | Some error -> error
        | None ->
            verify
              ~insecure_registries
              ~registry_access_tokens
              ~ssl_config:(construct_ssl_config cert ca_cert key)
              ~verbose
              host
              port
              stack
              composefile])
  in
  Command.group
    ~summary:"Deployment helper for Docker Stack"
    ["converge", converge; "verify", verify]
  |> Command.run
