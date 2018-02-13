open Core

(* TODO:
  * Parsing the images line of YAML
  * Implement the templating in Docker Compose files
  *)

type service_spec = {
  name: string;
  image: string;
  (* TODO: possibly other interesting values *)
}

let load_file = Yaml.yaml_of_string

let substitute_templates = Fn.id
