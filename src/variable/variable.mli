open Core

type context = string String.Map.t

val substitute : context -> string -> string Or_error.t
