(* simple and stupid debounce implementation *)
open Core

type t = {
  mutable last_triggered : Time.t;
  cooldown : Time.Span.t
}

let init ?(cooldown_ms = 2000.) () =
  {last_triggered = Time.epoch; cooldown = Time.Span.of_ms cooldown_ms}

let trigger v f =
  let now = Time.now () in
  let since_last_triggered = Time.diff now v.last_triggered in
  match Time.Span.(since_last_triggered > v.cooldown) with
  | false -> ()
  | true ->
      v.last_triggered <- now;
      f ()
