(** Pure terminal command parsing and display. *)

type command =
  | Action of Game.action
  | Status
  | Help
  | Quit

val parse : string -> (command, string) result
(** [parse line] returns the command written in [line], ignoring case and
    surrounding spaces, or a message describing an invalid command. *)

val help : string
(** [help] lists all commands and their arguments. *)

val render_state : Game.t -> string
(** [render_state game] returns the current location, routes, resources,
    inventory, items, statistics, and time as terminal text. *)

val render_success : Game.action -> string
(** [render_success action] returns a message for a completed [action]. *)

val render_error : Game.error -> string
(** [render_error error] returns an explanation of a failed action. *)

val render_summary : Game.t -> string
(** [render_summary game] returns the final days and hours survived. *)
