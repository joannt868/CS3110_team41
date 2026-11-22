(** Resources available on the island. *)
type t =
  | Stone
  | Fresh_water
  | Wood
  | Berries

val name : t -> string
(** [name resource] is the name displayed for [resource]. *)
